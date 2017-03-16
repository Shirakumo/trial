#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;; FIXME: configurable defaults

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defgeneric load (object)
  (:method-combination progn :most-specific-last))

(defmethod load progn (object)
  (v:info :trial "Loading ~a" object))

(defgeneric offload (object)
  (:method-combination progn :most-specific-first))

(defmethod load progn (object)
  (v:info :trial "Offloading ~a" object))

(defclass asset ()
  ((inputs :initarg :inputs :accessor inputs)
   (resource :initform NIL :accessor resource))
  (:default-initargs :inputs ()))

(defmethod initialize-instance :around ((asset asset) &rest args &key input inputs)
  (when input (setf (getf args :inputs) (list* input inputs)))
  (apply #'call-next-method asset args))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T :identity T)))

(defmethod finalize-resource ((asset asset) resource)
  (finalize-resource (type-of asset) resource))

(defmethod finalize-resource :before ((type symbol) resource)
  (v:debug :trial.asset "Finalising resource ~a of type ~a"
           resource type))

(defmethod install-finalizer ((asset asset))
  (let ((type (type-of asset))
        (resource (resource asset)))
    (tg:finalize asset (lambda () (finalize-resource type resource)))))

(defmethod (setf resource) :after (value (asset asset))
  (when value
    (install-finalizer asset)))

(defmethod coerce-input ((asset asset) input)
  (error "Incompatible input type ~s for asset of type ~s."
         (type-of input) (type-of asset)))

(defmethod coerced-inputs ((asset asset))
  (loop for input in (inputs asset)
        collect (coerce-input asset input)))

(defmethod load :around ((asset asset))
  (unless (resource asset)
    (call-next-method))
  asset)

(defmethod offload :around ((asset asset))
  (when (resource asset)
    (tg:cancel-finalization asset)
    (call-next-method))
  asset)

(defmethod offload ((asset asset))
  (finalize-resource asset (resource asset)))

(defmethod finalize :after ((asset asset))
  (offload asset))

(defun make-asset (type inputs &rest initargs)
  (apply #'make-instance type :inputs (enlist inputs) initargs))

(defmacro with-assets (asset-specs &body body)
  (if asset-specs
      (destructuring-bind (variable . initform) (first asset-specs)
        `(let ((,variable (make-asset ,@initform)))
           (load ,variable)
           (unwind-protect
                (with-assets ,(rest asset-specs)
                  ,@body)
             (offload ,variable))))
      `(progn ,@body)))

(defvar *asset-cache* (tg:make-weak-hash-table :weakness :key :test 'eq))

(defun clear-asset-cache ()
  (loop for table being the hash-values of *asset-cache*
        do (loop for asset being the hash-values of table
                 do (offload asset)))
  (clrhash *asset-cache*))

(defmacro with-assets* (asset-specs &body body)
  (let ((index (gensym "INDEX"))
        (table (gensym "TABLE")))
    `(let* ((,table (or (gethash ',index *asset-cache*)
                        (setf (gethash ',index *asset-cache*)
                              (make-hash-table :test 'eq))))
            ,@(loop for (variable . initform) in asset-specs
                    collect `(,variable (or (gethash ',variable ,table)
                                            (setf (gethash ',variable ,table)
                                                  (load (make-asset ,@initform)))))))
       ,@body)))

(defclass shader-asset (asset)
  ((shader-type :initarg :type :accessor shader-type)))

(defmethod initialize-instance :before ((asset shader-asset) &key type)
  (check-shader-type type))

(defmethod coerce-input ((asset shader-asset) (file pathname))
  (alexandria:read-file-into-string file))

(defmethod coerce-input ((asset shader-asset) (source string))
  source)

(defmethod finalize-resource ((type (eql 'shader-asset)) resource)
  (gl:delete-shader resource))

(defmethod load progn ((asset shader-asset))
  (let ((source (with-output-to-string (output)
                  (dolist (source (coerced-inputs asset))
                    (write-sequence source output))))
        (shader (gl:create-shader (shader-type asset))))
    (setf (resource asset) shader)
    (with-cleanup-on-failure (offload asset)
      (with-new-value-restart (source input-source) (use-source "Supply new source code directly.")
        (gl:shader-source shader source)
        (gl:compile-shader shader)
        (unless (gl:get-shader shader :compile-status)
          (error "Failed to compile ~a: ~%~a~%Shader source:~%~a"
                 asset (gl:get-shader-info-log shader)
                 source))))))

(defclass shader-program-asset (asset)
  ())

(defmethod coerce-input ((asset shader-program-asset) (shader shader-asset))
  shader)

(defun check-shader-compatibility (shaders)
  (loop with table = (make-hash-table :test 'eql)
        for shader in shaders
        do (if (gethash (shader-type shader) table)
               (error "Cannot compile two shaders of the same type into a single program~%  ~a~%  ~a"
                      (gethash (shader-type shader) table) shader)
               (setf (gethash (shader-type shader) table) shader))
        finally (return shaders)))

(defmethod finalize-resource ((type (eql 'shader-program-asset)) resource)
  (gl:delete-program resource))

(defmethod load progn ((asset shader-program-asset))
  (let ((shaders (coerced-inputs asset)))
    (check-shader-compatibility shaders)
    (let ((program (gl:create-program)))
      (setf (resource asset) program)
      (with-cleanup-on-failure (offload asset)
        (dolist (shader shaders)
          (gl:attach-shader program (resource shader)))
        (gl:link-program program)
        (dolist (shader shaders)
          (gl:detach-shader program (resource shader)))
        (unless (gl:get-program program :link-status)
          (error "Failed to link ~a: ~%~a"
                 asset (gl:get-program-info-log program)))))))

(defmethod uniform-location ((asset shader-program-asset) (name string))
  (gl:get-uniform-location (resource asset) name))

(defmethod uniform-location ((asset shader-program-asset) (name symbol))
  (uniform-location asset (symbol->c-name name)))

(defmethod (setf uniform) (data (asset shader-program-asset) (name symbol))
  (setf (uniform asset (symbol->c-name name)) data))

(defmethod (setf uniform) (data (asset shader-program-asset) (name string))
  (let ((location (uniform-location asset name)))
    (etypecase data
      (fixnum (gl:uniformi location data))
      (single-float (gl:uniformf location data))
      (vec2 (gl:uniformf location (vx data) (vy data)))
      (vec3 (gl:uniformf location (vx data) (vy data) (vz data)))
      (vec4 (gl:uniformf location (vx data) (vy data) (vz data) (vw data)))
      (mat2 (gl:uniform-matrix-2fv location (marr2 data)))
      (mat3 (gl:uniform-matrix-3fv location (marr3 data)))
      (mat4 (gl:uniform-matrix-4fv location (marr4 data)))
      (matn (ecase (mrows data)
              (2 (ecase (mcols data)
                   (3 (gl:uniform-matrix-2x3-fv location (marrn data)))
                   (4 (gl:uniform-matrix-2x4-fv location (marrn data)))))
              (3 (ecase (mcols data)
                   (2 (gl:uniform-matrix-3x2-fv location (marrn data)))
                   (4 (gl:uniform-matrix-3x4-fv location (marrn data)))))
              (4 (ecase (mcols data)
                   (2 (gl:uniform-matrix-4x2-fv location (marrn data)))
                   (3 (gl:uniform-matrix-4x3-fv location (marrn data))))))))))

(defclass vertex-buffer-asset (asset)
  ((buffer-type :initarg :type :accessor buffer-type)
   (element-type :initarg :element-type :accessor element-type)
   (data-usage :initarg :data-usage :accessor data-usage)
   (size :initarg :size :accessor size))
  (:default-initargs
   :type :array-buffer
   :element-type :float
   :data-usage :static-draw))

(defmethod initialize-instance :before ((asset vertex-buffer-asset) &key type element-type data-usage)
  (check-vertex-buffer-type type)
  (check-vertex-buffer-element-type element-type)
  (check-vertex-buffer-data-usage data-usage))

(defmethod coerce-input ((asset vertex-buffer-asset) (vector vector))
  vector)

(defmethod coerce-input ((asset vertex-buffer-asset) (list list))
  (coerce list 'vector))

(defmethod coerce-input ((asset vertex-buffer-asset) (number real))
  (make-array 1 :initial-element (float number)))

(defmethod finalize-resource ((type (eql 'vertex-buffer-asset)) resource)
  (gl:delete-buffers (list resource)))

(defun ensure-single-vector (inputs)
  (if (cdr inputs)
      (let ((output (make-array 0 :adjustable T :fill-pointer T)))
        (dolist (input inputs output)
          (loop for v across input do (vector-push-extend v output))))
      (first inputs)))

(defmethod load progn ((asset vertex-buffer-asset))
  (let ((buffer-data (ensure-single-vector (coerced-inputs asset))))
    (with-slots (element-type buffer-type data-usage) asset
      (let ((buffer (gl:gen-buffer)))
        (setf (resource asset) buffer)
        (with-cleanup-on-failure (offload asset)
          (let ((array (gl:alloc-gl-array element-type (length buffer-data))))
            (unwind-protect
                 (loop initially (gl:bind-buffer buffer-type buffer)
                       for i from 0
                       for el across buffer-data
                       do (setf (gl:glaref array i) (gl-coerce el element-type))
                       finally (gl:buffer-data buffer-type data-usage array))
              (gl:free-gl-array array)
              (gl:bind-buffer buffer-type 0))
            (setf (size asset) (length buffer-data))))))))

(defclass vertex-array-asset (asset)
  ((size :initarg :size :initform NIL :accessor size)))

(defmethod coerce-input ((asset vertex-array-asset) (buffer vertex-buffer-asset))
  (list buffer))

(defmethod coerce-input ((asset vertex-array-asset) (spec list))
  spec)

(defmethod finalize-resource ((type (eql 'vertex-array-asset)) resource)
  (gl:delete-vertex-arrays (list resource)))

(defmethod load progn ((asset vertex-array-asset))
  (let ((array (gl:gen-vertex-array)))
    (setf (resource asset) array)
    (with-cleanup-on-failure (offload asset)
      (gl:bind-vertex-array array)
      (unwind-protect
           (loop for buffer in (coerced-inputs asset)
                 for i from 0
                 do (destructuring-bind (buffer/s &key (index i)
                                                       (size 3)
                                                       (normalized NIL)
                                                       (stride 0)
                                                       (offset 0))
                        buffer
                      (let ((buffers (enlist buffer/s)))
                        (dolist (buffer buffers)
                          (load buffer)
                          (when (and (not (size asset))
                                     (eql :element-array-buffer (buffer-type buffer)))
                            (setf (size asset) (size buffer)))
                          (gl:bind-buffer (buffer-type buffer) (resource buffer)))
                        (gl:vertex-attrib-pointer index size (element-type (first buffers)) normalized stride offset)
                        (gl:enable-vertex-attrib-array index))))
        (gl:bind-vertex-array 0)))))

;; FIXME: delete vbos after vao is loaded and unbound
(defun pack-vao (element &rest specs)
  (let ((buffer (make-array 0 :adjustable T :fill-pointer 0))
        (groups)
        (inputs ())
        (offset 0))
    (loop for index from 0
          for (size input) on specs by #'cddr
          do ;; Ensure that each array matches with all others in groups.
             (cond ((/= 0 (mod (length input)size))
                    (error "The input array~%  ~s~% with size ~a cannot be divided into even groups."
                           input size))
                   ((not groups)
                    (setf groups (/ (length input) size)))
                   ((/= (/ (length input) size) groups)
                    (error "The input array~%  ~s~% with size ~a does not match the number of groups ~a."
                           input size groups)))
             ;; Accumulate into general buffer.
             (for:for ((el over input))
               (vector-push-extend el buffer))
             (push (list :index index :size size :offset offset) inputs)
             (incf offset (* size 4)))
    ;; Construct actual assets.
    (let ((buffer (make-asset 'vertex-buffer-asset (list buffer)
                              :element-type :float))
          (element (make-asset 'vertex-buffer-asset (list element)
                               :type :element-array-buffer
                               :element-type :uint)))
      (make-asset 'vertex-array-asset
                  (print (loop for spec in (nreverse inputs)
                               collect (list* (list buffer element) :stride offset spec)))))))

(defclass texture-asset (asset)
  ((target :initarg :target :accessor target)
   (mag-filter :initarg :mag-filter :accessor mag-filter)
   (min-filter :initarg :min-filter :accessor min-filter)
   (anisotropy :initarg :anisotropy :accessor anisotropy)
   (wrapping :initarg :wrapping :reader wrapping))
  (:default-initargs
   :target :texture-2d
   :mag-filter :linear
   :min-filter :linear
   :anisotropy NIL
   :wrapping :clamp-to-edge))

(defmethod initialize-instance :around ((asset texture-asset) &rest args)
  (setf (getf args :wrapping) (enlist (getf args :wrapping) (getf args :wrapping) (getf args :wrapping)))
  (apply #'call-next-method asset args))

(defmethod initialize-instance :before ((asset texture-asset) &key target mag-filter min-filter wrapping)
  (check-texture-target target)
  (check-texture-mag-filter mag-filter)
  (check-texture-min-filter min-filter)
  (check-texture-wrapping (first wrapping))
  (check-texture-wrapping (second wrapping))
  (check-texture-wrapping (third wrapping)))

(defmethod coerce-input ((asset texture-asset) (file pathname))
  (with-finalizing ((image (q+:make-qimage (uiop:native-namestring file))))
    (when (q+:is-null image)
      (error "Qt failed to load image ~s" file))
    (coerce-input asset image)))

(defmethod coerce-input ((asset texture-asset) (object qobject))
  (q+:qglwidget-convert-to-glformat object))

(defmethod finalize-resource ((type (eql 'texture-asset)) resource)
  (gl:delete-textures (list resource)))

(defun object-to-texparams (object)
  (etypecase object
    (qobject (list (q+:width object)
                   (q+:height object)
                   (q+:bits object)
                   :rgba))
    (cons    (destructuring-bind (width &optional (height 1) (bits 0) (format :rgba)) object
               (list width height bits format)))))

(defun images-to-textures (target images)
  (case target
    ;; FIXME: Array textures
    (:texture-cube-map
     (loop for image in images
           for target in '(:texture-cube-map-positive-x :texture-cube-map-negative-x
                           :texture-cube-map-positive-y :texture-cube-map-negative-y
                           :texture-cube-map-positive-z :texture-cube-map-negative-z)
           do (images-to-textures target (list image))))
    (T
     (when (cdr images) (error "Only one input is supported for the ~a target." target))
     (loop for level from 0
           for image in images
           do (case target
                (:texture-1d
                 (destructuring-bind (width height bits format) (object-to-texparams image)
                   (gl:tex-image-1d target level format (* width height) 0 format :unsigned-byte bits)))
                ((:texture-cube-map-positive-x :texture-cube-map-negative-x
                  :texture-cube-map-positive-y :texture-cube-map-negative-y
                  :texture-cube-map-positive-z :texture-cube-map-negative-z
                  :texture-2d)
                 (destructuring-bind (width height bits format) (object-to-texparams image)
                   (gl:tex-image-2d target level format width height 0 format :unsigned-byte bits)))
                (:texture-3d
                 (destructuring-bind (width height depth bits format) image
                   (gl:tex-image-3d target level format width height depth 0 format :unsigned-byte bits))))))))

(defmethod load progn ((asset texture-asset))
  (with-slots (target mag-filter min-filter anisotropy wrapping) asset
    (let ((images (coerced-inputs asset)))
      (unwind-protect
           (let ((texture (setf (resource asset) (gl:gen-texture))))
             (with-cleanup-on-failure (offload asset)
               (gl:bind-texture target texture)
               (images-to-textures target images)
               (unless (or (eql min-filter :nearest) (eql min-filter :linear))
                 (gl:generate-mipmap target))
               (gl:tex-parameter target :texture-min-filter min-filter)
               (gl:tex-parameter target :texture-mag-filter mag-filter)
               (gl:tex-parameter target :texture-wrap-s (first wrapping))
               (gl:tex-parameter target :texture-wrap-t (second wrapping))
               (when (eql target :texture-cube-map)
                 (gl:tex-parameter target :texture-wrap-r (third wrapping)))
               (when anisotropy
                 (gl:tex-parameter target :texture-max-anisotropy-ext anisotropy))))
        (mapc #'finalize images)))))

(defclass framebuffer-asset (asset)
  ())

(defmethod coerce-input ((asset asset) (texture texture-asset))
  (list texture :attachment :color-attachment0))

(defmethod coerce-input ((asset asset) (spec list))
  spec)

(defmethod finalize-resource ((type (eql 'framebuffer-asset)) resource)
  (gl:delete-framebuffers (list resource)))

(defmethod load progn ((asset framebuffer-asset))
  (let ((buffer (setf (resource asset) (gl:gen-framebuffer))))
    (with-cleanup-on-failure (offload asset)
      (gl:bind-framebuffer :framebuffer buffer)
      (unwind-protect
           (dolist (input (coerced-inputs asset))
             (destructuring-bind (texture &key (level 0) layer attachment) input
               (check-framebuffer-attachment attachment)
               (check-type texture texture-asset)
               (if layer
                   (%gl:framebuffer-texture-layer :framebuffer attachment (resource texture) level layer)
                   (%gl:framebuffer-texture :framebuffer attachment (resource texture) level))
               (unless (eql :framebuffer-complete (gl:check-framebuffer-status :framebuffer))
                 (error "Failed to attach ~a as ~s to ~a"
                        texture attachment asset))))
        (gl:bind-framebuffer :framebuffer 0)))))

(defclass framebuffer-bundle-asset (asset)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (framebuffer :initform NIL :accessor framebuffer)
   (textures :initform () :accessor textures))
  (:default-initargs
   :width (error "WIDTH required.")
   :height (error "HEIGHT required.")))

(defmethod finalize-resource ((type (eql 'famebuffer-bundle-asset)) resource)
  (mapcar #'offload resource))

(defmethod offload progn ((asset framebuffer-bundle-asset))
  (mapcar #'offload (list* (framebuffer asset) (textures asset)))
  (setf (framebuffer asset) NIL)
  (setf (textures asset) ())
  (setf (resource asset) NIL))

(defmethod install-finalizer ((asset framebuffer-bundle-asset))
  (let ((type (type-of asset))
        (resource (list* (framebuffer asset) (textures asset))))
    (tg:finalize asset (lambda () (finalize-resource type resource)))))

(defmethod coerce-input ((asset asset) (attachment symbol))
  (coerce-input asset (list :attachment attachment)))

(defmethod coerce-input ((asset asset) (spec cons))
  (let ((attachment (getf spec :attachment)))
    (check-framebuffer-attachment attachment)
    (list* (make-instance 'texture-asset :input (list (width asset) (height asset)
                                                      (getf spec :bits 0)
                                                      (case attachment
                                                        (:depth-attachment :depth-component)
                                                        (:depth-stencil-attachment :depth-stencil)
                                                        (T :rgba)))
                                         :min-filter (getf spec :min-filter :nearest)
                                         :mag-filter (getf spec :mag-filter :nearest)
                                         :wrapping (getf spec :wrapping))
           (loop for (k v) on spec by #'cddr
                 for test = (find k '(:min-filter :mag-filter :wrapping :bits))
                 unless test collect k
                 unless test collect v))))

(defmethod load progn ((asset framebuffer-bundle-asset))
  (let ((inputs (coerced-inputs asset)))
    (with-cleanup-on-failure (mapc #'offload (textures asset))
      (dolist (input inputs)
        (push (load (first input)) (textures asset)))
      (setf (framebuffer asset) buffer)
      (setf (resource asset) (resource buffer)))))

(defmethod resize ((asset framebuffer-bundle-asset) width height)
  (let ((loaded (resource asset)))
    (when loaded (offload asset))
    (setf (width asset) width)
    (setf (height asset) height)
    (when loaded (load asset))))
