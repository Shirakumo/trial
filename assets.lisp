#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

;; FIXME: How do we access extra information that is not the straight-up content?
;;        stuff like dimensions of a texture or mesh material or whatever?
;; FIXME: A better system to distinguish which assets are actually different
;;        if initialisation arguments are involved and some-such.

(defvar *assets* (make-hash-table :test 'equal))
(defvar *root* (asdf:system-source-directory :trial))

(defun resource-pathname (pathname)
  (let ((pathname (pathname pathname)))
    (pathname-utils:normalize-pathname
     (merge-pathnames
      (if (pathname-utils:absolute-p pathname) pathname (merge-pathnames "data/" pathname))
      *root*))))

(defun clear-assets ()
  (loop for k being the hash-keys of *assets*
        do (remove-asset k)))

(defclass asset ()
  ((id :initarg :id :accessor id)
   (state :initarg :state :accessor state)
   (data :initarg :data :accessor data))
  (:default-initargs
   :id (error "ID required.")
   :state :offloaded))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T)
    (format stream "~a ~s" (id asset) (state asset))))

(defmethod initialize-instance :after ((asset asset) &key)
  (setf (gethash (id asset) *assets*) asset))

(defmethod asset (id type &rest args)
  (apply #'make-instance type :id id args))

(defmethod asset ((asset asset) type &rest args)
  (declare (ignore args))
  asset)

(defmethod asset :around (id type &rest args)
  (declare (ignore args))
  (or (gethash id *assets*)
      (call-next-method)))

(defmethod remove-asset (id)
  (let ((asset (gethash id *assets*)))
    (when asset
      (offload asset)
      (remhash id *assets*))))

(defmethod content :before ((asset asset) &optional offset)
  (declare (ignore offset))
  (restore asset))

(defmethod content ((asset asset) &optional offset)
  (etypecase offset
    (null (data asset))
    (integer (elt (data asset) offset))))

(defmethod restore ((asset asset))
  (error "Don't know how to restore ~a" asset))

(defmethod restore :around ((asset asset))
  (unless (eql (state asset) :restored)
    (call-next-method))
  asset)

(defmethod restore :before ((asset asset))
  (v:info :trial.assets "Restoring ~a" asset))

(defmethod restore :after ((asset asset))
  (setf (state asset) :restored))

(defmethod offload ((asset asset))
  (finalize asset))

(defmethod offload :around ((asset asset))
  (unless (eql (state asset) :offloaded)
    (call-next-method))
  asset)

(defmethod offload :before ((asset asset))
  (v:info :trial.assets "Offloading ~a" asset))

(defmethod offload :after ((asset asset))
  (setf (state asset) :offloaded))

(defmethod finalize ((asset asset))
  (finalize (data asset))
  (setf (data asset) NIL))

(defclass named-asset (asset)
  ((id :initarg :id :accessor id :accessor name)))

(defclass font (named-asset)
  ((size :initarg :size :accessor size))
  (:default-initargs
   :size 12))

(defmethod restore ((asset font))
  (setf (data asset) (q+:make-qfont (name asset) (size asset))))

(defclass file-asset (asset)
  ((id :initarg :id :accessor file :accessor id)
   (allowed-types :initarg :allowed-types :accessor allowed-types)))

(defmethod initialize-instance :before ((asset file-asset) &key id allowed-types)
  (unless (or (eql allowed-types T)
              (find (pathname-type id) allowed-types :test #'string-equal))
    (error "~a does not know how to handle a file of type ~a."
           asset (pathname-type id))))

(defmethod asset :around ((pathname pathname) type &rest args)
  (apply #'call-next-method (resource-pathname pathname) type args))

(defmethod asset :around ((string string) (type symbol) &rest args)
  (if (subtypep type 'file-asset)
      (apply #'asset (uiop:parse-native-namestring string) type args)
      (call-next-method)))

(defmethod remove-asset ((pathname pathname))
  (call-next-method (resource-pathname pathname)))

(defmethod restore :before ((asset file-asset))
  (unless (probe-file (file asset))
    (error "Invalid file path ~s." (file asset))))

(defclass image (file-asset)
  ()
  (:default-initargs
   :allowed-types '(bmp gif jpg jpeg png pbm pgm ppm tiff xbm xpm)))

(defmethod restore ((asset image))
  (let ((image (q+:make-qimage (uiop:native-namestring (file asset)))))
    (when (q+:is-null image)
      (error "Invalid file ~s." (file asset)))
    (setf (data asset) image)))

(defclass texture (image)
  ((target :initarg :target :reader target)
   (filter :initarg :filter :reader filter)
   (wrapping :initarg :wrapping :reader wrapping))
  (:default-initargs
   :target :texture-2d
   :filter :linear
   :wrapping :clamp-to-edge))

(defmethod initialize-instance :before ((texture texture) &key target)
  (check-gl-texture-target target))

(defmethod restore ((asset texture))
  (call-next-method)
  (with-slots (target filter wrapping) asset
    (let* ((image (data asset))
           (buffer (q+:qglwidget-convert-to-glformat image))
           (texture (gl:gen-texture)))
      (check-gl-texture-size (q+:width buffer) (q+:height buffer))
      (gl:bind-texture target texture)
      (ecase target
        (:texture-2d
         (gl:tex-image-2d target 0 :rgba (q+:width buffer) (q+:height buffer) 0 :rgba :unsigned-byte (q+:bits buffer)))
        (:texture-cube-map
         (loop with width = (q+:width buffer)
               with height = (/ (q+:height buffer) 6)
               for target in '(:texture-cube-map-positive-x :texture-cube-map-negative-x
                               :texture-cube-map-positive-y :texture-cube-map-negative-y
                               :texture-cube-map-positive-z :texture-cube-map-negative-z)
               for index from 0
               do (gl:tex-image-2d target 0 :rgba width height 0 :rgba :unsigned-byte
                                   (cffi:inc-pointer (q+:bits buffer) (* width height index 4))))))
      (gl:tex-parameter target :texture-min-filter filter)
      (gl:tex-parameter target :texture-mag-filter filter)
      (gl:tex-parameter target :texture-wrap-s wrapping)
      (gl:tex-parameter target :texture-wrap-t wrapping)
      (unless (eql target :texture-2d)
        (gl:tex-parameter target :texture-wrap-r wrapping))
      (gl:bind-texture target 0)
      (setf (data asset) texture)
      (finalize image)
      (finalize buffer))))

(defmethod content ((asset texture) &optional offset)
  (declare (ignore offset))
  (data asset))

(defmethod finalize ((asset texture))
  (gl:delete-textures (list (data asset))))

(defclass sound (file-asset)
  ()
  (:default-initargs
   :allowed-types '(wav ogg mp3)))

(defclass model (file-asset)
  ()
  (:default-initargs
   :allowed-types '(obj)))

(defmethod restore ((asset model))
  (setf (data asset) (wavefront-loader:load-obj (file asset)))
  (loop for obj across (data asset)
        for diffuse = (wavefront-loader:diffuse-map (wavefront-loader:material obj))
        do (when (typep diffuse 'pathname)
             (let ((texture (asset diffuse 'texture)))
               (setf (wavefront-loader:diffuse-map (wavefront-loader:material obj))
                     (content texture))))))

(defclass shader (file-asset)
  ((shader-type :initarg :shader-type :reader shader-type))
  (:default-initargs
   :shader-type NIL
   :allowed-types '(glsl vert tesc tese geom frag comp vs fs cs gs tcs tes)))

(defun pathname->shader-type (pathname)
  (or (cdr (assoc (pathname-type pathname)
                  `((glsl . :vertex-shader)
                    (tesc . :tess-control-shader)
                    (tese . :tess-evaluation-shader)
                    (vert . :vertex-shader)
                    (geom . :geometry-shader)
                    (frag . :fragment-shader)
                    (comp . :compute-shader)
                    (tcs . :tess-control-shader)
                    (tes . :tess-evaluation-shader)
                    (vs . :vertex-shader)
                    (gs . :geometry-shader)
                    (fs . :fragment-shader)
                    (cs . :compute-shader)) :test #'string-equal))
      (error "Don't know how to convert ~s to shader type." pathname)))

(defmethod initialize-instance :before ((asset shader) &key shader-type)
  (when shader-type (check-gl-shader-type shader-type)))

(defmethod initialize-instance :after ((asset shader) &key)
  (unless (shader-type asset)
    (setf (slot-value asset 'shader-type) (pathname->shader-type (file asset)))))

(defmethod restore ((asset shader))
  (let ((shader (gl:create-shader (shader-type asset))))
    (gl:shader-source shader (alexandria:read-file-into-string (file asset)))
    (gl:compile-shader shader)
    (unless (gl:get-shader shader :compile-status)
      (error "Failed to compile ~a: ~%~a" asset (gl:get-shader-info-log shader)))
    (setf (data asset) shader)))

(defmethod finalize ((asset shader))
  (gl:delete-shader (data asset)))

(defclass shader-program (named-asset)
  ((shaders :initarg :shaders :accessor shaders)))

(defmethod restore ((asset shader-program))
  (let ((program (gl:create-program)))
    (dolist (shader (shaders asset))
      (gl:attach-shader program (content (asset shader 'shader))))
    (gl:link-program program)
    (dolist (shader (shaders asset))
      (gl:detach-shader program (content (asset shader 'shader))))
    (unless (gl:get-program program :link-status)
      (error "Failed to link ~a: ~%~a" asset (gl:get-program-info-log program)))
    (setf (data asset) program)))

(defmethod finalize ((asset shader-program))
  (gl:delete-program (data asset)))

(defclass gl-buffer (named-asset)
  ((buffer-type :initarg :buffer-type :accessor buffer-type)
   (element-type :initarg :element-type :accessor element-type)
   (buffer-data :initarg :buffer-data :accessor buffer-data)
   (data-usage :initarg :data-usage :accessor data-usage))
  (:default-initargs
   :buffer-type :array-buffer
   :element-type :float
   :data-usage :static-draw))

(defmethod initialize-instance :before ((asset gl-buffer) &key buffer-type element-type data-usage)
  ;; FIXME: automatically determine element-type from buffer-data if not specified
  (check-gl-buffer-type buffer-type)
  (check-gl-array-element-type element-type)
  (check-gl-buffer-data-usage data-usage))

(defmethod restore ((asset gl-buffer))
  (with-slots (element-type buffer-data buffer-type data-usage) asset
    (let ((buffer (gl:gen-buffer))
          (array (gl:alloc-gl-array element-type (length buffer-data))))
      (gl:bind-buffer buffer-type buffer)
      (loop for i from 0
            for el across buffer-data
            do (setf (gl:glaref array i) el))
      (gl:buffer-data buffer-type data-usage array)
      (gl:free-gl-array array)
      (gl:bind-buffer buffer-type 0)
      (setf (data asset) buffer))))

(defmethod finalize ((asset gl-buffer))
  (gl:delete-buffers (list (data asset))))

(defmethod (setf buffer-data) :after (data (asset gl-buffer))
  ;; Recreate
  (let ((old (data asset)))
    (restore asset)
    (gl:delete-buffers (list old))))

(defclass vertex-array (named-asset)
  ())

(defmethod restore ((asset vertex-array))
  (let ((vao (gl:gen-vertex-array)))
    (gl:bind-vertex-array vao)
    ;; FIXME: actually figure out how to generalise this mess
    ;;        and implement it properly.
    ))

(defmethod finalize ((asset vertex-array))
  (gl:delete-vertex-arrays (list (data asset))))
