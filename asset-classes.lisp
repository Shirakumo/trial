#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;; FIXME: configurable defaults

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass font (asset)
  ((size :initarg :size :accessor size)
   (family :initarg :family :accessor family))
  (:default-initargs
   :size 12
   :family (error "FAMILY required.")))

(defmethod (setf family) :after (family (asset font))
  (reload asset))

(defmethod (setf size) :after (size (asset font))
  (reload asset))

(defmethod load-data ((asset font))
  (q+:make-qfont (family asset) (size asset)))

(defmethod finalize-data ((asset font) data)
  (finalize data))

(defclass gl-asset (asset)
  ())

(defmethod reload ((asset gl-asset))
  (with-context (*main*)
    (call-next-method)))

(defmethod load-data :around ((asset gl-asset))
  (with-context (*main*)
    (call-next-method)))

(defmethod finalize-data :around ((asset gl-asset) data)
  (with-context (*main*)
    (call-next-method)))

(defclass file-asset (asset)
  ((file :initform NIL :accessor file))
  (:default-initargs
   :file (error "FILE required.")))

(defmethod shared-initialize :after ((asset file-asset) slot-names &key (file (file asset)))
  (setf (file asset) file)
  (unless (probe-file (file asset))
    (emit-compilation-note "Defining asset ~a on inexistent file: ~a"
                           asset (file asset))))

(defmethod (setf file) (thing (asset file-asset))
  (error "Cannot set ~s as file on ~a. Must be a pathname-designator."
         thing asset))

(defmethod (setf file) ((file string) (asset file-asset))
  (setf (file asset) (uiop:parse-native-namestring file)))

(defmethod (setf file) ((file pathname) (asset file-asset))
  (setf (slot-value asset 'file) (pathname-utils:normalize-pathname
                                  (merge-pathnames file (base (home asset))))))

(defmethod (setf file) :after ((file pathname) (asset file-asset))
  (reload asset))

(defmethod load-data :before ((asset file-asset))
  (unless (probe-file (file asset))
    (error "File for asset ~a not found on disk: ~a"
           asset (file asset))))

(defmethod load-data :around ((asset file-asset))
  (with-new-value-restart ((file asset)) (use-file "Enter a new file to use.")
    (call-next-method)))

(defclass image (file-asset)
  ())

(defmethod load-data ((asset image))
  (let ((image (q+:make-qimage (uiop:native-namestring (file asset)))))
    (when (q+:is-null image)
      (finalize image)
      (error "Qt failed to load image for ~a" asset))
    image))

(defmethod finalize-data ((asset image) data)
  (finalize data))

(defclass texture (image gl-asset)
  ((target :initarg :target :reader target)
   (mag-filter :initarg :mag-filter :reader mag-filter)
   (anisotropy :initarg :anisotropy :reader anisotropy)
   (wrapping :initarg :wrapping :reader wrapping))
  (:default-initargs
   :target :texture-2d
   :mag-filter :linear
   :min-filter :linear
   :anisotropy NIL
   :wrapping :clamp-to-edge))

(defmethod shared-initialize :before ((asset texture) slot-names &key (target (target asset))
                                                                      (mag-filter (mag-filter asset))
                                                                      (min-filter (min-filter asset)))
  (check-texture-target target)
  (check-texture-mag-filter mag-filter)
  (check-texture-min-filter min-filter))

(defun image-buffer-to-texture (buffer target)
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
                               (cffi:inc-pointer (q+:bits buffer) (* width height index 4)))))))

(defmethod load-data ((asset texture))
  (with-slots (target mag-filter min-filter anisotropy wrapping) asset
    (let ((image (call-next-method)))
      (check-texture-size (q+:width image) (q+:height image))
      (with-finalizing ((buffer (q+:qglwidget-convert-to-glformat image)))
        (finalize image)
        (let ((texture (gl:gen-texture)))
          (gl:bind-texture target texture)
          (with-cleanup-on-failure
              (finalize-data asset texture)
            (image-buffer-to-texture buffer target)
            (gl:tex-parameter target :texture-min-filter min-filter)
            (gl:tex-parameter target :texture-mag-filter mag-filter)
            (when anisotropy
              (gl:tex-parameter target :texture-max-anisotropy-ext anisotropy))
            (gl:tex-parameter target :texture-wrap-s wrapping)
            (gl:tex-parameter target :texture-wrap-t wrapping)
            (unless (eql target :texture-2d)
              (gl:tex-parameter target :texture-wrap-r wrapping))
            (gl:bind-texture target 0))
          texture)))))

(defmethod finalize-data ((asset texture) data)
  (gl:delete-textures (list data)))

(defclass model (file-asset)
  ())

(defmethod load-data ((asset model))
  (let ((data (wavefront-loader:load-obj (file asset))))
    (loop for obj across data
          for diffuse = (wavefront-loader:diffuse-map (wavefront-loader:material obj))
          do (when diffuse
               (setf (wavefront-loader:diffuse-map (wavefront-loader:material obj))
                     ;; FIXME: In general, this is suboptimal and will break horribly
                     ;;        if references to the asset are not maintained. How to fix?
                     (data (get-resource WHICH-POOL-?? 'texture WHICH-NAME-??)))))))

;; FIXME: allow specifying inline shaders
(defclass shader (file-asset gl-asset)
  ((shader-type :initarg :shader-type :reader shader-type))
  (:default-initargs
   :shader-type NIL))

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

(defmethod shared-initialize :before ((asset shader) slot-names &key shader-type)
  (when shader-type (check-shader-type shader-type)))

(defmethod shared-initialize :after ((asset shader) slot-names &key)
  (unless (shader-type asset)
    (setf (slot-value asset 'shader-type) (pathname->shader-type (file asset)))))

(defmethod load-data ((asset shader))
  (let ((source (alexandria:read-file-into-string (file asset)))
        (shader (gl:create-shader (shader-type asset))))
    (with-cleanup-on-failure
        (finalize-data asset shader)
      (with-new-value-restart (source input-source) (use-source "Supply new source code directly.")
        (gl:shader-source shader source)
        (gl:compile-shader shader)
        (unless (gl:get-shader shader :compile-status)
          (error "Failed to compile ~a: ~%~a" asset (gl:get-shader-info-log shader)))))
    shader))

(defmethod finalize-data ((asset shader) data)
  (gl:delete-shader data))

(defclass shader-program (gl-asset)
  ((shaders :initarg :shaders :accessor shaders)))

(defmethod (setf shaders) :after (shaders (asset shader-program))
  (reload asset))

(defmethod load-data ((asset shader-program))
  (let ((shaders (loop for (pool name) in (shaders asset)
                       collect (get-resource 'shader pool name)))
        (program (gl:create-program)))
    (with-cleanup-on-failure
        (finalize-data asset program)
      (mapc (lambda (shader) (gl:attach-shader program (data shader))) shaders)
      (gl:link-program program)
      (mapc (lambda (shader) (gl:detach-shader program (data shader))) shaders)
      (unless (gl:get-program program :link-status)
        (error "Failed to link ~a: ~%~a" asset (gl:get-program-info-log program))))
    program))

(defmethod finalize-data ((asset shader-program) data)
  (gl:delete-program data))

;; FIXME: allow loading from file or non-array type
(defclass vertex-buffer (gl-asset)
  ((buffer-type :initarg :buffer-type :accessor buffer-type)
   (element-type :initarg :element-type :accessor element-type)
   (buffer-data :initarg :buffer-data :accessor buffer-data)
   (data-usage :initarg :data-usage :accessor data-usage))
  (:default-initargs
   :buffer-type :array-buffer
   :element-type :float
   :data-usage :static-draw))

(defmethod shared-initialize :before ((asset vertex-buffer) slot-names &key (buffer-type (buffer-type asset))
                                                                            (element-type (element-type asset))
                                                                            (data-usage (data-usage asset)))
  ;; FIXME: automatically determine element-type from buffer-data if not specified
  (check-vertex-buffer-type buffer-type)
  (check-vertex-buffer-element-type element-type)
  (check-vertex-buffer-data-usage data-usage))

(defmethod (setf buffer-data) :after (data (asset vertex-buffer))
  (reload asset))

(defmethod load-data ((asset vertex-buffer))
  (with-slots (element-type buffer-data buffer-type data-usage) asset
    (let ((buffer (gl:gen-buffer))
          (array (gl:alloc-gl-array element-type (length buffer-data))))
      (unwind-protect
           (with-cleanup-on-failure
               (finalize-data asset buffer)
             (gl:bind-buffer buffer-type buffer)
             (loop for i from 0
                   for el across buffer-data
                   do (setf (gl:glaref array i) el))
             (gl:buffer-data buffer-type data-usage array))
        (gl:bind-buffer buffer-type 0)
        (gl:free-gl-array array))
      buffer)))

(defmethod finalize-data ((asset vertex-buffer) data)
  (gl:delete-buffers (list data)))

(defclass vertex-array (gl-asset)
  ((buffers :initarg :buffers :accessor buffers)))

(defmethod (setf buffers) :after (buffers (asset vertex-array))
  (reload asset))

(defmethod load-data ((asset vertex-array))
  (let ((array (gl:gen-vertex-array)))
    (with-cleanup-on-failure
        (finalize-data asset array)
      (gl:bind-vertex-array array)
      (loop for buffer in (buffers asset)
            do (destructuring-bind (pool name &key (index 0)
                                                   (size 3)
                                                   (normalized NIL)
                                                   (stride 0))
                   buffer
                 (let ((buffer (asset 'vertex-buffer pool name)))
                   (gl:bind-buffer (buffer-type buffer) (data buffer))
                   (gl:enable-vertex-attrib-array index)
                   (gl:vertex-attrib-pointer index size (element-type buffer) normalized stride (cffi:null-pointer))))))
    array))

(defmethod finalize-data ((asset vertex-array) data)
  (gl:delete-vertex-arrays (list data)))
