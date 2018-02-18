#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass texture (gl-resource)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (depth :initarg :depth :accessor depth)
   (target :initarg :target :accessor target)
   (level :initarg :level :accessor level)
   (samples :initarg :samples :accessor samples)
   (internal-format :initarg :internal-format :accessor internal-format)
   (pixel-format :initarg :pixel-format :accessor pixel-format)
   (pixel-type :initarg :pixel-type :accessor pixel-type)
   (pixel-data :initarg :pixel-data :accessor pixel-data)
   (mag-filter :initarg :mag-filter :accessor mag-filter)
   (min-filter :initarg :min-filter :accessor min-filter)
   (anisotropy :initarg :anisotropy :accessor anisotropy)
   (wrapping :initarg :wrapping :accessor wrapping)
   (storage :initarg :storage :reader storage))
  (:default-initargs
   :width 1
   :height 1
   :depth 1
   :target :texture-2d
   :level 0
   :samples 1
   :internal-format :rgba
   :pixel-type :unsigned-byte
   :pixel-data NIL
   :mag-filter :linear
   :min-filter :linear-mipmap-linear
   :anisotropy NIL
   :wrapping :clamp-to-edge
   :storage :dynamic))

(defmethod initialize-instance :around ((texture texture) &rest args)
  (setf (getf args :wrapping) (enlist (getf args :wrapping)
                                      (getf args :wrapping)
                                      (getf args :wrapping)))
  (unless (getf args :pixel-format)
    (setf (getf args :pixel-format) (texture-internal-format->pixel-format (getf args :internal-format))))
  (unless (getf args :pixel-type)
    (setf (getf args :pixel-type) (pixel-format->pixel-type (getf args :pixel-format))))
  (apply #'call-next-method texture args))

(defmethod initialize-instance :before ((texture texture) &key width height depth target internal-format pixel-format pixel-type mag-filter min-filter wrapping)
  (assert (< 0 width (gl:get* :max-texture-size)))
  (assert (< 0 height (gl:get* :max-texture-size)))
  (assert (< 0 depth (gl:get* :max-texture-size)))
  (check-texture-target target)
  (check-texture-internal-format internal-format)
  (check-texture-pixel-format pixel-format)
  (check-texture-pixel-type pixel-type)
  (check-texture-mag-filter mag-filter)
  (check-texture-min-filter min-filter)
  (check-texture-wrapping (first wrapping))
  (check-texture-wrapping (second wrapping))
  (check-texture-wrapping (third wrapping)))

(defmethod destructor ((texture texture))
  (let ((tex (gl-name texture)))
    (lambda () (gl:delete-textures (list tex)))))

(defun allocate-texture-storage (texture)
  (with-slots (target storage level internal-format width height depth samples pixel-format pixel-type pixel-data) texture
    (case target
      ((:texture-1d)
       (ecase storage
         (:dynamic (%gl:tex-image-1d target level internal-format width 0 pixel-format pixel-type pixel-data))
         (:static (%gl:tex-storage-1d target level internal-format width))))
      ((:texture-2d :texture-1d-array)
       (ecase storage
         (:dynamic (%gl:tex-image-2d target level internal-format width height 0 pixel-format pixel-type pixel-data))
         (:static (%gl:tex-storage-2d target level internal-format width height))))
      ((:texture-cube-map)
       (loop for target in '(:texture-cube-map-positive-x :texture-cube-map-negative-x
                             :texture-cube-map-positive-y :texture-cube-map-negative-y
                             :texture-cube-map-positive-z :texture-cube-map-negative-z)
             for data in (if (consp pixel-data)
                             pixel-data
                             (let ((c (cons pixel-data NIL)))
                               (setf (cdr c) c)))
             do (ecase storage
                  (:dynamic (%gl:tex-image-2d target level internal-format width height 0 pixel-format pixel-type data))
                  (:static (%gl:tex-storage-2d target level internal-format width height)))))
      ((:texture-3d :texture-2d-array)
       (ecase storage
         (:dynamic (%gl:tex-image-3d target level internal-format width height depth 0 pixel-format pixel-type pixel-data))
         (:static (%gl:tex-storage-3d target level internal-format width height depth))))
      ((:texture-2d-multisample)
       (%gl:tex-storage-2d-multisample target samples internal-format width height 1))
      ((:texture-2d-multisample-array)
       (%gl:tex-storage-3d-multisample target samples internal-format width height depth 1)))))

(defmethod allocate ((texture texture))
  (with-slots (width height depth target level samples internal-format pixel-format pixel-type pixel-data mag-filter min-filter anisotropy wrapping storage)
      texture
    (let ((tex (gl:create-texture target)))
      (with-cleanup-on-failure (gl:delete-textures (list tex))
        (gl:bind-texture target tex)
        (allocate-texture-storage texture)
        (unless (or (eql target :texture-2d-multisample)
                    (find internal-format '(:depth-component :depth-stencil)))
          (when (find min-filter '(:linear-mipmap-linear :linear-mipmap-nearest
                                   :nearest-mipmap-linear :nearest-mipmap-nearest))
            (gl:generate-mipmap target))
          (when anisotropy
            (gl:tex-parameter target :texture-max-anisotropy-ext anisotropy))
          (gl:tex-parameter target :texture-min-filter min-filter)
          (gl:tex-parameter target :texture-mag-filter mag-filter)
          (gl:tex-parameter target :texture-wrap-s (first wrapping))
          (unless (find target '(:texture-1d-array :texture-1d))
            (gl:tex-parameter target :texture-wrap-t (second wrapping)))
          (when (eql target :texture-cube-map)
            (gl:tex-parameter target :texture-wrap-r (third wrapping))))
        (setf (data-pointer texture) tex)))))

(defmethod resize ((texture texture) width height)
  (when (or (/= width (width texture))
            (/= height (height texture)))
    (assert (eql :dynamic (storage texture)))
    (setf (width texture) width)
    (setf (height texture) height)
    (when (allocated-p texture)
      (allocate-texture-storage texture)
      (when (find (min-filter texture) '(:linear-mipmap-linear :linear-mipmap-nearest
                                         :nearest-mipmap-linear :nearest-mipmap-nearest))
        (gl:generate-mipmap (target texture))))))
