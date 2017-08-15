#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass texture (asset)
  ((reuse-resource :initform NIL :accessor reuse-resource)
   (target :initarg :target :accessor target)
   (mag-filter :initarg :mag-filter :accessor mag-filter)
   (min-filter :initarg :min-filter :accessor min-filter)
   (anisotropy :initarg :anisotropy :accessor anisotropy)
   (wrapping :initarg :wrapping :reader wrapping))
  (:default-initargs
   :target :texture-2d
   :mag-filter :linear
   :min-filter :linear-mipmap-linear
   :anisotropy NIL
   :wrapping :clamp-to-edge))

(defmethod initialize-instance :around ((asset texture) &rest args)
  (setf (getf args :wrapping) (enlist (getf args :wrapping) (getf args :wrapping) (getf args :wrapping)))
  (apply #'call-next-method asset args))

(defmethod initialize-instance :before ((asset texture) &key target mag-filter min-filter wrapping)
  (check-texture-target target)
  (check-texture-mag-filter mag-filter)
  (check-texture-min-filter min-filter)
  (check-texture-wrapping (first wrapping))
  (check-texture-wrapping (second wrapping))
  (check-texture-wrapping (third wrapping)))

(defmethod coerce-input ((asset texture) (file pathname))
  (list file))

(defmethod coerce-input ((asset texture) (spec cons))
  spec)

(defmethod finalize-resource ((type (eql 'texture)) resource)
  (gl:delete-textures (list resource)))

(defun object-to-texparams (object)
  (destructuring-bind (file/bits &optional width height (format :rgba))
      object
    (cond ((pathnamep file/bits)
           (unless (probe-file file/bits)
             (error "The texture file ~s does not exist." file/bits))
           (multiple-value-bind (bits rwidth rheight)
               (cl-soil:load-image file/bits format)
             (list (or width rwidth) (or height rheight) bits format)))
          (T
           (list width (or height 1) (or file/bits (cffi:null-pointer)) format)))))

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
     (loop for level from 0
           for image in images
           do (v:debug :trial.asset "Loading texture from specs ~a" image)
              (case target
                (:texture-1d
                 (destructuring-bind (width height bits format) (object-to-texparams image)
                   (gl:tex-image-1d target level format (* width height) 0 (texture-internal-format->texture-format format) (texture-format->data-type format) bits)
                   (unless (cffi:null-pointer-p bits) (cffi:foreign-free bits))))
                ((:texture-cube-map-positive-x :texture-cube-map-negative-x
                  :texture-cube-map-positive-y :texture-cube-map-negative-y
                  :texture-cube-map-positive-z :texture-cube-map-negative-z
                  :texture-2d)
                 (destructuring-bind (width height bits format) (object-to-texparams image)
                   (gl:tex-image-2d target level format width height 0 (texture-internal-format->texture-format format) (texture-format->data-type format) bits)
                   (unless (cffi:null-pointer-p bits) (cffi:foreign-free bits))))
                (:texture-2d-multisample
                 (destructuring-bind (width height samples format) (object-to-texparams image)
                   (%gl:tex-image-2d-multisample target samples format width height 0)))
                (:texture-3d
                 (destructuring-bind (width height depth bits format) image
                   (gl:tex-image-3d target level format width height depth 0 (texture-internal-format->texture-format format) (texture-format->data-type format) bits)
                   (unless (cffi:null-pointer-p bits) (cffi:foreign-free bits)))))))))

(defmethod load progn ((asset texture))
  (with-slots (target mag-filter min-filter anisotropy wrapping) asset
    (let ((images (coerced-inputs asset))
          (texture (setf (resource asset) (or (reuse-resource asset) (gl:gen-texture)))))
      (with-cleanup-on-failure (offload asset)
        (gl:bind-texture target texture)
        (images-to-textures target images)
        (unless (eql target :texture-2d-multisample)
          (when (find min-filter '(:linear-mipmap-linear :linear-mipmap-nearest
                                   :nearest-mipmap-linear :nearest-mipmap-nearest))
            (gl:generate-mipmap target))
          (when anisotropy
            (gl:tex-parameter target :texture-max-anisotropy-ext anisotropy))
          (gl:tex-parameter target :texture-min-filter min-filter)
          (gl:tex-parameter target :texture-mag-filter mag-filter)
          (gl:tex-parameter target :texture-wrap-s (first wrapping))
          (gl:tex-parameter target :texture-wrap-t (second wrapping))
          (when (eql target :texture-cube-map)
            (gl:tex-parameter target :texture-wrap-r (third wrapping))))))))

(defmethod resize ((asset texture) width height)
  (let ((change NIL))
    (loop for input in (inputs asset)
          do (when (consp input)
               (when (and (second input) (/= (second input) width))
                 (setf (second input) width)
                 (setf change T))
               (when (and (third input) (/= (third input) height))
                 (setf (third input) height)
                 (setf change T))))
    (when (and (resource asset) change)
      ;; Force reload
      (setf (reuse-resource asset) (resource asset))
      (setf (resource asset) NIL)
      (load asset))))
