#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defun flip-image-vertically (image width height components)
  (let ((stride (* width components)))
    (loop for y1 from 0 below (floor height 2)
          for y2 downfrom (1- height)
          do (loop for x1 from (* y1 stride)
                   for x2 from (* y2 stride)
                   repeat stride
                   do (rotatef (aref image x1) (aref image x2))))
    image))

(defun convert-image-data (data-in width-in height-in &key (pixel-type-in :unsigned-byte) (pixel-format-in :rgba) (swizzle '(:r :g :b :a)) (pixel-type-out pixel-type-in) (pixel-format-out pixel-format-in)
                                                           (width-out width-in) (height-out height-in))
  (cond ((and (eql pixel-type-in pixel-type-out)
              (eql pixel-format-in pixel-format-out)
              (eql width-in width-out)
              (eql height-in height-out)
              (equal swizzle '(:r :g :b :a)))
         data-in)
        (T
         ;; TODO: implement convert-image-data
         (error "IMPLEMENT"))))

(defgeneric load-image (source type))

(defgeneric save-image (source target type &key))

(defmethod save-image (source (path pathname) (type (eql T)) &rest args)
  (apply #'save-image source path (kw (pathname-type path)) args))

(defmethod save-image (source target (type string) &rest args)
  (apply #'save-image source target
         (or (cl-ppcre:register-groups-bind (type) ("^[^/]*/([^+/]+)" mime-type) (kw type)) (kw type))
         args))

(defmethod load-image (source (type string))
  (or (cl-ppcre:register-groups-bind (type) ("^[^/]*/([^+/]+)" type)
        (load-image source (kw type)))
      (load-image source (kw type))))

(defmethod load-image ((path pathname) (type (eql T)))
  (load-image path (kw (pathname-type path))))

(defmethod load-image ((source texture-source) type)
  (merge-texture-sources (load-image (texture-source-pixel-data source) type) source))

(defmethod load-image ((sources cons) (type (eql T)))
  (loop for source in sources collect (load-image source T)))

(defun %load-image (source type)
  (with-new-value-restart (source) (use-value "Specify a new image source.")
    (with-retry-restart (retry "Retry loading the image source.")
      (load-image source type))))

(defclass image-loader (resource-generator)
  ())

(defmethod generate-resources ((generator image-loader) sources &rest texture-args &key (type T) target swizzle internal-format (resource (resource generator T)) (texture-class 'texture) &allow-other-keys)
  (multiple-value-bind (sources source-swizzle) (normalize-texture-sources (enlist (%load-image sources type)) target)
    (destructuring-bind (width height depth) (texture-sources->texture-size sources)
      (unwind-protect (apply #'ensure-instance resource texture-class
                             :sources sources :width width :height height :depth depth :target (or target (texture-sources->target sources))
                             :internal-format (or internal-format (infer-internal-format (pixel-type (first sources)) (pixel-format (first sources))))
                             :swizzle (or swizzle source-swizzle (infer-swizzle-format (pixel-format (first sources))))
                             (remf* texture-args :type :target :swizzle :internal-format :resource :texture-class))
        (dolist (source sources)
          (free-data (texture-source-pixel-data source)))))))

(defclass image (single-resource-asset file-input-asset image-loader)
  ())
