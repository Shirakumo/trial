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

(defun downscale-image (image width height components width2 height2)
  (let ((target (make-array (* width2 height2 components) :element-type '(unsigned-byte 8) :initial-element 0))
        (x-ratio (float (/ width width2) 0f0))
        (y-ratio (float (/ height height2) 0f0))
        (offset 0))
    (declare (optimize speed))
    (declare (type (unsigned-byte 32) width height width2 height2 offset))
    (declare (type (unsigned-byte 8) components))
    (declare (type (array (unsigned-byte 8)) image))
    (dotimes (i height2 target)
      (dotimes (j width2)
        (dotimes (c components)
          (let ((avg 0) (count 0))
            (declare (type (unsigned-byte 32) avg count))
            (loop for fi from (max 0 (floor (* y-ratio (- i 0.5)))) to (min (1- height) (ceiling (* y-ratio (+ i 0.5))))
                  do (loop for fj from (max 0 (floor (* x-ratio (- j 0.5)))) to (min (1- width) (ceiling (* x-ratio (+ j 0.5))))
                           do (incf avg (aref image (+ c (* components (+ fj (* fi width))))))
                              (incf count)))
            (setf (aref target offset) (floor (/ avg count)))
            (incf offset)))))))

(defgeneric load-image (path type &key width height pixel-type pixel-format &allow-other-keys))

(defmethod load-image (path (type (eql :tga)) &key)
  (let ((tga (tga:read-tga path)))
    (values (tga:image-data tga)
            (tga:image-width tga)
            (tga:image-height tga)
            (infer-pixel-type (/ (tga:image-bpp tga)
                                 (tga:image-channels tga))
                              :unsigned)
            (ecase (tga:image-channels tga)
              (1 :red)
              (2 :gr)
              (3 :bgr)
              (4 :bgra)))))

(defmethod load-image (path (type (eql :png)) &key)
  (let ((png (pngload:load-file path :flatten T :flip-y T)))
    (values (pngload:data png)
            (pngload:width png)
            (pngload:height png)
            (infer-pixel-type (pngload:bit-depth png) :unsigned)
            (ecase (pngload:color-type png)
              (:greyscale :red)
              (:greyscale-alpha :rg)
              (:truecolour :rgb)
              (:truecolour-alpha :rgba)))))

(defmethod load-image (path (type (eql :tiff)) &key)
  (let* ((tiff (retrospectiff:read-tiff-file path)))
    ;; FIXME: higher bittage than 8 still returns an ub8 array, but GL doesn't like it.
    (values (retrospectiff:tiff-image-data tiff)
            (retrospectiff:tiff-image-width tiff)
            (retrospectiff:tiff-image-length tiff)
            (infer-pixel-type (retrospectiff:tiff-image-bits-per-sample tiff) :unsigned)
            (ecase (retrospectiff:tiff-image-samples-per-pixel tiff)
              (1 :red)
              (3 :rgb)
              (4 :rgba)))))

(defmethod load-image (path (type (eql :tif)) &rest args)
  (apply #'load-image path :tiff args))

(defmethod load-image (path (type (eql :jpeg)) &key)
  (multiple-value-bind (image height width components) (jpeg:decode-image path)
    (flip-image-vertically image width height components)
    (values image
            width
            height
            :unsigned-byte
            (ecase components
              (1 :red)
              (2 :gr)
              (3 :bgr)
              (4 :bgra)))))

(defmethod load-image (path (type (eql :jpg)) &rest args)
  (apply #'load-image path :jpeg args))

(defmethod load-image (path (type (eql :ter)) &key)
  (let ((terrain (terrable:read-terrain path)))
    (tg:cancel-finalization terrain)
    (values (terrable:data terrain)
            (terrable:width terrain)
            (terrable:height terrain)
            :signed-short
            :red)))

(defmethod load-image (path (type (eql T)) &rest args)
  (let ((type (pathname-type path)))
    (apply #'load-image path (kw type) args)))

(defclass image-loader (resource-generator)
  ())

(defmethod generate-resources ((generator image-loader) path &rest texture-args &key (type T) internal-format pixel-format (resource (resource generator T)) &allow-other-keys)
  (multiple-value-bind (bits width height pixel-type inferred-pixel-format)
      (with-new-value-restart (path) (new-path "Specify a new image path.")
        (with-retry-restart (retry "Retry loading the image path.")
          (load-image path type)))
    (assert (not (null bits)))
    (let ((pixel-format (or pixel-format inferred-pixel-format)))
      (with-unwind-protection (free-data bits)
        (apply #'ensure-instance resource 'texture
               :width width :height height
               :pixel-data bits
               :pixel-type pixel-type
               :pixel-format pixel-format
               :internal-format (or internal-format
                                    (infer-internal-format pixel-type pixel-format))
               (remf* texture-args :type :resource))))))

(defclass image (single-resource-asset file-input-asset image-loader)
  ())

;; FIXME: multi-image textures such as cube maps
