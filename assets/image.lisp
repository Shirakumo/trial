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
                   do (rotatef (aref image x1) (aref image x2))))))

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
  (let* ((tiff (retrospectiff:read-tiff-file path))
         (bits (aref (retrospectiff:tiff-image-bits-per-sample tiff) 1)))
    ;; FIXME: higher bittage than 8 still returns an ub8 array, but GL doesn't like it.
    (values (retrospectiff:tiff-image-data tiff)
            (retrospectiff:tiff-image-width tiff)
            (retrospectiff:tiff-image-length tiff)
            (infer-pixel-type bits :unsigned)
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
    (apply #'load-image path (intern (string-upcase type) "KEYWORD") args)))

(defclass image-loader (resource-generator)
  ())

(defmethod generate-resources ((generator image-loader) path &rest texture-args &key (type T) internal-format (resource (resource generator T)) &allow-other-keys)
  (multiple-value-bind (bits width height pixel-type pixel-format)
      (with-new-value-restart (path) (new-path "Specify a new image path.")
        (with-retry-restart (retry "Retry loading the image path.")
          (load-image path type)))
    (assert (not (null bits)))
    (with-unwind-protection (free-data bits)
      (apply #'ensure-instance resource 'texture
             :width width :height height
             :pixel-format pixel-format
             :pixel-type pixel-type
             :pixel-data bits
             :internal-format (or internal-format
                                  (infer-internal-format pixel-type pixel-format))
             (remf* texture-args :type :resource)))))

(defclass image (single-resource-asset file-input-asset image-loader)
  ())

;; FIXME: multi-image textures such as cube maps
