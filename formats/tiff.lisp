#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

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
