#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

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
