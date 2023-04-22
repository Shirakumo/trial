#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image (path (type (eql :bmp)) &key)
  (multiple-value-bind (image width height channels) (org.shirakumo.bmp:read-bmp path)
    (flip-image-vertically image width height channels)
    (values image
            width
            height
            :unsigned-byte
            (ecase channels
              (1 :red)
              (2 :gr)
              (3 :rgb)
              (4 :rgba)))))

;; TODO: ICO to decode to mips? maybe?
