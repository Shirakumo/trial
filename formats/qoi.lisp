#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image (path (type (eql :qoy)) &key)
  (multiple-value-bind (image width height channels)
      (with-open-file (stream path :element-type '(unsigned-byte 8))
        (qoi:decode stream))
    (flip-image-vertically image width height channels)
    (make-image-source image
                       width
                       height
                       :unsigned-byte
                       (ecase channels
                         (1 :red)
                         (2 :rg)
                         (3 :rgb)
                         (4 :rgba)))))
