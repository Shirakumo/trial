#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image ((path pathname) (type (eql :jpeg)) &key)
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

(defmethod load-image ((source memory-region) (type (eql :jpeg)) &key)
  (multiple-value-bind (image height width components) (jpeg:decode-stream (memory-region-to-stream source))
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
