#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

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
