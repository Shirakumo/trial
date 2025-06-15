(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image ((stream stream) (type (eql :bmp)) &key)
  (let ((bmp (org.shirakumo.bmp:read-bmp path)))
    (multiple-value-bind (data width height channels) (org.shirakumo.bmp:decode-pixels bmp)
      (flip-image-vertically data width height channels)
      (make-image-source data
                         width
                         height
                         :unsigned-byte
                         (ecase channels
                           (1 :red)
                           (2 :rg)
                           (3 :rgb)
                           (4 :rgba))))))

(defmethod save-image ((source texture-source) (stream stream) (type (eql :bmp)) &key)
  (destructuring-bind (x y z w h d) (texture-source-src source)
    (declare (ignore x y z d))
    (let ((bmp (org.shirakumo.bmp:make-bmp)))
      (org.shirakumo.bmp:encode-pixels bmp (pixel-data source) w h
                                       (ecase (pixel-format source)
                                         (:red 1)
                                         (:rg 2)
                                         (:rgb 3)
                                         (:rgba 4)))
      (org.shirakumo.bmp:write-bmp bmp stream))))

(define-native-image-transcoder :bmp)

;; TODO: ICO to decode to mips? maybe?
