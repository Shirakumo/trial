(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image (path (type (eql :qoi)))
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

(defmethod save-image ((source texture-source) (path pathname) (type (eql :qoi)) &key)
  (destructuring-bind (x y z w h d) (texture-source-src source)
    (declare (ignore x y z d))
    (with-open-file (stream path :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let ((c (ecase (pixel-format source)
                 (1 :red)
                 (2 :rg)
                 (3 :rgb)
                 (4 :rgba))))
        (qoi:encode stream (flip-image-vertically (pixel-data source) w h c) w h c 1)))))

(define-native-image-transcoder :qoi)
