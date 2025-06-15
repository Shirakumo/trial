(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image ((stream stream) (type (eql :hdr)) &key)
  (let ((file (3b-hdr:read-hdr-stream stream :y-up T)))
    (make-image-source (3b-hdr:data file)
                       (3b-hdr:width file)
                       (3b-hdr:height file)
                       (3b-hdr:gl-pixel-type file)
                       (3b-hdr:gl-pixel-format file))))

(defmethod save-image ((source texture-source) (stream stream) (type (eql :hdr)) &key exposure)
  (destructuring-bind (x y z w h d) (texture-source-src source)
    (declare (ignore x y z d))
    (flip-image-vertically (pixel-data source) w h (ecase (pixel-format source) (:rgba 4) (:rgb 3) (:rg 2) (:red 1)))
    (let ((file (make-instance '3b-hdr:hdr-file :width w :height h 
                                                :pixel-type (pixel-type source)
                                                :pixel-format (pixel-format source)
                                                :data (pixel-data source)
                                                :exposure exposure)))
      (3b-hdr:write-hdr-stream stream file))))

(define-native-image-transcoder :hdr)
