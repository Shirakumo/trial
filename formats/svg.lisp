(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image ((stream stream) (type (eql :svg)) &key)
  (org.shirakumo.fraf.resvg:with-image (svg (alexandria:read-stream-content-into-string stream))
    (multiple-value-bind (pixels width height) (org.shirakumo.fraf.resvg:render svg)
      (make-image-source pixels width height :unsigned-byte :rgba))))

