(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image (source (type (eql :svg)))
  (org.shirakumo.fraf.resvg:with-image (svg source)
    (multiple-value-bind (pixels width height) (org.shirakumo.fraf.resvg:render svg)
      (make-image-source pixels width height :unsigned-byte :rgba))))

