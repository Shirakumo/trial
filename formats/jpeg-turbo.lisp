(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image (source (type (eql :jpeg)))
  (mem:with-memory-region (region source)
    (multiple-value-bind (buffer width height)
        (org.shirakumo.fraf.turbojpeg:load-image (mem:memory-region-pointer region) T
                                                 :size (mem:memory-region-size region)
                                                 :pixel-format :rgb
                                                 :buffer :vector)
      (make-image-source buffer width height :unsigned-byte :rgb))))

(defmethod load-image (path (type (eql :jpg)))
  (load-image path :jpeg))

(defmethod save-image ((source texture-source) destination (type (eql :jpeg)) &key)
  (destructuring-bind (x y z w h d) (texture-source-src source)
    (declare (ignore x y z d))
    (mem:with-memory-region (region destination)
      (org.shirakumo.fraf.turbojpeg:save-image (mem:memory-region-pointer region) (texture-source-pixel-data source) w h T
                                               :size (mem:memory-region-size region)
                                               :pixel-format (texture-source-pixel-format source)
                                               :bit-depth (ecase (texture-source-pixel-type source)
                                                            (:unsigned-byte 8)
                                                            (:unsigned-short 16))))))

(defmethod save-image (source target (type (eql :jpg)) &rest args)
  (apply #'save-image source target :jpeg args))
