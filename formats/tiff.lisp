(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image (path (type (eql :tiff)))
  (let* ((tiff (retrospectiff:read-tiff-file path)))
    ;; FIXME: higher bittage than 8 still returns an ub8 array, but GL doesn't like it.
    (make-image-source (retrospectiff:tiff-image-data tiff)
                       (retrospectiff:tiff-image-width tiff)
                       (retrospectiff:tiff-image-length tiff)
                       (infer-pixel-type (retrospectiff:tiff-image-bits-per-sample tiff) :unsigned)
                       (ecase (retrospectiff:tiff-image-samples-per-pixel tiff)
                         (1 :red)
                         (3 :rgb)
                         (4 :rgba)))))

(defmethod load-image (path (type (eql :tif)))
  (load-image path :tiff))

(defmethod save-image ((source texture-source) (path pathname) (type (eql :tiff)) &key)
  (destructuring-bind (x y z w h d) (texture-source-src source)
    (declare (ignore x y z d))
    (let ((image (make-instance 'retrospectiff:tiff-image :width w :length h
                                                          :bit-sper-sample (ecase (pixel-type source)
                                                                             (:unsigned-byte 8)
                                                                             (:unsigned-short 16)
                                                                             (:unsigned-int 32))
                                                          :samples-per-pixel (ecase (pixel-format source)
                                                                               (:red 1)
                                                                               (:rg 2)
                                                                               (:rgb 3)
                                                                               (:rgba 4))
                                                          :data (pixel-data source))))
      (retrospectiff:write-tiff-file path image :if-exists :supersede))))

(define-native-image-transcoder :tiff)
