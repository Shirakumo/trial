(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image (path (type (eql :ter)))
  (let ((terrain (terrable:read-terrain path)))
    (tg:cancel-finalization terrain)
    (make-image-source (terrable:data terrain)
                       (terrable:width terrain)
                       (terrable:height terrain)
                       :signed-short
                       :red)))

(defmethod save-image ((source texture-source) (path pathname) (type (eql :ter)) &rest args)
  (destructuring-bind (x y z w h d) (texture-source-src source)
    (declare (ignore x y z d))
    (let ((terrain (apply #'make-instance 'terrable:terrain :width w :height h :data (pixel-data source) args)))
      (terrable:write-terrain terrain path))))

(define-native-image-transcoder :ter)
