(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass font (trial:file-input-asset trial:compiled-generator)
  ())

(defmethod trial:compile-resources ((font font) (input pathname) &rest args &key &allow-other-keys)
  (apply #'trial:transcode (make-pathname :type "ttf" :defaults input) :ttf input :json args))

(defmethod trial:transcode (source (source-type (eql :ttf)) target (target-type (eql :json)) &rest args &key &allow-other-keys)
  (let* ((json (apply #'org.shirakumo.alloy.renderers.opengl.msdf:cache-font
                      source :cache-file target
                      args))
         (png (make-pathname :type "png" :defaults json)))
    (ignore-errors (trial::run "optipng" "-o" "5" "-clobber" "-out" png png))
    json))
