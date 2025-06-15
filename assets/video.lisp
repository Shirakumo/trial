(in-package #:org.shirakumo.fraf.trial)

(define-standard-load-function load-video)

;;;; Protocol functions for video:
;; update done-p duration seek

(defclass video-loader (compiled-generator)
  ())

(defmethod generate-resources ((loader video-loader) input &rest args)
  (with-new-value-restart (input) (use-value "Specify a new video source.")
    (with-retry-restart (retry "Retry loading the video source.")
      (apply #'load-video input T :generator loader args))))

(defmethod compile-resources ((generator video-loader) target &rest args &key (source-file-type "mkv"))
  (let ((source (make-pathname :type source-file-type :defaults target)))
    (when (and (not (equal target source))
               (probe-file source)
               (trial:recompile-needed-p target source))
      (apply #'transcode source T target T args))))

(defclass video-file (file-input-asset multi-resource-asset video-loader)
  ((video :initform NIL :accessor video)))

(defmethod generate-resources ((asset video-file) input &key)
  (let ((video (call-next-method)))
    (setf (video asset) video)
    (list-resources asset)))

(defmethod unload :after ((asset video-file))
  (when (video asset)
    (finalize (video asset))))
