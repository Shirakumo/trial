(in-package #:org.shirakumo.fraf.trial)

(defgeneric load-audio (source type &key generator &allow-other-keys))

(defmethod load-audio (source (type string) &rest args &key &allow-other-keys)
  (apply #'load-audio source (or (cl-ppcre:register-groups-bind (type) ("^[^/]*/([^+/]+)" type) (kw type)) (kw type)) args))

(defmethod load-audio ((source pathname) (type (eql T)) &rest args &key &allow-other-keys)
  (apply #'load-audio source (pathname-type source) args))

(defmethod load-audio (source (type symbol) &key &allow-other-keys)
  (let ((types (delete T (list-eql-specializers #'load-audio 1))))
    (if (find type types)
        (error "Don't know how to load~%  ~a~%from ~a"
               source type)
        (error "Don't know how to load from ~a~%known types are:~%  ~a~%Did you load the respective format system?"
               type types))))

(defclass audio-loader (compiled-generator)
  ())

(defmethod generate-resources ((loader audio-loader) input &rest args)
  (with-new-value-restart (input) (use-value "Specify a new audio source.")
    (with-retry-restart (retry "Retry loading the audio source.")
      (apply #'load-audio input T :generator loader args))))

(defmethod compile-resources ((generator audio-loader) source &key (source-file-type "wav") codec (quality 5))
  (run "ffmpeg" "-hide_banner" "-loglevel" "error"
       "-i" (make-pathname :type source-file-type :defaults source)
       "-codec:a" (cond (codec codec)
                        ((string-equal "ogg" (pathname-type source)) "libvorbis")
                        ((string-equal "mp3" (pathname-type source)) "libmp3lame")
                        ((string-equal "opus" (pathname-type source)) "libopus")
                        ((string-equal "flac" (pathname-type source)) "flac")
                        (T (error "Unsupported file type ~s" (pathname-type source))))
       "-qscale:a" quality
       source))

(defclass audio-file (file-input-asset single-resource-asset audio-loader)
  ((audio :initform NIL :accessor audio)))

(defmethod generate-resources ((asset audio-file) input &key)
  (let ((audio (call-next-method)))
    (setf (audio asset) audio)
    (list-resources asset)))

(defmethod unload :after ((asset audio-file))
  (when (audio asset)
    (finalize (audio asset))))
