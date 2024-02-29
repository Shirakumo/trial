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

(defmethod compile-resources ((generator audio-loader) target &key (source-file-type "wav") codec (samplerate 44100) (sample-type :int16) (quality 3))
  (let ((source (make-pathname :type source-file-type :defaults target)))
    (flet ((reencode ()
             (v:info :trial.asset "Compiling sound from ~a...." source)
             (run "ffmpeg" "-hide_banner" "-loglevel" "error"
                  "-i" source
                  "-c:a" (cond (codec codec)
                               ((string-equal "ogg" (pathname-type target)) "libvorbis")
                               ((string-equal "oga" (pathname-type target)) "libvorbis")
                               ((string-equal "mp3" (pathname-type target)) "libmp3lame")
                               ((string-equal "opus" (pathname-type target)) "libopus")
                               ((string-equal "flac" (pathname-type target)) "flac")
                               ((string-equal "wav" (pathname-type target))
                                (format NIL "pcm_~ale"
                                        (case sample-type
                                          (:int16 "s16")
                                          (:int24 "s24")
                                          (:int32 "s32")
                                          (:uint16 "u16")
                                          (:uint24 "u24")
                                          (:uint32 "u32")
                                          (:float "f32")
                                          (:double "f64"))))
                               (T (error "Unsupported file type ~s" (pathname-type target))))
                  "-ar" samplerate
                  "-q:a" quality
                  "-y" target)))
      (cond ((string= "wav" (pathname-type source))
             (let (source-sample-type source-samplerate)
               (loop for line in (cl-ppcre:split "\\n+" (run "ffprobe" "-hide_banner" "-loglevel" "error" "-i" source "-of" "flat=s=-" "-show_streams"))
                     do (when (search "sample_fmt" line)
                          (setf source-sample-type
                                (cond ((search "s16" line) :int16)
                                      ((search "s24" line) :int24)
                                      ((search "s32" line) :int32)
                                      ((search "u16" line) :uint16)
                                      ((search "u24" line) :uint24)
                                      ((search "u32" line) :uint32)
                                      ((search "f32" line) :float)
                                      ((search "f64" line) :double))))
                        (when (search "sample_rate" line)
                          (setf source-samplerate (parse-integer line :start (+ 2 (position #\= line)) :end (1- (length line))))))
               (when (or (< samplerate source-samplerate)
                         (not (equal sample-type source-sample-type)))
                 (reencode))))
            (T
             (when (and (not (equal target source))
                        (probe-file source)
                        (trial:recompile-needed-p target source))
               (reencode)))))))

(defclass audio-file (file-input-asset single-resource-asset audio-loader)
  ((audio :initform NIL :accessor audio)))

(defmethod generate-resources ((asset audio-file) input &key)
  (let ((audio (call-next-method)))
    (setf (audio asset) audio)
    (list-resources asset)))

(defmethod unload :after ((asset audio-file))
  (when (audio asset)
    (finalize (audio asset))))
