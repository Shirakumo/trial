(in-package #:org.shirakumo.fraf.trial)

(defgeneric load-audio (source type &key generator &allow-other-keys))

(defmethod load-audio (source (type string) &rest args &key &allow-other-keys)
  (apply #'load-audio source (normalize-file-type type) args))

(defmethod load-audio ((source pathname) (type (eql T)) &rest args &key &allow-other-keys)
  (apply #'load-audio source (pathname-type source) args))

(defmethod load-audio (source (type symbol) &key &allow-other-keys)
  (let ((types (delete T (list-eql-specializers #'load-audio 1))))
    (if (find type types)
        (error "Don't know how to load~%  ~a~%from ~a"
               source type)
        (error "Don't know how to load from ~a~%known types are:~%  ~a~%Did you load the respective format system?"
               type types))))

;;;; Protocol functions for audio:
;; location velocity play stop fade-to done-p duration seek

(defclass audio-loader (compiled-generator)
  ())

(defmethod generate-resources ((loader audio-loader) input &rest args)
  (with-new-value-restart (input) (use-value "Specify a new audio source.")
    (with-retry-restart (retry "Retry loading the audio source.")
      (apply #'load-audio input T :generator loader args))))

(defmethod compile-resources ((generator audio-loader) target &rest args &key (source-file-type "wav"))
  (let ((source (make-pathname :type source-file-type :defaults target)))
    (when (and (probe-file source)
               (trial:recompile-needed-p target source))
      (apply #'transcode source T target T args))))

(defmacro define-ffmpeg-transcoder (target codec)
  `(defmethod transcode (source (source-type symbol) target (target-type (eql ,target)) &key (quality 3) (samplerate 44100))
     (run "ffmpeg" "-hide_banner" "-loglevel" "error"
          "-i" source
          "-c:a" ,codec
          "-ar" samplerate
          "-q:a" quality
          "-y" target)))

(define-ffmpeg-transcoder :ogg "libvorbis")
(define-ffmpeg-transcoder :oga "libvorbis")
(define-ffmpeg-transcoder :mp3 "libmp3lame")
(define-ffmpeg-transcoder :opus "libopus")
(define-ffmpeg-transcoder :flac "flac")

(defmethod transcode (source (source-type (eql :wav)) target (target-type (eql :wav)) &rest args &key (sample-type :int16) (samplerate 44100))
  (if (equalp source target)
      (let ((tmp (make-pathname :name (format NIL "tmp_~a" (pathname-name target)) :defaults target)))
        (apply #'transcode source source-type tmp target-type args)
        (when (probe-file tmp)
          (rename-file tmp target)))
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
          (call-next-method)))))

(defmethod transcode (source (source-type symbol) target (target-type (eql :wav)) &key (sample-type :int16) (samplerate 44100))
  (run "ffmpeg" "-hide_banner" "-loglevel" "error"
       "-i" source
       "-c:a" (format NIL "pcm_~ale"
                      (ecase sample-type
                        (:int16 "s16")
                        (:int24 "s24")
                        (:int32 "s32")
                        (:uint16 "u16")
                        (:uint24 "u24")
                        (:uint32 "u32")
                        (:float "f32")
                        (:double "f64")))
       "-ar" samplerate
       "-y" target))

(defmethod transcode (source (source-type (eql :wav)) target (target-type (eql :qoa)) &key)
  (if (find-package '#:org.shirakumo.qoa)
      (funcall (find-symbol (string '#:convert-wav) '#:org.shirakumo.qoa) source :out target :if-exists :supersede)
      (call-next-method)))

(defclass audio-file (file-input-asset single-resource-asset audio-loader)
  ())
