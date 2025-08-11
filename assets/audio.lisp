(in-package #:org.shirakumo.fraf.trial)

(define-standard-load-function load-audio)

;;;; Protocol functions for audio:
;; location velocity play stop fade-to done-p duration seek

(defclass audio-loader (compiled-generator)
  ())

(defmethod generate-resources ((loader audio-loader) input &rest args)
  (with-new-value-restart (input) (use-value "Specify a new audio source.")
    (with-retry-restart (retry "Retry loading the audio source.")
      (apply #'load-audio input T :generator loader args))))

(defmethod compile-resources ((generator audio-loader) (target sequence) &rest args &key &allow-other-keys)
  (map NIL (lambda (x) (apply #'compile-resources generator x args)) target))

(defmethod compile-resources ((generator audio-loader) (target pathname) &rest args &key (source-file-type "wav") force &allow-other-keys)
  (let ((source (make-pathname :type source-file-type :defaults target)))
    (when (and (probe-file source)
               (or force (trial:recompile-needed-p target source)))
      (apply #'transcode source T target T args))))

(defmacro define-ffmpeg-transcoder (target codec &rest args)
  `(defmethod transcode (source (source-type symbol) target (target-type (eql ,target)) &key (quality 3) (samplerate 48000))
     (run "ffmpeg" "-hide_banner" "-loglevel" "error"
          "-i" source
          "-c:a" ,codec
          "-ar" samplerate
          ,@args
          "-y" target)))

(defun opus-quality->bitrate (quality)
  (ecase quality
    (0 "160k")
    (1 "140k")
    (2 "120k")
    (3 "96k")
    (4 "80k")
    (5 "64k")
    (6 "48k")
    (7 "32k")
    (8 "24k")
    (9 "16k")))

(define-ffmpeg-transcoder :ogg "libvorbis" "-q:a" quality)
(define-ffmpeg-transcoder :oga "libvorbis" "-q:a" quality)
(define-ffmpeg-transcoder :mp3 "libmp3lame" "-q:a" quality)
(define-ffmpeg-transcoder :opus "libopus" "-b:a" (opus-quality->bitrate quality))
(define-ffmpeg-transcoder :flac "flac" "-q:a" quality)

(defmethod transcode (source (source-type (eql :wav)) target (target-type (eql :wav)) &rest args &key (sample-type :int16) (samplerate 48000))
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

(defmethod transcode (source (source-type symbol) target (target-type (eql :wav)) &key (sample-type :int16) (samplerate 48000))
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

(define-accessor-wrapper-methods location (audio-file (resource audio-file T)))
(define-accessor-wrapper-methods velocity (audio-file (resource audio-file T)))
(define-accessor-wrapper-methods stop (audio-file (resource audio-file T)))
(define-accessor-wrapper-methods done-p (audio-file (resource audio-file T)))
(define-accessor-wrapper-methods duration (audio-file (resource audio-file T)))

(defmethod play ((file audio-file) target)
  (play (resource file T) target))

(defmethod fade-to (target (file audio-file) &rest args)
  (apply #'fade-to target (resource file T) args))

(defmethod seek ((file audio-file) target)
  (seek (resource file T) target))

(defclass sound-bank (file-input-asset multi-resource-asset audio-loader)
  ())

(defmethod generate-resources ((loader sound-bank) (input list) &rest args)
  (dolist (file input)
    (with-new-value-restart (file) (use-value "Specify a new audio file.")
      (with-retry-restart (retry "Retry loading the audio source.")
        (apply #'load-audio file T :generator loader :resource (resource loader (pathname-name file))
               args)))))

(defmethod load :after ((loader sound-bank))
  (loop for resource being the hash-values of (slot-value loader 'resources)
        do (load resource)))

(defmethod play ((file sound-bank) target)
  (check-loaded file)
  (let ((chance 8f0)
        (resources (slot-value file 'resources)))
    (when (= 0 (hash-table-count resources))
      (error "Sound bank has no resources."))
    (loop (loop for resource being the hash-values of resources
                do (when (and (done-p resource) (< (random chance) 1))
                     (return-from play (play resource target))))
          (when (< (setf chance (* 0.5f0 chance)) 1.0)
            (return NIL)))))

(defmethod stop ((file sound-bank))
  (loop for resource being the hash-values of (slot-value file 'resources)
        do (stop resource)))
