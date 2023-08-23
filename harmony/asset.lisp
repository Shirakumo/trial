(in-package #:org.shirakumo.fraf.trial.harmony)

(defun run (&rest args)
  (uiop:run-program (loop for arg in args
                          collect (typecase arg
                                    (pathname (uiop:native-namestring arg))
                                    (string arg)
                                    (T (princ-to-string arg))))
                    :output :string :error-output *error-output*))

(defclass sound-loader (trial:compiled-generator)
  ())

(defmethod trial:compile-resources ((generator sound-loader) path &key (samplerate 44100) (sample-type :int16) codec (source-file-type "wav") (quality 3))
  (cond ((string= "wav" (pathname-type path))
         (let ((temp (make-pathname :type "temp.wav" :defaults path))
               source-samplerate source-sample-type)
           (loop for line in (cl-ppcre:split "\\n+" (run "ffprobe" "-hide_banner" "-loglevel" "error" "-i" path "-of" "flat=s=-" "-show_streams"))
                 do (when (search "sample_fmt" line)
                      (setf source-sample-type
                            (cond ((search "s16" line) :int16)
                                  ((search "s24" line) :int24)
                                  ((search "s32" line) :int32)
                                  ((search "u16" line) :uint16)
                                  ((search "u24" line) :uint24)
                                  ((search "u32" line) :uint32))))
                    (when (search "sample_rate" line)
                      (setf source-samplerate (parse-integer line :start (+ 2 (position #\= line)) :end (1- (length line))))))
           (unless (and (equal samplerate source-samplerate)
                        (equal sample-type source-sample-type))
             (v:info :trial.harmony "Reencoding sound file from ~a..." path)
             (run "ffmpeg" "-hide_banner" "-loglevel" "error"
                  "-i" path
                  "-c:a" (format NIL "pcm_~ale"
                                 (case sample-type
                                   (:int16 "s16")
                                   (:int24 "s24")
                                   (:int32 "s32")
                                   (:uint16 "u16")
                                   (:uint24 "u24")
                                   (:uint32 "u32")))
                  "-ar" samplerate
                  temp)
             (rename-file temp path))))
        (T
         (let ((source (make-pathname :type source-file-type :defaults path)))
           (when (and (not (equal path source))
                      (probe-file source)
                      (trial:recompile-needed-p path source))
             (v:info :trial.harmony "Compiling sound from ~a...." path)
             (run "ffmpeg" "-hide_banner" "-loglevel" "error"
                  "-i" source
                  "-c:a" (cond (codec codec)
                               ((string-equal "oga" (pathname-type path)) "libvorbis")
                               ((string-equal "ogg" (pathname-type path)) "libvorbis")
                               ((string-equal "flac" (pathname-type path)) "flac")
                               ((string-equal "mp3" (pathname-type path)) "libmp3lame")
                               ((string-equal "opus" (pathname-type path)) "libopus")
                               (T (error "Unsupported file type ~s" (pathname-type path))))
                  "-ar" samplerate
                  "-qscale:a" quality
                  "-y" path))))))

(defmethod trial:generate-resources ((generator sound-loader) path &key (voice-class 'harmony:voice) (mixer :effect) effects repeat (repeat-start 0) (volume 1.0) (resource (trial:resource generator T)) max-distance min-distance)
  (trial::ensure-instance resource 'voice
                          :voice-class voice-class
                          :mixer mixer :source path :effects effects :volume volume
                          :max-distance max-distance :min-distance min-distance
                          :repeat repeat :repeat-start repeat-start))

(defclass sound (trial:single-resource-asset trial:file-input-asset sound-loader)
  ())

(defmethod trial:reload ((sound sound))
  (let ((resource (trial:resource sound T)))
    (when (and (typep resource 'voice)
               (voice resource))
      (mixed:end (harmony:source (voice resource)))
      (mixed:start (harmony:source (voice resource))))))

(defmethod reinitialize-instance :after ((sound sound) &key)
  (unless (typep (trial:resource sound T) 'trial:placeholder-resource)
    (apply #'trial:generate-resources sound (trial:input* sound) (trial::generation-arguments sound))))

(defclass environment-loader (trial:compiled-generator)
  ())

(defmethod trial:compile-resources ((generator environment-loader) sets &key (source-file-type "wav") codec (samplerate 44100) (quality 3))
  (dolist (set sets)
    (dolist (track (cdr set))
      (let ((target (first track))
            (source (make-pathname :type source-file-type :defaults (first track))))
        (when (and (not (equal target source))
                   (probe-file source)
                   (trial:recompile-needed-p target source))
          (v:info :trial.harmony "Compiling music track from ~a...." source)
          (run "ffmpeg" "-hide_banner" "-loglevel" "error"
               "-i" source
               "-c:a" (cond (codec codec)
                            ((string-equal "oga" (pathname-type target)) "libvorbis")
                            ((string-equal "ogg" (pathname-type target)) "libvorbis")
                            ((string-equal "flac" (pathname-type target)) "flac")
                            ((string-equal "mp3" (pathname-type target)) "libmp3lame")
                            ((string-equal "opus" (pathname-type target)) "libopus")
                            (T (error "Unsupported file type ~s" (pathname-type target))))
               "-ar" samplerate
               "-qscale:a" quality
               "-y" target))))))

(defmethod trial:generate-resources ((generator environment-loader) sets &key (resource (trial:resource generator T)))
  (if (typep resource 'music)
      resource
      (trial::ensure-instance resource 'music :sets sets)))

(defclass environment (trial:single-resource-asset environment-loader)
  ())

(defmethod trial:reload ((asset environment))
  )

(defmethod trial:coerce-asset-input ((asset environment) (input string))
  (trial:coerce-asset-input asset (pathname input)))

(defmethod trial:coerce-asset-input ((asset environment) (description cons))
  (loop for (key . set) in description
        collect (cons key (loop for track in set
                                for (input . params) = (if (listp track) track (list track))
                                for file = (trial:coerce-asset-input asset input)
                                collect (list* file :name (file-namestring file) params)))))

(defclass music (trial:resource harmony:environment)
  ())

(defmethod trial:allocated-p ((music music)) T)

(defmethod trial:allocate ((music music)) music)

(defmethod trial:deallocate ((music music))
  (harmony:with-server ()
    (mixed:free music))
  (change-class music 'trial:placeholder-resource))

;; KLUDGE: This cannot be a harmony:voice since it does not handle the change-class/reinitialize-instance
;;         protocol we have going for voices gracefully. It would also cause allocation to happen at
;;         initialisation rather than at, well, allocation time.
(defclass voice (trial:resource)
  ((voice :initform NIL :accessor voice)
   (voice-class :initarg :voice-class :initform 'harmony:voice :accessor voice-class)
   (source :initarg :source :accessor source)
   (mixer :initarg :mixer :initform :effect :accessor mixer)
   (effects :initarg :effects :initform NIL :accessor effects)
   (repeat :initarg :repeat :initform NIL :accessor harmony:repeat)
   (repeat-start :initarg :repeat-start :initform 0 :accessor harmony:repeat-start)
   (min-distance :initarg :min-distance :initform NIL :accessor mixed:min-distance)
   (max-distance :initarg :max-distance :initform NIL :accessor mixed:max-distance)
   (volume :initarg :volume :initform 1.0 :accessor mixed:volume)))

(defmethod trial:allocate ((voice voice))
  (setf (voice voice) (harmony:create (source voice)
                                      :class (voice-class voice)
                                      :effects (effects voice) :on-end :disconnect
                                      :repeat (harmony:repeat voice) :repeat-start (harmony:repeat-start voice)
                                      :volume (mixed:volume voice) :mixer (mixer voice))))

(defmethod trial:deallocate ((voice voice))
  (if (harmony:chain (voice voice))
      (harmony:with-server ()
        (mixed:free (voice voice)))
      (mixed:free (voice voice)))
  (setf (voice voice) NIL))

(defmethod trial:allocated-p ((voice voice))
  (not (null (voice voice))))

(defmethod harmony:play ((voice voice) &key reset location velocity (volume (mixed:volume voice)) (min-distance (mixed:min-distance voice)) (max-distance (mixed:max-distance voice)))
  (let ((voice (or (voice voice)
                   (error "Voice has not been allocated.")))
        (sources (harmony:segment :sources harmony:*server*))
        (mixer (harmony:segment (mixer voice) harmony:*server*)))
    (setf (mixed:volume voice) volume)
    (when reset
      (mixed:seek voice 0))
    (when (or (null (harmony:chain voice)) location velocity min-distance max-distance)
      (harmony:with-server (harmony:*server* :synchronize NIL)
        (unless (harmony:chain voice)
          (mixed:add voice sources)
          (harmony:connect voice T mixer T))
        (when location (setf (mixed:location voice) location))
        (when velocity (setf (mixed:velocity voice) velocity))
        (when min-distance (setf (mixed:min-distance voice) (float min-distance 0f0)))
        (when max-distance (setf (mixed:max-distance voice) (float max-distance 0f0)))))
    voice))

(defmethod harmony:stop ((resource trial:placeholder-resource)))
(defmethod mixed:volume ((resource trial:placeholder-resource)) 1.0)
(defmethod (setf mixed:volume) (volume (resource trial:placeholder-resource)) volume)

(defmethod harmony:stop ((voice voice))
  (harmony:stop (voice voice)))

(defmethod (setf mixed:volume) :after (volume (voice voice))
  (when (voice voice)
    (setf (mixed:volume (voice voice)) volume)))

(defmethod (setf mixed:min-distance) :after (min-distance (voice voice))
  (when (voice voice)
    (setf (mixed:min-distance (voice voice)) (float min-distance 0f0))))

(defmethod (setf mixed:max-distance) :after (max-distance (voice voice))
  (when (voice voice)
    (setf (mixed:max-distance (voice voice)) (float max-distance 0f0))))

(defmethod (setf mixed:rolloff) :after (rolloff (voice voice))
  (when (voice voice)
    (setf (mixed:rolloff (voice voice)) (float rolloff 0f0))))

(defmethod mixed:location ((voice voice))
  (mixed:location (voice voice)))

(defmethod (setf mixed:location) (location (voice voice))
  (let ((voice (voice voice)))
    (when (harmony:chain voice)
      (setf (mixed:location voice) location))
    location))

(defmethod (setf mixed:location) ((location math:vec2) (voice harmony:voice))
  (let ((list (make-array 2 :element-type 'single-float)))
    (declare (dynamic-extent list))
    (setf (aref list 0) (math:vx2 location))
    (setf (aref list 1) (math:vy2 location))
    (setf (mixed:location voice) list)))

(defmethod (setf mixed:location) ((location math:vec3) (voice harmony:voice))
  (let ((list (make-array 3 :element-type 'single-float)))
    (declare (dynamic-extent list))
    (setf (aref list 0) (math:vx3 location))
    (setf (aref list 1) (math:vy3 location))
    (setf (aref list 2) (math:vy3 location))
    (setf (mixed:location voice) list)))

(defmethod mixed:velocity ((voice voice))
  (mixed:velocity (voice voice)))

(defmethod (setf mixed:velocity) (velocity (voice voice))
  (let ((voice (voice voice)))
    (when (harmony:chain voice)
      (setf (mixed:velocity voice) velocity))
    velocity))

(defmethod (setf mixed:velocity) ((velocity math:vec2) (voice harmony:voice))
  (let ((list (make-array 2 :element-type 'single-float)))
    (declare (dynamic-extent list))
    (setf (aref list 0) (math:vx2 velocity))
    (setf (aref list 1) (math:vy2 velocity))
    (setf (mixed:velocity voice) list)))

(defmethod (setf mixed:velocity) ((velocity math:vec3) (voice harmony:voice))
  (let ((list (make-array 3 :element-type 'single-float)))
    (declare (dynamic-extent list))
    (setf (aref list 0) (math:vx3 velocity))
    (setf (aref list 1) (math:vy3 velocity))
    (setf (aref list 2) (math:vy3 velocity))
    (setf (mixed:velocity voice) list)))

(defmethod mixed:done-p ((voice voice))
  (mixed:done-p (voice voice)))

(defmethod mixed:duration ((voice voice))
  (mixed:duration (voice voice)))

(defmethod mixed:seek ((voice voice) position &rest args)
  (apply #'mixed:seek (voice voice) position args))

(defmethod harmony:segment (segment (voice voice) &optional (errorp T))
  (harmony:segment segment (voice voice) errorp))

(defmethod (setf mixed:location) ((location math:vec2) (server harmony:server))
  (let ((list (make-array 2 :element-type 'single-float)))
    (declare (dynamic-extent list))
    (setf (aref list 0) (math:vx2 location))
    (setf (aref list 1) (math:vy2 location))
    (setf (mixed:location server) list)))

(defmethod (setf mixed:location) ((location math:vec3) (server harmony:server))
  (let ((list (make-array 3 :element-type 'single-float)))
    (declare (dynamic-extent list))
    (setf (aref list 0) (math:vx3 location))
    (setf (aref list 1) (math:vy3 location))
    (setf (aref list 2) (math:vz3 location))
    (setf (mixed:location server) list)))

(defmethod harmony:transition ((voice voice) to &rest args &key &allow-other-keys)
  (apply #'harmony:transition (voice voice) to args))

(defmethod trial:clone ((voice voice) &key)
  (error "FIXME: implement this."))
