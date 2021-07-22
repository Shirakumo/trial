#|
 This file is a part of trial
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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

(defmethod trial:compile-resources ((generator sound-loader) path &key (samplerate 44100) (sample-type :int16))
  (when (string= "wav" (pathname-type path))
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
        (rename-file temp path)))))

(defmethod trial:generate-resources ((generator sound-loader) path &key (mixer :effect) effects repeat (repeat-start 0) (volume 1.0) (resource (trial:resource generator T)))
  (trial::ensure-instance resource 'voice
                          :mixer mixer :source path :effects effects :volume volume
                          :repeat repeat :repeat-start repeat-start))

(defclass sound (trial:single-resource-asset trial:file-input-asset sound-loader)
  ())

(defmethod trial:reload ((sound sound))
  (let ((resource (trial:resource sound T)))
    (when (and (typep resource 'voice)
               (voice resource))
      (mixed:end (harmony:source (voice resource)))
      (mixed:start (harmony:source (voice resource))))))

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
                            ((string-equal "flac" (pathname-type target)) "flac")
                            ((string-equal "mp3" (pathname-type target)) "libmp3lame")
                            ((string-equal "opus" (pathname-type target)) "libopus")
                            (T (error "Unsupported file type ~s" (pathname-type target))))
               "-ar" samplerate
               "-qscale:a" quality
               "-y" target))))))

(defmethod trial:generate-resources ((generator environment-loader) sets &key (resource (trial:resource generator T)))
  (trial::ensure-instance resource 'music :sets sets))

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
                                collect (list* (trial:coerce-asset-input asset input) params)))))

(defclass music (trial:resource harmony:environment)
  ())

(defmethod trial:allocated-p ((music music)) T)

(defmethod trial:allocate ((music music)) music)

(defmethod trial:deallocate ((music music))
  (mixed:free music)
  (change-class music 'trial:placeholder-resource))

;; KLUDGE: This cannot be a harmony:voice since it does not handle the change-class/reinitialize-instance
;;         protocol we have going for voices gracefully. It would also cause allocation to happen at
;;         initialisation rather than at, well, allocation time.
(defclass voice (trial:resource)
  ((voice :initform NIL :accessor voice)
   (mixer :initarg :mixer :accessor mixer)
   (source :initarg :source :accessor source)
   (effects :initarg :effects :accessor effects)
   (repeat :initarg :repeat :accessor repeat)
   (repeat-start :initarg :repeat-start :accessor repeat-start)
   (volume :initarg :volume :accessor volume)))

(defmethod trial:allocate ((voice voice))
  (setf (voice voice) (harmony:create (source voice)
                                      :effects (effects voice) :on-end :disconnect
                                      :repeat (repeat voice) :repeat-start (repeat-start voice)
                                      :volume (volume voice) :mixer (mixer voice))))

(defmethod trial:deallocate ((voice voice))
  (if (harmony:chain (voice voice))
      (harmony:with-server ()
        (mixed:free (voice voice)))
      (mixed:free (voice voice)))
  (setf (voice voice) NIL))

(defmethod trial:allocated-p ((voice voice))
  (not (null (voice voice))))

(defun ensure-vector (vec)
  (etypecase vec
    (list
     vec)
    (3d-vectors:vec2
     (list (3d-vectors:vx2 vec) (3d-vectors:vy2 vec) 0))
    (3d-vectors:vec3
     (list (3d-vectors:vx3 vec) (3d-vectors:vy3 vec) (3d-vectors:vz3 vec)))))

(defmethod harmony:play ((voice voice) &key reset location velocity volume)
  (let ((voice (or (voice voice)
                   (error "Voice has not been allocated.")))
        (sources (harmony:segment :sources harmony:*server*))
        (mixer (harmony:segment (mixer voice) harmony:*server*))
        (location (ensure-vector location))
        (velocity (ensure-vector velocity))
        (volume (or volume (volume voice))))
    ;; KLUDGE: possible race here.
    (unless (harmony:chain voice)
      (setf (mixed:volume voice) volume)
      (when reset
        (mixed:seek voice 0))
      (harmony:with-server (harmony:*server* :synchronize NIL)
        (unless (harmony:chain voice)
          (mixed:add voice sources)
          (harmony:connect voice T mixer T))
        (when location (setf (mixed:location voice) location))
        (when velocity (setf (mixed:velocity voice) velocity))))
    voice))

(defmethod harmony:stop ((resource trial:placeholder-resource)))
(defmethod mixed:volume ((resource trial:placeholder-resource)) 1.0)
(defmethod (setf mixed:volume) (volume (resource trial:placeholder-resource)) volume)

(defmethod harmony:stop ((voice voice))
  (harmony:stop (voice voice)))

(defmethod mixed:volume ((voice voice))
  (mixed:volume (voice voice)))

(defmethod (setf mixed:volume) (volume (voice voice))
  (setf (mixed:volume (voice voice)) volume))

(defmethod mixed:location ((voice voice))
  (mixed:location (voice voice)))

(defmethod (setf mixed:location) (location (voice voice))
  (setf (mixed:location (voice voice)) (ensure-vector location)))

(defmethod mixed:velocity ((voice voice))
  (mixed:velocity (voice voice)))

(defmethod (setf mixed:velocity) (velocity (voice voice))
  (setf (mixed:velocity (voice voice)) (ensure-vector velocity)))

(defmethod mixed:done-p ((voice voice))
  (mixed:done-p (voice voice)))

(defmethod mixed:seek ((voice voice) position &rest args)
  (apply #'mixed:seek (voice voice) position args))

(defmethod harmony:segment (segment (voice voice) &optional (errorp T))
  (harmony:segment segment (voice voice) errorp))
