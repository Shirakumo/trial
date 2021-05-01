#|
 This file is a part of trial
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.harmony)

(defclass sound-loader (trial:resource-generator)
  ())

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
