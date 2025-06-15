(in-package #:org.shirakumo.fraf.trial.harmony)

(defmethod trial:load-audio ((source pathname) (type symbol) &key generator (voice-class 'harmony:voice) (mixer :effect) effects repeat (repeat-start 0) (volume 1.0) (resource (trial:resource generator T)) max-distance min-distance)
  (trial::ensure-instance resource 'voice
                          :voice-class voice-class
                          :mixer mixer :source source :effects effects :volume volume
                          :max-distance max-distance :min-distance min-distance
                          :repeat repeat :repeat-start repeat-start))

(defmethod trial:reload ((sound trial:audio-file))
  (let ((resource (trial:resource sound T)))
    (when (and (typep resource 'voice)
               (voice resource))
      (mixed:end (harmony:source (voice resource)))
      (mixed:start (harmony:source (voice resource))))))

(defmethod harmony:play ((file trial:sound-bank) &rest args)
  (let ((chance 8f0)
        (resources (slot-value file 'trial::resources)))
    (when (= 0 (hash-table-count resources))
      (error "Sound bank has no resources."))
    (loop (loop for resource being the hash-values of resources
                do (when (or (< chance 1) (and (< (random chance) 1) (trial:done-p resource)))
                     (return-from harmony:play (apply #'harmony:play resource args))))
          (setf chance (* 0.5f0 chance)))))

(defclass sound (trial:audio-file)
  ())

(defclass environment (trial:single-resource-asset trial:compiled-generator)
  ())

(defmethod trial:compile-resources ((generator environment) sets &rest args)
  (dolist (set sets)
    (dolist (track (cdr set))
      (let ((target (first track)))
        (apply #'trial:compile-resources generator target args)))))

(defmethod trial:generate-resources ((generator environment) sets &key (resource (trial:resource generator T)))
  (if harmony:*server*
      (if (typep resource 'music)
          resource
          (trial::ensure-instance resource 'music :sets sets))
      (trial::ensure-instance resource 'music :sets ())))

(defmethod trial:reload ((asset environment)))

(defmethod trial:unload ((environment environment)))

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
  (when harmony:*server*
    (setf (voice voice) (harmony:create (source voice)
                                        :class (voice-class voice)
                                        :effects (effects voice) :on-end :disconnect
                                        :repeat (harmony:repeat voice) :repeat-start (harmony:repeat-start voice)
                                        :volume (mixed:volume voice) :mixer (mixer voice)))))

(defmethod trial:deallocate ((voice voice))
  (if (harmony:chain (voice voice))
      (harmony:with-server ()
        (mixed:free (voice voice)))
      (mixed:free (voice voice)))
  (setf (voice voice) NIL))

(defmethod trial:allocated-p ((voice voice))
  (not (null (voice voice))))

(defmethod trial:unload ((voice voice))
  (when (trial:allocated-p voice)
    (trial:deallocate voice)))

(defmethod harmony:play ((voice voice) &key reset location velocity (volume (mixed:volume voice)) (min-distance (mixed:min-distance voice)) (max-distance (mixed:max-distance voice)))
  (let ((voice (or (voice voice)
                   #-elide-allocation-checks (error "Voice has not been allocated.")
                   #+elide-allocation-checks (return-from harmony:play voice)))
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
  (when (voice voice)
    (harmony:stop (voice voice))))

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

(defmethod (setf mixed:location) ((location math:vec2) thing)
  (setf (mixed:location thing) (math:varr2 location)))

(defmethod (setf mixed:location) ((location math:vec3) thing)
  (setf (mixed:location thing) (math:varr3 location)))

(defmethod (setf mixed:direction) ((direction math:vec3) thing)
  (setf (mixed:direction thing) (math:varr3 direction)))

(defmethod (setf mixed:up) ((up math:vec3) thing)
  (setf (mixed:up thing) (math:varr3 up)))

(defmethod mixed:velocity ((voice voice))
  (mixed:velocity (voice voice)))

(defmethod (setf mixed:velocity) (velocity (voice voice))
  (let ((voice (voice voice)))
    (when (harmony:chain voice)
      (setf (mixed:velocity voice) velocity))
    velocity))

(defmethod (setf mixed:velocity) ((velocity math:vec2) thing)
  (setf (mixed:velocity thing) (math:varr2 velocity)))

(defmethod (setf mixed:velocity) ((velocity math:vec3) thing)
  (setf (mixed:velocity thing) (math:varr3 velocity)))

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
  (apply #'harmony:transition (or (voice voice)
                                  #-elide-allocation-checks (error "Voice has not been allocated.")
                                  #+elide-allocation-checks (return-from harmony:transition voice))
         to args))

(defmethod trial:location ((voice voice))
  (math:vec3 (mixed:location (voice voice))))

(defmethod (setf trial:location) ((location math:vec3) (voice voice))
  (setf (mixed:location voice) (math:varr3 location)))

(defmethod trial:velocity ((voice voice))
  (math:vec3 (mixed:velocity (voice voice))))

(defmethod (setf trial:velocity) ((velocity math:vec3) (voice voice))
  (setf (mixed:velocity voice) (math:varr3 velocity)))

(defmethod trial:play ((voice voice) target)
  (harmony:play voice :location target))

(defmethod trial:play ((voice voice) (target trial:entity))
  (let ((vec (math:vec3)))
    (trial:global-location target vec)
    (harmony:play voice :location (math:varr3 vec))))

(defmethod trial:duration ((voice voice))
  (mixed:duration (voice voice)))

(defmethod trial:stop ((voice voice))
  (harmony:stop (voice voice)))

(defmethod trial:done-p ((voice voice))
  ;; This means something slightly different to mixed:done-p
  (not (harmony:active-p (voice voice))))

(defmethod trial::volume ((voice voice))
  (mixed:volume (voice voice)))

(defmethod (setf trial::volume) (value (voice voice))
  (setf (mixed:volume (voice voice)) value))

(defmethod trial:seek ((voice voice) to)
  (mixed:seek (voice voice) to :by :second))

(defmethod trial:fade-to (volume (voice voice) &rest args &key &allow-other-keys)
  (apply #'harmony:transition (voice voice) volume args))

(defmethod trial:clone ((voice voice) &key)
  (error "FIXME: implement this."))
