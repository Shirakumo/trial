(in-package #:org.shirakumo.fraf.trial.harmony)

(defclass main (trial:main)
  ())

(defgeneric server-initargs (main)
  (:method-combination append :most-specific-first))

(defmethod server-initargs append ((main main))
  ())

(defmethod initialize-instance ((main main) &key audio-backend)
  (call-next-method)
  (flet ((start (drain)
           (handler-bind ((mixed:device-not-found
                            (lambda (e)
                              (v:error :trial.harmony "~a" e)
                              (continue e))))
             (mixed:start (apply #'harmony:make-simple-server
                                 :name trial:+app-system+ :drain drain (server-initargs main))))
           (let ((drain (harmony:segment :drain (harmony:segment :output T))))
             (v:info :trial.harmony "Configured output for ~s~@[ on ~a~]: ~d ~a channels ~aHz.~%  Channel layout is ~a"
                     (type-of drain) (when (typep drain 'mixed:device-drain) (mixed:device drain))
                     (mixed:channels drain) (mixed:encoding drain) (mixed:samplerate drain)
                     (subseq (mixed:channel-order drain) 0 (mixed:channels drain)))
             (when (typep drain 'mixed:device-drain)
               (v:info :trial.harmony "Device list:~{~%  ~a~}" (mixed:list-devices drain)))
             drain)))
    (or (when audio-backend
          (ignore-errors (trial:with-error-logging (:trial.harmony "Failed to set up requested backend, falling back to default output.")
                           (start audio-backend))))
        (ignore-errors (trial:with-error-logging (:trial.harmony "Failed to set up sound, falling back to dummy output.")
                         (start :default)))
        (start :dummy))))

(defmethod trial:finalize :after ((main main))
  (when harmony:*server*
    (trial:finalize harmony:*server*))
  (setf harmony:*server* NIL))

(defmethod trial:finalize ((server harmony:server))
  (trial:with-ignored-errors-on-release (:trial.harmony)
    (mixed:free server)))

(defmethod (setf mixed:device) :after (device (server harmony:server))
  (let ((drain (harmony:segment :drain (harmony:segment :output server))))
    (v:info :trial.harmony "Configured output for ~s~@[ on ~a~]: ~d ~a channels ~aHz.~%  Channel layout is ~a"
            (type-of drain) (mixed:device drain)
            (mixed:channels drain) (mixed:encoding drain) (mixed:samplerate drain)
            (subseq (mixed:channel-order drain) 0 (mixed:channels drain)))))

(defclass settings-main (main)
  ()
  (:default-initargs
   :audio-backend (trial:setting :audio :backend)))

(defmethod initialize-instance :after ((main settings-main) &key)
  (loop for (k v) on (trial:setting :audio :volume) by #'cddr
        for segment = (harmony:segment k harmony:*server* NIL)
        do (if segment
               (setf (mixed:volume segment) v)
               (v:warn :trial.harmony "Can't set volume for inexistent segment ~s" k))))

(defmethod server-initargs append ((main settings-main))
  (list :latency (trial:setting :audio :latency)
        :device (trial:setting :audio :device)))

(trial:define-setting-observer volumes :audio :volume (value)
  (when harmony:*server*
    (loop for (k v) on value by #'cddr
          for segment = (harmony:segment k harmony:*server* NIL)
          do (if segment
                 (setf (mixed:volume segment) v)
                 (v:warn :trial.harmony "Can't set volume for inexistent segment ~s" k)))))

(trial:define-setting-observer audio-device :audio :device (value)
  (when harmony:*server*
    (when (typep (harmony:segment :drain (harmony:segment :output harmony:*server*)) 'mixed:device-drain)
      (let ((new (setf (mixed:device harmony:*server*) value)))
        (unless (equal value new)
          (setf (trial:setting :audio :device) new))))))

#+nx
(defmethod harmony:run :before ((server harmony:server))
  (setf (org.shirakumo.machine-state:thread-core-mask T) #b0100))
