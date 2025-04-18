(in-package #:org.shirakumo.fraf.trial.harmony)

(defgeneric setup-server (main server)
  (:method-combination progn :most-specific-last))

(defmethod setup-server progn (main server))

(defun channel-order (drain)
  (let ((order (mixed:channel-order drain)))
    (if (< (mixed:channels drain) (length order))
        (subseq order 0 (mixed:channels drain))
        order)))

(defun try-audio-backend (main backend &rest initargs)
  (handler-bind ((mixed:device-not-found
                   (lambda (e)
                     (v:error :trial.harmony "~a" e)
                     (continue e))))
    (let ((server (apply #'harmony:make-simple-server :name trial:+app-system+ :drain backend initargs)))
      (trial:with-cleanup-on-failure (mixed:free server)
        (setup-server main server)
        (mixed:start server))))
  (let ((drain (harmony:segment :drain (harmony:segment :output T))))
    (v:info :trial.harmony "Configured output for ~s~@[ on ~a~]: ~d ~a channels ~aHz.~%  Channel layout is ~a"
            (type-of drain) (when (typep drain 'mixed:device-drain) (mixed:device drain))
            (mixed:channels drain) (mixed:encoding drain) (mixed:samplerate drain)
            (channel-order drain))
    (when (typep drain 'mixed:device-drain)
      (v:info :trial.harmony "Device list:~{~%  ~a~}" (mixed:list-devices drain)))
    drain))

(defun initialize-audio-backend (main &optional preferred-backend &rest initargs)
  (or (when preferred-backend
        (trial:with-ignored-errors-on-release (:trial.harmony "Failed to set up ~a, falling back to default output." preferred-backend)
          (apply #'try-audio-backend main preferred-backend initargs)))
      (trial:with-ignored-errors-on-release (:trial.harmony "Failed to set up sound, falling back to dummy output.")
        (apply #'try-audio-backend main :default initargs))
      (apply #'try-audio-backend main :dummy initargs)))

(defmethod trial:finalize ((server harmony:server))
  (trial:with-ignored-errors-on-release (:trial.harmony)
    (mixed:free server))
  (trial:with-ignored-errors-on-release (:trial.harmony)
    (mixed:shutdown)))

(defmethod (setf mixed:device) :after (device (server harmony:server))
  (let ((drain (harmony:segment :drain (harmony:segment :output server))))
    (v:info :trial.harmony "Configured output for ~s~@[ on ~a~]: ~d ~a channels ~aHz.~%  Channel layout is ~a"
            (type-of drain) (mixed:device drain)
            (mixed:channels drain) (mixed:encoding drain) (mixed:samplerate drain)
            (channel-order drain))))

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

(defclass main (trial:main)
  ((initialize-audio :initarg :initialize-audio :initform T :accessor initialize-audio)))

(defgeneric server-initargs (main)
  (:method-combination append :most-specific-first))

(defmethod server-initargs append ((main main))
  ())

(defmethod initialize-instance ((main main) &key audio-backend)
  (call-next-method)
  (when (initialize-audio main)
    (apply #'initialize-audio-backend main audio-backend (server-initargs main))))

(defmethod trial:finalize :after ((main main))
  (when harmony:*server*
    (trial:finalize harmony:*server*))
  (setf harmony:*server* NIL))

(defclass settings-main (main)
  ()
  (:default-initargs
   :audio-backend (trial:setting :audio :backend)))

(defmethod initialize-instance :after ((main settings-main) &key)
  (when harmony:*server*
    (loop for (k v) on (trial:setting :audio :volume) by #'cddr
          for segment = (harmony:segment k harmony:*server* NIL)
          do (if segment
                 (setf (mixed:volume segment) v)
                 (v:warn :trial.harmony "Can't set volume for inexistent segment ~s" k)))))

(defmethod server-initargs append ((main settings-main))
  (list :latency (trial:setting :audio :latency)
        :device (trial:setting :audio :device)))

#+nx
(defmethod harmony:run :before ((server harmony:server))
  (setf (org.shirakumo.machine-state:thread-core-mask T) #b0100))
