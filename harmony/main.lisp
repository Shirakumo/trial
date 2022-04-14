#|
 This file is a part of trial
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
           (mixed:start (apply #'harmony:make-simple-server
                               :name trial:+app-system+ :drain drain (server-initargs main)))))
    (handler-case (trial:with-error-logging (:trial.harmony "Failed to set up sound, falling back to dummy output.")
                    (start (or audio-backend :default)))
      (error () (start :dummy)))))

(defmethod trial:finalize :before ((main main))
  (when harmony:*server*
    (trial:finalize harmony:*server*))
  (setf harmony:*server* NIL))

(defmethod trial:finalize ((server harmony:server))
  (mixed:free server))

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
    (let* ((seg (harmony:segment :drain (harmony:segment :output T)))
           (prev (mixed:device seg))
           (success NIL))
      (harmony:with-server (harmony:*server* :synchronize T)
        (handler-case
            (progn (setf (mixed:device seg) value)
                   (setf success T))
          (error ()
            (setf (mixed:device seg) prev))))
      (unless success
        (setf (trial:setting :audio :device) prev)))))
