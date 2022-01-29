#|
 This file is a part of trial
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.harmony)

(defclass main (trial:main)
  ())

(defgeneric server-initargs (main)
  (:method-combination append :most-specific-last))

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

(defmethod trial:finalize :after ((main main))
  (when harmony:*server*
    (trial:finalize harmony:*server*))
  (setf harmony:*server* NIL))

(defmethod trial:finalize ((server harmony:server))
  (mixed:free server))
