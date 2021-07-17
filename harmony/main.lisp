#|
 This file is a part of trial
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.harmony)

(defclass main (trial:main)
  ())

(defmethod initialize-instance ((main main) &key (audio-latency 0.05))
  (call-next-method)
  (mixed:start (harmony:make-simple-server :name trial:+app-system+ :latency audio-latency)))

(defmethod trial:finalize :after ((main main))
  (when harmony:*server*
    (trial:finalize harmony:*server*))
  (setf harmony:*server* NIL))

(defmethod trial:finalize ((server harmony:server))
  (mixed:free server))
