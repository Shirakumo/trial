#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass resource-generator ()
  ((observers :initform (tg:make-weak-hash-table :weakness :key) :accessor observers)))

(defgeneric generate-resources (generator input &key &allow-other-keys))
(defgeneric register-generation-observer (observer generator))
(defgeneric clear-observers (generator))
(defgeneric observe-generation (observer generator result))
(defgeneric resource (generator identifier))

(defmethod register-generation-observer (observer (generator resource-generator))
  (setf (gethash observer (observers generator)) T))

(defmethod clear-observers ((generator resource-generator))
  (clrhash (observers generator)))

(defmethod generate-resources :around ((generator resource-generator) input &key)
  (let ((result (call-next-method)))
    (loop for observer being the hash-keys of (observers generator)
          do (observe-generation observer generator result))
    result))

(defmethod generate-resources ((generator symbol) input &rest args)
  (apply #'generate-resources (make-instance generator) input args))
