#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene (entity event-loop)
  ((subjects :initform () :accessor subjects)))

(defmethod add-subject ((subject subject) (scene scene))
  (add-handler subject scene)
  (push subject (subjects scene)))

(defmethod remove-subject ((subject subject) (scene scene))
  (remove-handler subject scene)
  (setf (subjects scene) (delete subject (subjects scene))))

(defmethod finalize ((scene scene))
  (mapc #'finalize (subjects scene)))

(defmethod draw ((scene scene))
  (mapc #'draw (subjects scene)))
