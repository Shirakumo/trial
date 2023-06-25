#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene (bag event-loop)
  ((camera :initarg :camera :initform NIL :accessor camera)
   (name-map :initform (make-hash-table :test 'equal) :accessor name-map)))

(defmethod enter :after ((camera camera) (scene scene))
  (setf (camera scene) camera))

(defmethod unit (name (scene scene))
  (gethash name (name-map scene)))

(defmethod node (name (scene scene))
  (gethash name (name-map scene)))

(defmethod node (name (scene (eql T)))
  (gethash name (name-map (scene +main+))))

(defmethod register ((entity entity) (scene scene))
  (when (name entity)
    (setf (gethash (name entity) (name-map scene)) entity))
  entity)

(defmethod register :after ((listener listener) (scene scene))
  (add-listener listener scene))

(defmethod deregister ((entity entity) (scene scene))
  (remhash (name entity) (name-map scene))
  entity)

(defmethod deregister :after ((listener listener) (scene scene))
  (remove-listener listener scene))

(defmethod scene ((scene scene)) scene)

(defmethod apply-transforms progn ((scene scene)))
