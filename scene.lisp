(in-package #:org.shirakumo.fraf.trial)

(define-event scene-change () changed-node)
(define-event register (scene-change))
(define-event deregister (scene-change))

(defclass scene (bag event-loop)
  ((camera :initarg :camera :initform NIL :accessor camera)
   (name-map :initform (make-hash-table :test 'equal) :accessor name-map)))

(defmethod enter :after ((camera camera) (scene scene))
  (setf (camera scene) camera))

(defmethod unit (name (scene scene))
  (gethash name (name-map scene)))

(trivial-deprecate:declaim-deprecated (function unit)
                                      :software "trial"
                                      :version "1.2.0"
                                      :alternatives (node))

(defmethod node (name (scene scene))
  (gethash name (name-map scene)))

(defmethod node (name (scene (eql T)))
  (gethash name (name-map (scene +main+))))

(defmethod register ((entity entity) (scene scene))
  (when (name entity)
    (setf (gethash (name entity) (name-map scene)) entity))
  (issue scene 'register :changed-node entity)
  entity)

(defmethod register :after ((listener listener) (scene scene))
  (add-listener listener scene))

(defmethod deregister ((entity entity) (scene scene))
  (remhash (name entity) (name-map scene))
  (issue scene 'deregister :changed-node entity)
  entity)

(defmethod deregister :after ((listener listener) (scene scene))
  (remove-listener listener scene))

(defmethod scene ((scene scene)) scene)

(defmethod apply-transforms progn ((scene scene)))

(defun ensure-entity (name container &optional (class name) &rest initargs)
  (or (node name container)
      (enter (apply #'make-instance class :name name initargs) container)))
