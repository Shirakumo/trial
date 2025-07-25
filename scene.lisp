(in-package #:org.shirakumo.fraf.trial)

(define-event scene-change () changed-node)
(define-event register (scene-change))
(define-event deregister (scene-change))

(defclass scene (bag event-loop)
  ((camera :initarg :camera :initform NIL :accessor camera)
   (name-map :initform (make-hash-table :test 'equal) :accessor name-map)))

(defmethod enter ((camera camera) (scene scene))
  (register camera scene)
  (unless (camera scene)
    (setf (camera scene) camera)))

(defmethod leave ((camera camera) (scene scene))
  (deregister camera scene)
  (when (eq camera (camera scene))
    (setf (camera scene) NIL)))

(defmethod (setf camera) :around ((camera camera) (scene scene))
  (let ((old (camera scene)))
    (when old (v<- (bsize camera) (bsize old))))
  (call-next-method)
  (setup-perspective camera T T)
  camera)

(defmethod handle :after ((tick post-tick) (scene scene))
  (when (camera scene)
    (project-view (camera scene))))

(defmethod handle :after ((event resize) (scene scene))
  (when (camera scene)
    (setup-perspective (camera scene) T T)))

(defmethod unit (name (scene scene))
  (gethash name (name-map scene)))

(trivial-deprecate:declaim-deprecated (function unit)
                                      :software "trial"
                                      :version "1.2.0"
                                      :alternatives (node))

(defmethod node (name (scene scene))
  (gethash name (name-map scene)))

(defmethod node (name (scene (eql T)))
  (when (and +main+ (slot-boundp +main+ 'scene))
    (gethash name (name-map (scene +main+)))))

(defmethod register ((entity entity) (scene scene))
  (when (name entity)
    (setf (gethash (name entity) (name-map scene)) entity))
  (issue scene 'register :changed-node entity)
  entity)

(defmethod register ((listener listener) (scene scene))
  (when (next-method-p) (call-next-method)))

(defmethod register :after ((listener listener) (scene scene))
  (add-listener listener scene))

(defmethod deregister ((entity entity) (scene scene))
  (remhash (name entity) (name-map scene))
  (issue scene 'deregister :changed-node entity)
  entity)

(defmethod deregister ((listener listener) (scene scene))
  (when (next-method-p) (call-next-method)))

(defmethod deregister :after ((listener listener) (scene scene))
  (remove-listener listener scene))

(defmethod scene ((scene scene)) scene)

(defmethod apply-transforms progn ((scene scene)))

(defun ensure-entity (name container &optional (class name) &rest initargs)
  (let ((existing (node name container)))
    (if existing
        (apply #'reinitialize-instance existing initargs)
        (let ((object (apply #'make-instance class :name name initargs)))
          (enter object container)
          object))))
