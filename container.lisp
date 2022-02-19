#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defgeneric clear (container))
(defgeneric enter (thing container))
(defgeneric leave (thing conatiner))
(defgeneric register (thing container))
(defgeneric deregister (thing conatiner))
(defgeneric contains-p (thing conatiner))

(defclass scene-node ()
  ((container :initarg :container :initform NIL :accessor container)))

(defgeneric root (node))

(defmethod root ((node scene-node))
  (if (null (container node))
      node
      (root (container node))))

(defmethod leave ((node scene-node) (container (eql T)))
  (when (container node)
    (leave node (container node))))

(defclass container (scene-node)
  ())

(defmethod register ((node scene-node) (null null)))

(defmethod deregister ((node scene-node) (null null)))

(defmethod register ((node scene-node) (container container))
  (register node (container container)))

(defmethod deregister ((node scene-node) (container container))
  (deregister node (container container)))

(defmethod enter :after ((node scene-node) (container container))
  (setf (container node) container))

(defmethod leave :after ((node scene-node) (container container))
  (setf (container node) NIL))

#-elide-container-checks
(defmethod enter :before ((node scene-node) (container container))
  (when (container node)
    (error "The node~%  ~a~%cannot be entered into~%  ~a~%as it is already contained in~%  ~a"
           node container (container entity))))

#-elide-container-checks
(defmethod leave :before ((entity entity) (container container))
  (when (and (container entity) (not (eq container (container entity))))
    (error "The entity~%  ~a~%cannot be left from~%  ~a~%as it is contained in~%  ~a"
           entity container (container entity))))

(defmethod finalize ((container container))
  (for:for ((object over container))
    (finalize object))
  (clear container))

(defclass entity (scene-node)
  ((name :initform NIL :initarg :name :accessor name)))

(defmethod enter :after ((entity entity) (container container))
  (register entity container))

(defmethod leave :after ((node entity) (container container))
  (deregister entity container))

(defmethod (setf name) :around (name (entity entity))
  (unless (eq name (name entity))
    (let ((root (root entity)))
      (cond ((eq entity root)
             (call-next-method))
            (T
             (deregister entity root)
             (call-next-method)
             (register entity root))))))

(defmethod enter* ((thing entity) (container container))
  (enter thing container)
  (compile-into-pass thing container (scene +main+)))

(defmethod leave* ((thing entity) (container (eql T)))
  (when (container thing)
    (leave* thing (container thing))))

(defmethod leave* ((thing entity) (container container))
  (leave thing container)
  (remove-from-pass thing (scene +main+)))

(defmethod scene ((entity entity))
  (when (slot-boundp entity 'container)
    (scene (container entity))))

(defmethod clone ((entity entity) &rest initargs)
  (let ((initvalues ()))
    (loop for initarg in (initargs entity)
          for slot = (initarg-slot (class-of entity) initarg)
          do (when slot
               (push (clone (slot-value entity (c2mop:slot-definition-name slot))) initvalues)
               (push initarg initvalues)))
    (apply #'make-instance (class-of entity) (append initargs initvalues (when (name entity) (list :name (generate-name (type-of entity))))))))
