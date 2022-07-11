#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass entity (unit)
  ((container :accessor container)
   (name :accessor name)))

(defmethod enter :after ((entity entity) (container flare:container))
  (setf (container entity) container))

(defmethod leave :after ((entity entity) (container flare:container))
  (slot-makunbound entity 'container))

(defmethod leave ((entity entity) (container (eql T)))
  (leave entity (container entity)))

(defmethod (setf name) :around (name (entity entity))
  (unless (eq name (name entity))
    (let ((scene (scene entity)))
      (cond (scene
             (deregister entity scene)
             (call-next-method)
             (register entity scene))
            (T
             (call-next-method))))))

#-elide-container-checks
(defmethod enter :before ((entity entity) (container flare:container))
  (when (slot-boundp entity 'container)
    (error "The entity~%  ~a~%cannot be entered into~%  ~a~%as it is already contained in~%  ~a"
           entity container (container entity))))

#-elide-container-checks
(defmethod leave :before ((entity entity) (container flare:container))
  (unless (and (slot-boundp entity 'container)
               (eq container (container entity)))
    (error "The entity~%  ~a~%cannot be left from~%  ~a~%as it is contained in~%  ~a"
           entity container (container entity))))

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

(defclass container (flare:container-unit entity)
  ())

(defmethod preceding-entity ((thing entity) (container flare:container))
  (multiple-value-bind (last valid-p) (flare-indexed-set:set-last (objects container))
    (when valid-p
      (let ((last (flare-queue:value last)))
        (unless (eq last thing) last)))))

(defmethod enter* ((thing entity) (container container))
  (enter thing container)
  (compile-into-pass thing container (scene +main+)))

(defmethod leave* ((thing entity) (container (eql T)))
  (when (slot-boundp thing 'container)
    (leave* thing (container thing))))

(defmethod leave* ((thing entity) (container container))
  (leave thing container)
  (remove-from-pass thing (scene +main+)))
