#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass achievement ()
  ((name :initform (error "NAME required") :initarg :name :accessor name)
   (api-name :initform NIL :initarg :api-name :accessor api-name)
   (icon :initform NIL :initarg :icon :accessor icon)
   (unlocked-p :initform NIL :initarg :unlocked-p :accessor unlocked-p)))

(defmethod shared-initialize :after ((achievement achievement) slots &key name)
  (when name (setf (api-name achievement) (symbol->c-name name))))

(defmethod title ((achievement achievement))
  (language-string (name achievement)))

(defmethod description ((achievement achievement))
  (language-string (mksym (symbol-package (name achievement)) (name achievement) '/description)))

(defclass achievement-container ()
  ())

(defgeneric list-achievements (main))
(defgeneric achievement-state (achievement container))
(defgeneric (setf achievement-state) (value achievement container))

(defmethod achievement-state ((name symbol) (container achievement-container))
  (handler-case (achievement-state (or (find name (list-achievements container) :key #'name)
                                       (error "No achievement named ~s found!" name))
                                   container)
    #+trial-release
    (error (e)
      (v:error :trial.achievements "Error retrieving achievement state: ~a" e)
      NIL)))

(defmethod (setf achievement-state) (value (name symbol) (container achievement-container))
  (handler-case (setf (achievement-state (or (find name (list-achievements container) :key #'name)
                                             (error "No achievement named ~s found!" name))
                                         container)
                      value)
    #+trial-release
    (error (e)
      (v:error :trial.achievements "Error setting achievement state: ~a" e)
      value)))

(defmethod achievement-state ((achievement achievement) container)
  (unlocked-p achievement))

(defmethod (setf achievement-state) (value (achievement achievement) container)
  (setf (unlocked-p achievement) value))

(defmethod (setf achievement-state) :after (value (achievement achievement) (main main))
  (handle (make-instance 'achievement-changed :achievement achievement) main))

(defclass achievement-changed (event)
  ((achievement :initform (error "ACHIEVEMENT required") :initarg :achievement :reader achievement)))

(defmacro define-achievements (name achievement-superclass &body achievements)
  `(let ((achievements (list ,@(loop for (name . initargs) in achievements
                                     collect `(make-instance ',achievement-superclass :name ',name ,@initargs)))))
     (unless (find-class ',name)
       (defclass ,name ()
         ()))

     (defmethod list-achievements ((_ ,name))
       achievements)

     ;; FIXME: integrate with save state stuff, once that's sorted out.
     ))
