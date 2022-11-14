#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *achievements* (make-hash-table :test 'eq))

(defmethod achievement ((name symbol))
  (or (gethash name *achievements*)
      (error "No achievement named ~s found." name)))

(defmethod achievement ((name string))
  (loop for achievement being the hash-keys of *achievements*
        for string = (string achievement)
        do (when (and (= (length string) (length name))
                      (loop for a across name
                            for b across string
                            always (or (char-equal a b)
                                       (char= a #\_)
                                       (char= a #\ ))))
             (return (gethash achievement *achievements*)))
        finally (error "No achievement named ~s found." name)))

(defmethod (setf achievement) (achievement (name symbol))
  (setf (gethash name *achievements*) achievement))

(defmethod (setf achievement) ((none null) (name symbol))
  (remhash name *achievements*)
  NIL)

(defun list-achievements ()
  (sort (alexandria:hash-table-values *achievements*) #'string< :key #'name))

(defclass achievement ()
  ((name :initarg :name :accessor name)
   (title :initarg :title :accessor title)
   (description :initarg :description :initform NIL :accessor description)
   (icon :initarg :icon :initform NIL :accessor icon)
   (event-type :initarg :event-type :initform NIL :accessor event-type)
   (test-function :initarg :test-function :initform (constantly NIL) :accessor test-function)
   (active-p :initform NIL :accessor active-p)))

(defmethod print-object ((achievement achievement) stream)
  (print-unreadable-object (achievement stream :type T)
    (format stream "~s ~:[LOCKED~;UNLOCKED~]" (name achievement) (active-p achievement))))

(defmacro define-achievement (name &optional event-type &body test-function)
  (destructuring-bind (name &key (title name) (description (mksym *package* name '/description)) (icon NIL)) (enlist name)
    (destructuring-bind (event-name event-type) (enlist (or event-type '(unused NIL)) event-type)
      `(progn (setf (achievement ',name) (ensure-instance (ignore-errors (achievement ',name)) 'achievement
                                                          :name ',name
                                                          :title ',title
                                                          :description ',description
                                                          :icon ,icon
                                                          :event-type ',event-type
                                                          :test-function (lambda (,event-name)
                                                                           (declare (ignorable ,event-name))
                                                                           ,@test-function)))
              ',name))))

(defmethod title ((achievement achievement))
  (language-string (slot-value achievement 'title)))

(defmethod description ((achievement achievement))
  (language-string (slot-value achievement 'description)))

(define-handler (achievement (ev event)) ()
  (when (and (not (active-p achievement))
             (typep ev (event-type achievement))
             (funcall (test-function achievement) ev))
    (award achievement)))

(defmethod award ((achievement achievement))
  (setf (active-p achievement) T))

(defmethod award ((name symbol))
  (award (achievement name)))

(defmethod (setf active-p) :around (new (achievement achievement))
  (let ((old (active-p achievement)))
    (cond ((and new (not old))
           (v:info :trial.achievements "Unlocked ~a ~s" (name achievement) (title achievement))
           (call-next-method)
           (issue T 'achievement-unlocked :achievement achievement))
          ((and old (not new))
           (v:info :trial.achievements "Relocked ~a ~s" (name achievement) (title achievement))
           (call-next-method)
           (issue T 'achievement-relocked :achievement achievement)))))

(define-event achievement-event () achievement)
(define-event achievement-unlocked (achievement-event))
(define-event achievement-relocked (achievement-event))

(define-global +achievement-api+ NIL)
(defvar *achievement-apis* ())

(defclass achievement-api ()
  ((active-p :initform NIL :accessor active-p)))

(defgeneric load-achievement-data (achievement-api))
(defgeneric notifications-display-p (achievement-api))
(defgeneric (setf notifications-display-p) (value achievement-api))

(define-handler (achievement-api event) ()
  (loop for achievement being the hash-values of *achievements*
        do (handle event achievement)))

(defmethod load-achievement-data ((all (eql T)))
  (dolist (api *achievement-apis*)
    (ignore-errors
     (with-error-logging (:trial.achievements "Failed to load achievement data from ~a" api)
       (return (setf +achievement-api+ (load-achievement-data api)))))))

(defclass local-achievement-api ()
  ())

(defun achievement-file-path ()
  (make-pathname :name "achievements" :type "lisp"
                 :defaults (config-directory)))

(defmethod load-achievement-data ((api local-achievement-api))
  (with-open-file (stream (achievement-file-path) :if-does-not-exist NIL)
    (when stream
      (loop for achievement being the hash-values of *achievements*
            do (setf (slot-value (achievement name) 'active-p) NIL))
      (loop for name = (read stream NIL NIL)
            while name
            do (ignore-errors
                (with-error-logging (:trial.achievements "Failed to find achievement ~a" name)
                  (setf (slot-value (achievement name) 'active-p) T)))))))

(defmethod save-achievement-data ((api local-achievement-api))
  (with-open-file (stream (achievement-file-path) :direction :output :if-exists :supersede)
    (loop for achievement being the hash-values of *achievements*
          do (when (active-p achievement)
               (prin1 (name achievement) stream)
               (terpri stream)))))

(defmethod notifications-display-p ((api local-achievement-api)) NIL)
(defmethod (setf notifications-display-p) (value (api local-achievement-api)) NIL)

(define-handler (local-achievement-api achievement-event :after) ()
  (save-achievement-data achievement-api))

(pushnew (make-instance 'local-achievement-api) *achievement-apis*)

(defclass achievement-main (main)
  ())

(defmethod initialize-instance :after ((main main) &key)
  (load-achievement-data T))

(defmethod handle :before (event (main achievement-main))
  (with-ignored-errors-on-release (:trial.achievements "Failed handling event ~a" event)
    (handle event +achievement-api+)))
