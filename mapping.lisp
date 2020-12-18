#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-global +retention-table* (make-hash-table :test 'eql))
(defvar *mappings* (make-hash-table :test 'equal))

(declaim (inline retained (setf retained) clear-retained))
(defun retained (id)
  (gethash id +retention-table*))

(defun (setf retained) (bool id)
  (setf (gethash id +retention-table*) bool))

(defun clear-retained ()
  (clrhash +retention-table*))

(defun mapping (name)
  (gethash name *mappings*))

(defun (setf mapping) (mapping name)
  (setf (gethash name *mappings*) mapping))

(defun remove-mapping (name)
  (remhash name *mappings*))

(defmacro define-mapping (name (loop ev) &body body)
  `(setf (mapping ',name)
         (list (lambda (,loop ,ev)
                 ,@body)
               ())))

(defmacro define-simple-mapping (name (from to &rest to-args) &body tests)
  (let ((loop (gensym "LOOP")))
    `(define-mapping ,name (,loop ,from)
       (when (typep ,from ',from)
         (with-all-slots-bound (,from ,from)
           (when (and ,@tests)
             (issue ,loop (make-instance ',to ,@to-args))))))))

(defun map-event (event loop)
  (loop for (function) being the hash-values of *mappings*
        do (funcall function loop event)))

(defclass action (event)
  ((source-event :initarg :source-event :initform NIL :accessor source-event)))

(defclass action-set () ()) ;; marker-class

(defun action-set (action)
  (let ((action (ensure-class action)))
    (flet ((direct-action-set (base)
             (loop for class in (c2mop:class-direct-superclasses base)
                   do (when (eql class (find-class 'action-set))
                        (return base)))))
      (or (direct-action-set action)
          (loop for class in (or (ignore-errors (c2mop:class-precedence-list action))
                                 (c2mop:compute-class-precedence-list action))
                thereis (direct-action-set class))
          (find-class 'action)))))

(defclass analog-action (action)
  ((value :initarg :value :initform 0f0 :accessor value)))

(defclass directional-action (action)
  ((x :initarg :value :initform 0f0 :accessor x)
   (y :initarg :value :initform 0f0 :accessor y)))

(defclass spatial-action (action)
  ((x :initarg :value :initform 0f0 :accessor x)
   (y :initarg :value :initform 0f0 :accessor y)
   (z :initarg :value :initform 0f0 :accessor z)))

(defun remove-action-mappings (action)
  (loop for k being the hash-keys of *mappings*
        do (when (and (consp k) (eql (car k) action))
             (remhash k *mappings*))))

(defmacro define-action (name superclasses &body mappings)
  (flet ((compile-mapping (mapping)
           (destructuring-bind (type &rest tests) mapping
             `(define-simple-mapping (,name ,type) (,type ,name :source-event ,type)
                ,@tests))))
    (setf superclasses (append superclasses '(action)))
    `(progn
       (defclass ,name ,superclasses
         ())
       (remove-action-mappings ',name)
       ,@(mapcar #'compile-mapping mappings))))

(defgeneric process-trigger-form (ev event &key &allow-other-keys)
  (:method (ev (_ (eql 'label)) &key &allow-other-keys))
  (:method (ev (_ (eql 'key)) &key one-of (edge :rise))
    `(,(ecase edge (:rise 'key-press) (:fall 'key-release))
      (one-of (key ,ev) ,@one-of)))
  (:method (ev (_ (eql 'button)) &key one-of (edge :rise))
    `(,(ecase edge (:rise 'gamepad-press) (:fall 'gamepad-release))
      (one-of (button ,ev) ,@one-of)))
  (:method (ev (_ (eql 'mouse)) &key one-of (edge :rise))
    `(,(ecase edge (:rise 'mouse-press) (:fall 'mouse-release))
      (one-of (button ,ev) ,@one-of)))
  (:method (ev (_ (eql 'axis)) &key one-of (edge :rise) (threshold 0.5))
    `(gamepad-move
      (and (one-of (axis ,ev) ,@one-of)
           ,(if (xor (eql edge :rise) (plusp threshold))
                `(< (pos ,ev) ,threshold (old-pos ,ev))
                `(< (old-pos ,ev) ,threshold (pos ,ev)))))))

(defgeneric process-retain-form (ev event &key &allow-other-keys)
  (:method (ev (_ (eql 'label)) &key &allow-other-keys))
  (:method (ev (_ (eql 'key)) &key one-of (edge :rise))
    `(,(ecase edge (:rise 'key-press) (:fall 'key-release))
      ,(ecase edge (:rise 'key-release) (:fall 'key-press))
      (one-of (key ,ev) ,@one-of)))
  (:method (ev (_ (eql 'button)) &key one-of (edge :rise))
    `(,(ecase edge (:rise 'gamepad-press) (:fall 'gamepad-release))
      ,(ecase edge (:rise 'gamepad-release) (:fall 'gamepad-press))
      (one-of (button ,ev) ,@one-of)))
  (:method (ev (_ (eql 'mouse)) &key one-of (edge :rise))
    `(,(ecase edge (:rise 'mouse-press) (:fall 'mouse-release))
      ,(ecase edge (:rise 'mouse-release) (:fall 'mouse-press))
      (one-of (button ,ev) ,@one-of)))
  (:method (ev (_ (eql 'axis)) &key one-of (edge :rise) (threshold 0.5))
    `(gamepad-move
      gamepad-move
      (and (one-of (axis ,ev) ,@one-of)
           ,(if (xor (eql edge :rise) (plusp threshold))
                `(< (pos ,ev) ,threshold (old-pos ,ev))
                `(< (old-pos ,ev) ,threshold (pos ,ev))))
      (and (one-of (axis ,ev) ,@one-of)
           ,(if (xor (eql edge :rise) (plusp threshold))
                `(< (old-pos ,ev) ,threshold (pos ,ev))
                `(< (pos ,ev) ,threshold (old-pos ,ev)))))))

(defgeneric process-analog-form (ev event &key &allow-other-keys)
  (:method (ev (_ (eql 'label)) &key &allow-other-keys))
  (:method (ev (_ (eql 'key)) &key one-of (edge :rise) (value 1.0))
    `(key-event
      (one-of (key ,ev) ,@one-of)
      (etypecase ,ev
        (key-press ,(ecase edge (:rise value) (:fall 0.0)))
        (key-release (ecase edge (:rise 0.0) (:fall ,value))))))
  (:method (ev (_ (eql 'button)) &key one-of (edge :rise) (value 1.0))
    `(button-event
      (one-of (button ,ev) ,@one-of)
      (etypecase ,ev
        (button-press ,(ecase edge (:rise value) (:fall 0.0)))
        (button-release (ecase edge (:rise 0.0) (:fall ,value))))))
  (:method (ev (_ (eql 'mouse)) &key one-of (edge :rise) (value 1.0))
    `(mouse-button-event
      (one-of (button ,ev) ,@one-of)
      (etypecase ,ev
        (mouse-press ,(ecase edge (:rise value) (:fall 0.0)))
        (mouse-release (ecase edge (:rise 0.0) (:fall ,value))))))
  (:method (ev (_ (eql 'cursor)) &key (axis :x) (multiplier 1.0))
    `(mouse-move
      T
      (* ,multiplier (,(ecase axis (:x 'vx2) (:y 'vy2)) (pos ,ev)))))
  (:method (ev (_ (eql 'axis)) &key one-of (threshold 0.1) (multiplier 1.0))
    `(gamepad-move
      (and (one-of (axis ,ev) ,@one-of)
           (< ,threshold (pos ,ev)))
      (* ,multiplier (pos ,ev)))))

(defun process-mapping-form (loop ev form)
  (destructuring-bind (type action &body triggers) form
    (ecase type
      (trigger
       (loop for trigger in triggers
             for (evtype condition) = (apply #'process-trigger-form ev trigger)
             when evtype
             collect (list evtype
                           `(when ,condition
                              (issue ,loop (make-instance ',action :source-event ,ev))))))
      (retain
       (loop for trigger in triggers
             for (evdn evup cddn cdup) = (apply #'process-retain-form ev trigger)
             when evdn
             collect (list evdn
                           `(when ,cddn
                              ,@(when (find-class action NIL)
                                  `((issue ,loop (make-instance ',action :source-event ,ev))))
                              (setf (retained ',action) T)))
             when evup
             collect (list evup
                           `(when ,(or cdup cddn)
                              (setf (retained ',action) NIL)))))
      (analog
       (loop for trigger in triggers
             for (evtype condition value) = (apply #'process-analog-form ev trigger)
             when evtype
             collect (list evtype
                           `(when ,condition
                              (issue ,loop (make-instance ',action :source-event ,ev :value ,value)))))))))

;; TODO: could optimise this further by combining ONE-OF tests.
(defun load-mapping (input &key (name 'keymap) (package *package*))
  (etypecase input
    ((or pathname string)
     (with-open-file (stream input :direction :input)
       (load-mapping stream :name name :package package)))
    (stream
     (load-mapping (loop with *package* = package
                         for form = (read input NIL '#1=#:END)
                         until (eq form '#1#)
                         collect form)
                   :name name))
    (list
     (let ((bits (make-hash-table :test 'eql)))
       (dolist (form input)
         (loop for (type body) in (process-mapping-form 'loop 'event form)
               do (push body (gethash type bits))))
       (setf (mapping name)
             (list
              (compile NIL `(lambda (loop event)
                              (typecase event
                                ,@(loop for event being the hash-keys of bits
                                        for bodies being the hash-values of bits
                                        collect `(,event ,@bodies)))))
              input))))))

(defun event-trigger (event &optional (base-event 'input-event))
  (loop for (_function mapping) being the hash-values of *mappings*
        do (loop for (_type target . sources) in mapping
                 do (when (eql event target)
                      (loop for (source . args) in sources
                            for source-event = (case source
                                                 (key 'key-event)
                                                 (mouse 'mouse-button-event)
                                                 (button 'gamepad-event))
                            do (when (subtypep source-event base-event)
                                 (return-from event-trigger
                                   (values (getf args :one-of) source))))))))

#| Keymap should have the following syntax:

keymap    ::= mapping*
mapping   ::= (type action trigger*)
type      ::= retain | trigger
trigger   ::= (key one-of edge?)
            | (mouse one-of edge?)
            | (button one-of edge?)
            | (axis one-of edge? threshold?)
one-of    ::= :one-of label
edge      ::= :edge :rise | :edge :fall
threshold ::= :threshold number
action    --- a symbol naming an action event
label     --- a keyword naming a key or button label

Examples:

(trigger quicksave
  (label :english "Quick Save")
  (key :one-of (:f5)))

(retain dash
  (label :english "Dash")
  (axis :one-of (:r2) :threshold 0.2))
|#
