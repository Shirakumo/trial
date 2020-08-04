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
         (lambda (,loop ,ev)
           ,@body)))

(defmacro define-simple-mapping (name (from to &rest to-args) &body tests)
  (let ((loop (gensym "LOOP")))
    `(define-mapping ,name (,loop ,from)
       (when (typep ,from ',from)
         (with-all-slots-bound (,from ,from)
           (when (and ,@tests)
             (issue ,loop (make-instance ',to ,@to-args))))))))

(defun map-event (event loop)
  (loop for function being the hash-values of *mappings*
        do (funcall function loop event)
        do (when result (issue loop result))))

(defclass action (event)
  ((source-event :initarg :source-event :initform NIL :accessor source-event)))

(defun remove-action-mappings (action)
  (loop for k being the hash-keys of *mappings*
        do (when (and (consp k) (eql (car k) action))
             (remhash k *mappings*))))

(defmacro define-action (name superclasses &body mappings)
  (flet ((compile-mapping (mapping)
           (destructuring-bind (type &rest tests) mapping
             `(define-simple-mapping (,name ,type) (,type ,name :source-event ,type)
                ,@tests))))
    `(progn
       (defclass ,name ,(or superclasses '(action))
         ())
       (remove-action-mappings ',name)
       ,@(mapcar #'compile-mapping mappings))))

(defgeneric process-trigger-form (ev event &key &allow-other-keys)
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

(defun process-mapping-form (loop ev form)
  (destructuring-bind (type action &body triggers) form
    (ecase type
      (trigger
       (loop for trigger in triggers
             for (evtype condition) = (apply #'process-trigger-form ev trigger)
             collect (list evtype
                           `(when ,condition
                              (issue ,loop (make-instance ',action))))))
      (retain
       (loop for trigger in triggers
             for (evup evdn cdup cddn) = (apply #'process-retain-form ev trigger)
             collect (list evup
                           `(when ,cdup
                              (setf (retained ',action) T)))
             collect (list evdn
                           `(when ,(or cddn cdup)
                              (setf (retained ',action) NIL))))))))

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
             (compile NIL `(lambda (loop event)
                             (typecase event
                               ,@(loop for event being the hash-keys of bits
                                       for bodies being the hash-values of bits
                                       collect `(,event ,@bodies))))))))))

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
  (key :one-of (:f5)))

(retain dash
  (axis :one-of (:r2) :threshold 0.2))
|#
