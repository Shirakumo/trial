#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *mappings* (make-hash-table :test 'equal))

(defun mapping (name)
  (gethash name *mappings*))

(defun (setf mapping) (mapping name)
  (setf (gethash name *mappings*) mapping))

(defun remove-mapping (name)
  (remhash name *mappings*))

(defmacro define-mapping ((name event-type) args &body body)
  (let ((ev (or (first args) (gensym "EVENT"))))
    `(setf (mapping ',name)
           (lambda (,ev)
             (when (typep ,ev ',event-type)
               (with-slots ,(rest args) ,ev
                 ,@body))))))

(defmacro define-simple-mapping (name (from to) &body tests)
  (let ((ev (gensym "EVENT"))
        (to (enlist to)))
    `(setf (mapping ',name)
           (lambda (,ev)
             (when (typep ,ev ',from)
               (with-all-slots-bound (,ev ,from)
                 (when (and ,@tests)
                   (make-instance ',(first to) ,@(rest to)))))))))

;; Currently this system is very unoptimised as it has to
;; loop through all potential mappers every time. Optimisation
;; could be done by separating out the event-type test somehow.
(defun map-event (event loop)
  (loop for function being the hash-values of *mappings*
        for result = (funcall function event)
        do (when result (issue loop result))))

(defclass action (event)
  ())

(defun remove-action-mappings (action)
  (loop for k being the hash-keys of *mappings*
        do (when (and (consp k) (eql (car k) action))
             (remhash k *mappings*))))

(defmacro define-action (name superclasses &body mappings)
  (flet ((compile-mapping (mapping)
           (destructuring-bind (type &rest tests) mapping
             `(define-simple-mapping (,name ,type) (,type ,name)
                ,@tests))))
    `(progn
       (defclass ,name ,(or superclasses '(action))
         ())
       (remove-action-mappings ',name)
       ,@(mapcar #'compile-mapping mappings))))

(define-action system-action ())

(define-action launch-editor (system-action)
  (key-press (eql key :section)))

(define-action pack (system-action)
  (key-press (eql key :f2)))

(define-action unpack (system-action)
  (key-press (eql key :f3)))

(define-action reload-assets (system-action)
  (key-press (eql key :f5)))

(define-action reload-scene (system-action)
  (key-press (eql key :f6)))

(define-action player-action ())

(define-action movement (player-action))

(define-action start-left (movement)
  (key-press (one-of key :a :left))
  (gamepad-press (eql button :dpad-left))
  (gamepad-move (one-of axis :left-h :dpad-h) (< pos -0.2 old-pos)))

(define-action start-right (movement)
  (key-press (one-of key :d :right))
  (gamepad-press (eql button :dpad-right))
  (gamepad-move (one-of axis :left-h :dpad-h) (< old-pos 0.2 pos)))

(define-action start-up (movement)
  (key-press (one-of key :w :up))
  (gamepad-press (eql button :dpad-up))
  (gamepad-move (one-of axis :left-v :dpad-v) (< pos -0.2 old-pos)))

(define-action start-down (movement)
  (key-press (one-of key :s :down))
  (gamepad-press (eql button :dpad-down))
  (gamepad-move (one-of axis :left-v :dpad-v) (< old-pos 0.2 pos)))

(define-action stop-left (movement)
  (key-release (one-of key :a :left))
  (gamepad-release (eql button :dpad-left))
  (gamepad-move (one-of axis :left-h :dpad-h) (< old-pos -0.2 pos)))

(define-action stop-right (movement)
  (key-release (one-of key :d :right))
  (gamepad-release (eql button :dpad-right))
  (gamepad-move (one-of axis :left-h :dpad-h) (< pos 0.2 old-pos)))

(define-action stop-up (movement)
  (key-release (one-of key :w :up))
  (gamepad-release (eql button :dpad-up))
  (gamepad-move (one-of axis :left-v :dpad-v) (< old-pos -0.2 pos)))

(define-action stop-down (movement)
  (key-release (one-of key :s :down))
  (gamepad-release (eql button :dpad-down))
  (gamepad-move (one-of axis :left-v :dpad-v) (< pos 0.2 old-pos)))
