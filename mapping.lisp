#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-global +retention-table+ (make-hash-table :test 'eql))
(defvar *mapping-functions* (make-hash-table :test 'eql))
(defvar *action-mappings* ())

(defun mapping-function (name)
  (gethash name *mapping-functions*))

(defun (setf mapping-function) (mapping name)
  (setf (gethash name *mapping-functions*) mapping))

(defun remove-mapping-function (name)
  (remhash name *mapping-functions*))

(defmacro define-mapping-function (name (loop ev) &body body)
  `(setf (mapping-function ',name)
         (lambda (,loop ,ev)
           (declare (ignorable ,loop))
           ,@body)))

(defun map-event (event loop)
  (loop for function being the hash-values of *mapping-functions*
        do (funcall function loop event)))

(declaim (inline %retained (setf %retained) retained (setf retained) clear-retained))
(defun %retained (id)
  (gethash id +retention-table+ 0))

(defun (setf %retained) (int id)
  (setf (gethash id +retention-table+) int))

(defun retained (id)
  (< 0 (gethash id +retention-table+ 0)))

(defun (setf retained) (bool id)
  (setf (gethash id +retention-table+) (if bool 1 0)))

(defun clear-retained ()
  (clrhash +retention-table+))

(defun reset-retained (&optional (scene (scene +main+)))
  (clear-retained)
  (typecase +input-source+
    ((eql :keyboard)
     ;; FIXME: how the heck do we do this?
     )
    (gamepad:device
     (loop for label across gamepad:+labels+
           for button = (gamepad:button label +input-source+)
           for axis = (gamepad:axis label +input-source+)
           do (when button
                (map-event (make-instance 'gamepad-press :button label :device +input-source+)
                           scene))
              (when (<= 0.2 (abs axis))
                (map-event (make-instance 'gamepad-move :axis label :pos axis :old-pos 0.0 :device +input-source+)
                           scene))))))

(define-mapping-function retain-generic (loop ev)
  (typecase ev
    (mouse-press
     (setf (retained (button ev)) T))
    (mouse-release
     (setf (retained (button ev)) NIL))
    (key-press
     (setf (retained (key ev)) T)
     (case (key ev)
       ((:left-control :right-control) (setf (retained :control) T))
       ((:left-shift :right-shift) (setf (retained :shift) T))
       ((:left-alt :right-alt) (setf (retained :alt) T))))
    (key-release
     (setf (retained (key ev)) NIL)
     (case (key ev)
       ((:left-control :right-control) (setf (retained :control) NIL))
       ((:left-shift :right-shift) (setf (retained :shift) NIL))
       ((:left-alt :right-alt) (setf (retained :alt) NIL))))))

(defclass action-mapping ()
  ((action-type :initarg :action-type :accessor action-type)
   (event-type :initarg :event-type :accessor event-type)
   (qualifier :initarg :qualifier :accessor qualifier)
   (%action-prototype)))

(defmethod shared-initialize :after ((mapping action-mapping) slots &key)
  (setf (slot-value mapping '%action-prototype) (c2mop:class-prototype (c2mop:ensure-finalized (find-class (action-type mapping))))))

(defmethod print-object ((mapping action-mapping) stream)
  (print-unreadable-object (mapping stream :type T)
    (format stream "~s ~s -> ~s" (event-type mapping) (qualifier mapping) (action-type mapping))))

(defgeneric event-applicable-p (event mapping))
(defgeneric event-active-p (event mapping))
(defgeneric perform-event-mapping (event mapping loop))
(defgeneric from-mapping-description (type action bindings))
(defgeneric to-mapping-description (mapping))
(defgeneric event-from-action-mapping (mapping))
(defgeneric event-to-action-mapping (event action &key))
(defgeneric stratify-action-mapping (mapping))

(defmethod active-p ((mapping action-mapping))
  (active-p (slot-value mapping '%action-prototype)))

(defmethod event-applicable-p ((event input-event) (mapping action-mapping))
  NIL)

(defmethod event-applicable-p ((event key-event) (mapping action-mapping))
  (and (not (repeat-p event))
       (typep event (event-type mapping))
       (find (key event) (qualifier mapping))))

(defmethod event-applicable-p ((event mouse-button-event) (mapping action-mapping))
  (and (typep event (event-type mapping))
       (find (button event) (qualifier mapping))))

(defmethod event-applicable-p ((event gamepad-button-event) (mapping action-mapping))
  (and (typep event (event-type mapping))
       (find (button event) (qualifier mapping))))

(defmethod event-applicable-p ((event gamepad-move) (mapping action-mapping))
  (and (typep event (event-type mapping))
       (find (axis event) (qualifier mapping))))

(defmethod stratify-action-mapping ((mapping action-mapping))
  (loop for qualifier in (qualifier mapping)
        collect (make-instance (type-of mapping) :action-type (action-type mapping)
                                                 :event-type (event-type mapping)
                                                 :qualifier (list qualifier))))

(defclass digital-mapping (action-mapping)
  ((threshold :initarg :threshold :initform +0.5 :accessor threshold)
   (toggle-p :initarg :toggle-p :initform NIL :accessor toggle-p)))

(defmethod event-active-p ((event gamepad-move) (mapping digital-mapping))
  (and (<= (abs (threshold mapping)) (abs (pos event)))
       (= (float-sign (threshold mapping)) (float-sign (pos event)))))

(defmethod event-active-p ((event digital-event) (mapping digital-mapping))
  (typep event '(or key-press mouse-press gamepad-press)))

(defmethod perform-event-mapping (event (mapping digital-mapping) loop)
  (let ((active-p (event-active-p event mapping))
        (action (action-type mapping)))
    (cond ((toggle-p mapping)
           (when active-p
             (setf (%retained action) (if (retained action) -1 +1))))
          (T
           (when (and (not (retained action)) active-p)
             (issue loop (make-instance action :source-event event)))
           (typecase event
             (digital-event
              (setf (%retained action) (max 0 (+ (%retained action) (if active-p +1 -1)))))
             (T
              (setf (%retained action) (if active-p +1 0))))))))

(defun normalize-mapping-event-type (type)
  (case type
    (key 'key-event)
    (mouse 'mouse-button-event)
    (button 'gamepad-button-event)
    (axis 'gamepad-move)
    (T type)))

(defmethod from-mapping-description ((type (eql 'trigger)) action bindings)
  (loop for binding in bindings
        collect (destructuring-bind (type &key one-of edge threshold (toggle NIL toggle-p)) binding
                  (make-instance 'digital-mapping
                                 :action-type action
                                 :event-type (normalize-mapping-event-type type)
                                 :qualifier one-of
                                 :threshold (or threshold 0.5)
                                 :toggle-p (if toggle-p toggle (eql :rise-only edge))))))

(defmethod to-mapping-description ((mapping digital-mapping))
  (list* 'trigger (action-type mapping)
         (append (list (case (event-type mapping)
                         ((key-event key-press) 'key)
                         ((mouse-button-event mouse-press) 'mouse)
                         ((gamepad-button-event gamepad-press) 'button)
                         (gamepad-move 'axis)
                         (T (event-type mapping)))
                       :one-of (qualifier mapping))
                 (unless (subtypep (event-type mapping) 'digital-event)
                   (list :threshold (threshold mapping)))
                 (when (toggle-p mapping) `(:toggle T)))))

(defmethod event-from-action-mapping ((mapping digital-mapping))
  (let ((qualifier (first (qualifier mapping))))
    (ecase (event-type mapping)
      ((key-event key-press key-release) (make-instance 'key-press :key qualifier))
      ((mouse-button-event mouse-press mouse-release) (make-instance 'mouse-press :button qualifier :pos #.(vec 0 0)))
      ((gamepad-button-event gamepad-press gamepad-release) (make-instance 'gamepad-press :device NIL :button qualifier))
      (gamepad-move (make-instance 'gamepad-move :device NIL :axis qualifier :old-pos 0.0 :pos (threshold mapping))))))

(defmethod event-to-action-mapping (event (action symbol) &rest args &key &allow-other-keys)
  (apply #'event-to-action-mapping event (make-instance action) args))

(defmethod event-to-action-mapping ((event gamepad-move) (action action) &key (threshold 0.5) toggle-p)
  (make-instance 'digital-mapping
                 :action-type (type-of action)
                 :event-type (type-of event)
                 :qualifier (list (axis event))
                 :threshold threshold
                 :toggle-p toggle-p))

(defmethod event-to-action-mapping ((event digital-event) (action action) &key toggle-p)
  (make-instance 'digital-mapping
                 :action-type (type-of action)
                 :event-type (typecase event
                               (key-event 'key-event)
                               (mouse-button-event 'mouse-button-event)
                               (gamepad-button-event 'gamepad-button-event)
                               (T (type-of event)))
                 :qualifier (list (button event))
                 :toggle-p toggle-p))

(defmethod stratify-action-mapping ((mapping digital-mapping))
  (mapc (lambda (new)
          (setf (threshold new) (threshold mapping))
          (setf (toggle-p new) (toggle-p mapping)))
        (call-next-method)))

(define-mapping-function input-maps (loop event)
  (when (typep event 'input-event)
    ;; TODO: This is slow, as we keep iterating over and testing for events that will
    ;;       very likely not change for a long time (comparatively). We should cache
    ;;       the set of applicable mappings depending on active action-sets whenever
    ;;       those change.
    (dolist (mapping *action-mappings*)
      (when (and (active-p mapping)
                 (event-applicable-p event mapping))
        (perform-event-mapping event mapping loop)))))

(defun compile-mapping (input)
  (let ((mappings ()))
    (dolist (description input)
      (destructuring-bind (type action &rest bindings) description
        (dolist (mapping (from-mapping-description type action bindings))
          (push mapping mappings))))
    (setf *action-mappings* mappings)))

(defun load-mapping (input &key (package *package*))
  (etypecase input
    ((or pathname string)
     (with-open-file (stream input :direction :input)
       (load-mapping stream :package package)))
    (stream
     (load-mapping (loop with *package* = package
                         for form = (read input NIL '#1=#:END)
                         until (eq form '#1#)
                         collect form)))
    (list
     (compile-mapping input))))

(defun save-mapping (output)
  (etypecase output
    (null
     (with-output-to-string (stream)
       (save-mapping stream)))
    ((or pathname string)
     (with-open-file (stream output :direction :output :if-exists :supersede)
       (save-mapping stream)))
    (stream
     (let ((descriptions (mapcar #'to-mapping-description *action-mappings*))
           (cache (make-hash-table :test 'equal))
           (*print-case* :downcase))
       (dolist (description descriptions)
         (push (cddr description) (gethash (list (first description) (second description)) cache)))
       ;; FIXME: collect based on matching :one-of.
       (let ((descriptions (loop for preamble being the hash-keys of cache
                                 for bindings being the hash-values of cache
                                 collect (append preamble bindings))))
         (loop for (type event . bindings) in descriptions
               do (format output "(~s ~s~{~%  ~s~})~%~%"
                          type event bindings)))))))

(defun find-action-mappings (action &optional (input-event-type 'input-event))
  (let ((triggers ()))
    (loop for mapping in *action-mappings*
          do (when (and (eql action (action-type mapping))
                        (subtypep (event-type mapping) input-event-type))
               (push mapping triggers)))
    triggers))

(defun update-action-mappings (new-mappings &key (prune-event-type T))
  (let ((mappings (append new-mappings
                          (remove-if (lambda (mapping)
                                       (and (find (action-type mapping) new-mappings :key #'action-type)
                                            (subtypep (event-type mapping) prune-event-type)))
                                     *action-mappings*))))
    (setf *action-mappings* mappings)))

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
