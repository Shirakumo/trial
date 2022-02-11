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

(defun action-definition (mapping action)
  (find action (second (mapping mapping)) :key #'second))

(defun action-binding (mapping action &key (device :gamepad))
  (let ((binds (cddr (action-definition mapping action))))
    (ecase device
      (:keyboard
       (or (assoc 'key binds) (assoc 'mouse binds)))
      (:gamepad
       (or (assoc 'button binds) (assoc 'axis binds))))))

(defun action-input (mapping action &key (device :gamepad))
  (getf (rest (action-binding mapping action :device device)) :one-of))

(defun make-event-from-binding (binding)
  (destructuring-bind (type &key one-of (threshold 0.5) (edge :rise) &allow-other-keys) binding
    (ecase type
      (key (make-instance (ecase edge ((:rise :rise-only) 'key-press) ((:fall :fall-only) 'key-release))
                          :key (first one-of)))
      (mouse (make-instance (ecase edge ((:rise :rise-only) 'mouse-press) ((:fall :fall-only) 'mouse-release))
                            :button (first one-of)))
      (button (make-instance (ecase edge ((:rise :rise-only) 'gamepad-press) ((:fall :fall-only) 'gamepad-release))
                             :device NIL
                             :button (first one-of)))
      (axis (make-instance 'gamepad-move
                           :device NIL
                           :axis (first one-of)
                           :old-pos (ecase edge ((:rise :rise-only) 0.0) ((:fall :fall-only) threshold))
                           :pos (ecase edge ((:rise :rise-only) threshold) ((:fall :fall-only) 0.0)))))))

(defclass action-set () ()) ;; marker-class
(defclass exclusive-action-set () ())

(defmethod (setf active-p) :after (value (set exclusive-action-set))
  (when value
    (dolist (other (c2mop:class-direct-subclasses (find-class 'exclusive-action-set)))
      (unless (eql (class-of set) other)
        (setf (active-p other) NIL)))))

(defun find-action-set (action)
  (flet ((direct-action-set (base)
           (loop for class in (c2mop:class-direct-superclasses base)
                 do (when (eql class (find-class 'action-set))
                      (return base)))))
    (or (direct-action-set action)
        (loop for class in (or (ignore-errors (c2mop:class-precedence-list action))
                               (c2mop:compute-class-precedence-list action))
              thereis (direct-action-set class))
        (find-class 'action))))

(defun action-set (action)
  (find-action-set (ensure-class action)))

(define-compiler-macro action-set (action &environment env)
  (if (constantp action env)
      `(load-time-value (find-action-set (ensure-class ,action)))
      `(find-action-set (ensure-class ,action))))

(defmacro define-action-set (name &optional superclasses)
  `(progn (defclass ,name (,@superclasses action-set)
            ((active-p :initform T :accessor active-p :allocation :class)))
          (defmethod active-p ((class (eql (find-class ',name))))
            (active-p (c2mop:class-prototype class)))
          (defmethod (setf active-p) (value (class (eql (find-class ',name))))
            (setf (active-p (c2mop:class-prototype class)) value))
          (c2mop:finalize-inheritance (find-class ',name))))

(defclass action (event)
  ((source-event :initarg :source-event :initform NIL :accessor source-event)))

(defmethod active-p ((action (eql (find-class 'action)))) T)

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

(defun process-edge (edge rise fall)
  (list (ecase edge
          ((:rise-only :rise) rise)
          (:fall-only) (:fall fall))
        (ecase edge
          (:rise-only) (:rise fall)
          ((:fall-only :fall) rise))))

(defgeneric process-trigger-form (ev event &key &allow-other-keys)
  (:method (ev (_ (eql 'label)) &key &allow-other-keys))
  (:method (ev (_ (eql 'key)) &key one-of (edge :rise))
    `(,@(process-edge edge 'key-press 'key-release)
      (and (one-of (key ,ev) ,@one-of)
           (not (repeat-p ,ev)))))
  (:method (ev (_ (eql 'button)) &key one-of (edge :rise))
    `(,@(process-edge edge 'gamepad-press 'gamepad-release)
      (one-of (button ,ev) ,@one-of)))
  (:method (ev (_ (eql 'mouse)) &key one-of (edge :rise))
    `(,@(process-edge edge 'mouse-press 'mouse-release)
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
        (key-release ,(ecase edge (:rise 0.0) (:fall value))))))
  (:method (ev (_ (eql 'button)) &key one-of (edge :rise) (value 1.0))
    `(button-event
      (one-of (button ,ev) ,@one-of)
      (etypecase ,ev
        (button-press ,(ecase edge (:rise value) (:fall 0.0)))
        (button-release ,(ecase edge (:rise 0.0) (:fall value))))))
  (:method (ev (_ (eql 'mouse)) &key one-of (edge :rise) (value 1.0))
    `(mouse-button-event
      (one-of (button ,ev) ,@one-of)
      (etypecase ,ev
        (mouse-press ,(ecase edge (:rise value) (:fall 0.0)))
        (mouse-release ,(ecase edge (:rise 0.0) (:fall value))))))
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
             for (evdn evup cddn cdup) = (apply #'process-trigger-form ev trigger)
             when evdn
             collect (list evdn
                           `(when (and ,cddn
                                       (active-p (action-set ',action)))
                              (issue ,loop (make-instance ',action :source-event ,ev))
                              (setf (retained ',action)
                                    ,(if evup T `(not (retained ',action))))))
             when evup
             collect (list evup
                           `(when (and ,(or cdup cddn)
                                       (active-p (action-set ',action)))
                              (setf (retained ',action) NIL)))))
      (analog
       (loop for trigger in triggers
             for (evtype condition value) = (apply #'process-analog-form ev trigger)
             when evtype
             collect (list evtype
                           `(when (and ,condition
                                       (active-p (action-set ',action)))
                              (issue ,loop (make-instance ',action :source-event ,ev :value ,value)))))))))

(defun compile-mapping (&key name)
  (let ((bits (make-hash-table :test 'eql))
        (data (mapping name)))
    (dolist (form (second data))
      (loop for (type body) in (process-mapping-form 'loop 'event form)
            do (push body (gethash type bits))))
    (setf (first data) (compile NIL `(lambda (loop event)
                                       (typecase event
                                         ,@(loop for event being the hash-keys of bits
                                                 for bodies being the hash-values of bits
                                                 collect `(,event ,@bodies))))))))

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
     (setf (mapping name) (list NIL input))
     (compile-mapping :name name))))

(defun save-mapping (output &key (name 'keymap))
  (etypecase output
    (null
     (with-output-to-string (stream)
       (save-mapping stream :name name)))
    ((or pathname string)
     (with-open-file (stream output :direction :output :if-exists :supersede)
       (save-mapping stream :name name)))
    (stream
     (let ((bindings (second (mapping name)))
           (*print-case* :downcase))
       (loop for (type event . actions) in bindings
             do (format output "(~s ~s~{~%  ~s~})~%~%"
                        type event actions))))))

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
                                   (values (getf args :one-of) source args))))))))

(defun set-trigger-from-event (event action &key (mapping 'keymap) (threshold 0.5) (edge :rise) (compile T))
  (let* ((map (mapping mapping))
         (binding (find action (second map) :key #'second))
         (action-binding (etypecase event
                           (key-event
                            `(key :one-of (,(key event))))
                           (mouse-button-event
                            `(mouse :one-of (,(button event))))
                           ((or gamepad-press gamepad-release)
                            `(button :one-of (,(button event))))
                           (gamepad-move
                            `(axis :one-of (,(axis event)) :threshold ,(* (float-sign (pos event)) threshold)))))
         (action-binding (if (eql edge :rise) action-binding
                             (append action-binding (list :edge edge))))
         (pruned (loop for action in (cddr binding)
                       unless (find (first action)
                                    (etypecase event
                                      (key-event '(key))
                                      (mouse-event '(mouse))
                                      (gamepad-event '(button axis))))
                       collect action))
         (new-binding (list* (first binding) action action-binding pruned)))
    (setf (second map) (list* new-binding (remove binding (second map))))
    (when compile
      (compile-mapping :name mapping))))

#| Keymap should have the following syntax:
                                        ; ;
keymap    ::= mapping*                  ; ;
mapping   ::= (type action trigger*)    ; ;
type      ::= retain | trigger          ; ;
trigger   ::= (key one-of edge?)        ; ;
| (mouse one-of edge?)                  ; ;
| (button one-of edge?)                 ; ;
| (axis one-of edge? threshold?)        ; ;
one-of    ::= :one-of label             ; ;
edge      ::= :edge :rise | :edge :fall ; ;
threshold ::= :threshold number         ; ;
action    --- a symbol naming an action event ; ;
label     --- a keyword naming a key or button label ; ;
                                        ; ;
Examples:                               ; ;
                                        ; ;
(trigger quicksave                      ; ;
(label :english "Quick Save")           ; ;
(key :one-of (:f5)))                    ; ;
                                        ; ;
(retain dash                            ; ;
(label :english "Dash")                 ; ;
(axis :one-of (:r2) :threshold 0.2))    ; ;
|#
