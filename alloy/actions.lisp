(in-package #:org.shirakumo.fraf.trial.alloy)

(trial:define-action-set ui-actions (trial:exclusive-action-set))
(trial:define-action select-left (ui-actions))
(trial:define-action select-right (ui-actions))
(trial:define-action select-up (ui-actions))
(trial:define-action select-down (ui-actions))
(trial:define-action accept (ui-actions))
(trial:define-action back (ui-actions))

(defmethod trial:handle ((ev select-left) (ui ui))
  (alloy:handle (load-time-value (make-instance 'alloy:focus-left)) ui))

(defmethod trial:handle ((ev select-right) (ui ui))
  (alloy:handle (load-time-value (make-instance 'alloy:focus-right)) ui))

(defmethod trial:handle ((ev select-up) (ui ui))
  (alloy:handle (load-time-value (make-instance 'alloy:focus-up)) ui))

(defmethod trial:handle ((ev select-down) (ui ui))
  (alloy:handle (load-time-value (make-instance 'alloy:focus-down)) ui))

(defmethod trial:handle ((ev accept) (ui ui))
  (alloy:handle (load-time-value (make-instance 'alloy:activate)) ui))

(defmethod trial:handle ((ev back) (ui ui))
  (alloy:handle (load-time-value (make-instance 'alloy:exit)) ui))

(defmethod trial:handle :after ((ev trial:tick) (ui ui))
  (let ((hold (first-hold-time ui)))
    (loop for action in '(select-left select-right select-up select-down)
          do (when (trial:retained action)
               (if (eql action (car hold))
                   (incf (cdr hold) (trial:dt ev))
                   (setf (car hold) action
                         (cdr hold) 0.0))
               (return))
          finally (setf (car hold) NIL
                        (cdr hold) 0.0))
    ;; 0.5 is the initial repeat delay
    (when (< 0.5 (cdr hold))
      (trial:handle trial:+main+ (trial:make-event (car hold)))
      ;; 0.1 is the delay between repeats
      (setf (cdr hold) (- 0.5 0.1)))))
