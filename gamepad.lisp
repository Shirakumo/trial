#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *gamepad-device-table* (make-hash-table :test 'equal))
(defvar *gamepad-axis-table* (make-hash-table :test 'eql))
(defvar *gamepad-button-table* (make-hash-table :test 'eql))
(defvar *gamepad-handlers* ())
(defvar *gamepad-handlers-lock* (bt:make-lock))
(defvar *gamepad-input-thread* ())

(defun add-gamepad-handler (handler)
  (bt:with-lock-held (*gamepad-handlers-lock*)
    (pushnew handler *gamepad-handlers*)))

(defun remove-gamepad-handler (handler)
  (bt:with-lock-held (*gamepad-handlers-lock*)
    (setf *gamepad-handlers* (remove handler *gamepad-handlers*))))

(defun init-gamepad-system ()
  (or *gamepad-input-thread*
      (setf *gamepad-input-thread*
            (with-thread ("gamepad event thread")
              (cl-gamepad:init)
              (unwind-protect
                   (loop for i = 0 then (1+ i)
                         while *gamepad-input-thread*
                         do (when (= 0 (mod i 60))
                              (cl-gamepad:detect-devices))
                            (cl-gamepad:process-events)
                            (sleep 1/60))
                (cl-gamepad:shutdown))))))

(defun shutdown-gamepad-system ()
  (with-thread-exit (*gamepad-input-thread*)
    (setf *gamepad-input-thread* NIL)))

(init-gamepad-system)

(defun cl-gamepad:device-attached (device)
  (v:info :trial.input "Attached ~s (~:[Unknown~;~:*~a~])"
          (cl-gamepad:print-device device NIL)
          (gethash (cons (cl-gamepad:vendor device)
                         (cl-gamepad:product device))
                   *gamepad-device-table*))
  (dolist (handler *gamepad-handlers*)
    (handle (make-instance 'gamepad-attach :device device) handler)))

(defun cl-gamepad:device-removed (device)
  (v:info :trial.input "Removed ~s" (cl-gamepad:print-device device NIL))
  (dolist (handler *gamepad-handlers*)
    (handle (make-instance 'gamepad-remove :device device) handler)))

(defun cl-gamepad:button-pressed (button time device)
  (declare (ignore time))
  (let ((button (gamepad-button->symbol device button)))
    (dolist (handler *gamepad-handlers*)
      (handle (make-instance 'gamepad-press :button button :device device) handler))))

(defun cl-gamepad:button-released (button time device)
  (declare (ignore time))
  (let ((button (gamepad-button->symbol device button)))
    (dolist (handler *gamepad-handlers*)
      (handle (make-instance 'gamepad-release :button button :device device) handler))))

(defun cl-gamepad:axis-moved (axis last-value value time device)
  (declare (ignore time))
  (let ((axis (gamepad-axis->symbol device axis)))
    (dolist (handler *gamepad-handlers*)
      (handle (make-instance 'gamepad-move :axis axis :old-pos last-value :pos value :device device) handler))))

(defun make-gamepad-table (defs &optional base)
  (let ((table (make-hash-table :test 'eql)))
    (when base
      (maphash (lambda (k v) (setf (gethash k table) v)) base))
    (dolist (entry defs table)
      (setf (gethash (first entry) table) (second entry)))))

(defmacro define-gamepad (name (manufacturer id &key inherit) &body options)
  (let ((name (intern (string name) :keyword))
        (inherit (when inherit (intern (string name) :keyword))))
    `(progn
       (setf (gethash (cons ,manufacturer ,id) *gamepad-device-table*) ,name)
       (setf (gethash ,name *gamepad-axis-table*)
             (make-gamepad-table ',(cdr (assoc :axes options))
                                 (gethash ,inherit *gamepad-axis-table*)))
       (setf (gethash ,name *gamepad-button-table*)
             (make-gamepad-table ',(cdr (assoc :buttons options))
                                 (gethash ,inherit *gamepad-button-table*))))))

;; FIXME: Some gamepad axes are reversed, we need a way to
;;        indicate that.

;;; General name mapping conventions:
;; :left-h      -- Left analog stick, horizontal movement
;; :left-v      -- Left analog stick, vertical movement
;; :right-h     -- Right analog stick, horizontal movement
;; :right-v     -- Right analog stick, vertical movement
;; :dpad-h      -- Directional pad, horizontal movement
;; :dpad-v      -- Directional pad, vertical movement
;; :dpad-up     -- Directional pad up
;; :dpad-right  -- Directional pad right
;; :dpad-down   -- Directional pad down
;; :dpad-left   -- Directional pad left
;; :l1          -- Left upper trigger or bumper
;; :l2          -- Left lower trigger or bumper
;; :r1          -- Right upper trigger or bumper
;; :r2          -- Right lower trigger or bumper
;; :y           -- Upper button (Y on Xbox pads)
;; :b           -- Right button (B on Xbox pads)
;; :a           -- Lower button (A on Xbox pads)
;; :x           -- Left button  (X on Xbox pads)
;; :left        -- Left analog stick click
;; :right       -- Right analog stick click
;; :select      -- Left menu button
;; :home        -- Middle menu button
;; :start       -- Right menu button
;;
;; Gamepads with special hardware may have additional axes
;; and buttons and thus additional names. If you wish to
;; use those, see the respective mapping table for the
;; device

(macrolet ((define-generic-controller (name (vendor id))
             `(define-gamepad ,name (,vendor ,id)
                (:axes ,@(loop for i from 0 to 255
                               collect `(,i ,(intern (format NIL "AXIS-~a" i) :keyword))))
                (:buttons ,@(loop for i from 0 to 255
                                  collect `(,i ,(intern (format NIL "BUTTON-~a" i) :keyword)))))))
  (define-generic-controller generic (0 0)))

(define-gamepad xbox-360 (1118 654)
  (:axes
   ( 0 :left-h)
   ( 1 :left-v)
   ( 2 :l2)
   ( 3 :right-h)
   ( 4 :right-v)
   ( 5 :r2)
   ( 6 :dpad-h)
   ( 7 :dpad-v))
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :select)
   ( 7 :start)
   ( 8 :home)
   ( 9 :left)
   (10 :right)))

(define-gamepad logitech-f310 (1133 49693 :inherit xbox-360)
  (:axes)
  (:buttons))

(define-gamepad dualshock-3 (1356 616)
  (:axes
   ( 0 :left-h)
   ( 1 :left-v)
   ( 2 :right-h)
   ( 3 :right-v)
   ( 8 :dpad-up)
   ( 9 :dpad-right)
   (10 :dpad-down)
   (11 :dpad-left)
   (12 :l2)
   (13 :r2)
   (14 :l1)
   (15 :r1)
   (16 :y) ; triangle
   (17 :b) ; circle
   (18 :a) ; cross
   (19 :x) ; square
   (23 :axis-x)
   (24 :axis-z)
   (25 :axis-y)
   (26 :axis-r))
  (:buttons
   ( 0 :select)
   ( 1 :left)
   ( 2 :right)
   ( 3 :start)
   ( 4 :dpad-up)
   ( 5 :dpad-right)
   ( 6 :dpad-down)
   ( 7 :dpad-left)
   ( 8 :l2)
   ( 9 :r2)
   (10 :l1)
   (11 :r1)
   (12 :y) ; triangle
   (13 :b) ; circle
   (14 :a) ; cross
   (15 :x) ; square
   (16 :home)))

(define-gamepad dualshock-4 (1356 2508)
  (:axes
   ( 0 :left-h)
   ( 1 :left-v)
   ( 2 :l2)
   ( 3 :right-h)
   ( 4 :right-v)
   ( 5 :r2))
  (:buttons
   ( 0 :x)
   ( 1 :a)
   ( 2 :b)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 8 :select)
   ( 9 :start)
   (10 :home)
   (11 :left)
   (12 :right)))

(define-gamepad buffalo-bsgp801 (1411 8288)
  (:axes
   ( 0 :dpad-h)
   ( 1 :dpad-v))
  (:buttons
   ( 0 :b)
   ( 1 :a)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :select)
   ( 7 :start)))

(define-gamepad steam-controller (10462 4604 :inherit xbox-360)
  (:axes)
  (:buttons))

(defun gamepad-axis->symbol (device axis)
  (let ((device (or (gethash (cons (cl-gamepad:vendor device)
                                   (cl-gamepad:product device))
                             *gamepad-device-table*)
                    :generic)))
    (or (gethash axis (gethash device *gamepad-axis-table*))
        axis)))

(defun gamepad-button->symbol (device button)
  (let ((device (or (gethash (cons (cl-gamepad:vendor device)
                                   (cl-gamepad:product device))
                             *gamepad-device-table*)
                    :generic)))
    (or (gethash button (gethash device *gamepad-button-table*))
        button)))
