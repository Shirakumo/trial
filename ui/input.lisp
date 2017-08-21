#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(define-action ui-action ())

(define-action activate-next (ui-action)
  (mouse-scroll (< 0 delta))
  (key-release (one-of key :down :s))
  (gamepad-release (eql button :dpad-down))
  (gamepad-move (and (eql axis :left-v) (< old-pos -0.5 pos))))

(define-action activate-previous (ui-action)
  (mouse-scroll (< delta 0))
  (key-release (one-of key :up :w))
  (gamepad-release (eql button :dpad-up))
  (gamepad-move (and (eql axis :left-v) (< pos 0.5 old-pos))))

(define-action next-value (ui-action)
  (key-release (one-of key :right :d))
  (gamepad-release (eql button :dpad-right))
  (gamepad-move (and (eql axis :left-h) (< pos 0.5 old-pos))))

(define-action previous-value (ui-action)
  (key-release (one-of key :left :a))
  (gamepad-release (eql button :dpad-left))
  (gamepad-move (and (eql axis :left-h) (< old-pos -0.5 pos))))

(define-action confirm (ui-action)
  (key-release (eql key :return))
  (gamepad-release (eql button :a)))

(define-action cancel (ui-action)
  (key-release (eql key :escape))
  (gamepad-release (eql button :b)))

(defmethod handle ((event event) (ui-element ui-element)))

(defmethod handle :around ((event event) (ui-element ui-element))
  (when (focused-p ui-element)
    (call-next-method)))

(defmethod handle ((event event) (pane pane))
  (loop for child across (children pane)
        when (focused-p child)
        return (handle event child)))

(defmethod handle ((event activate-next) (pane pane))
  (let* ((children (children pane))
         (focus (position T children :key #'focused-p)))
    (cond ((and (active-p pane) focus)
           (setf (focused-p (aref children focus)) NIL)
           (loop do (setf focus (mod (1+ focus) (length children)))
                 until (setf (focused-p (aref children focus)) T)))
          (T (call-next-method)))))

(defmethod handle ((event activate-previous) (pane pane))
  (let* ((children (children pane))
         (focus (position T children :key #'focused-p)))
    (cond ((and (active-p pane) focus)
           (setf (focused-p (aref children focus)) NIL)
           (loop do (setf focus (mod (1- focus) (length children)))
                 until (setf (focused-p (aref children focus)) T)))
          (T (call-next-method)))))

(defmethod handle ((event confirm) (pane pane))
  (let* ((children (children pane))
         (active (when (active-p pane) 0)))
    (cond ((and active (/= 0 (length children)))
           (setf (active-p pane) NIL)
           (loop do (setf active (mod (1+ active) (length children)))
                 until (setf (active-p (aref children active)) T)))
          (T
           (call-next-method)))))

(defmethod handle ((event cancel) (ui-element ui-element))
  (cond ((active-p ui-element)
         (when (parent ui-element)
           (setf (active-p ui-element) NIL)
           (setf (active-p (parent ui-element)) T)))
        (T
         (call-next-method))))
