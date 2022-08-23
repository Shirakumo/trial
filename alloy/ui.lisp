#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass event-bridge ()
  ())

(defmacro define-event-translator (trial-type alloy-type &body args)
  `(defmethod trial:handle ((ev ,trial-type) (bridge event-bridge))
     (alloy:handle (make-instance ',alloy-type ,@args) bridge)))

(defun vec->point (vec)
  (alloy:px-point (3d-vectors:vx vec) (3d-vectors:vy vec)))

(define-event-translator trial:mouse-move alloy:pointer-move
  :old-location (vec->point (trial:old-pos ev))
  :location (vec->point (trial:pos ev)))

(define-event-translator trial:mouse-press alloy:pointer-down
  :kind (trial:button ev)
  :location (vec->point (trial:pos ev)))

(define-event-translator trial:mouse-release alloy:pointer-up
  :kind (trial:button ev)
  :location (vec->point (trial:pos ev)))

(define-event-translator trial:mouse-scroll alloy:scroll
  :dy (trial:delta ev)
  :dx 0
  :location (vec->point (trial:pos ev)))

(define-event-translator trial:text-entered alloy:text-event
  :text (trial:text ev))

(define-event-translator trial:key-press alloy:key-down
  :modifiers (trial:modifiers ev)
  :key (trial:key ev)
  :code 0)

(define-event-translator trial:key-release alloy:key-up
  :modifiers (trial:modifiers ev)
  :key (trial:key ev)
  :code 0)

(define-event-translator trial:gamepad-press alloy:button-down
  :device (trial:device ev)
  :button (trial:button ev))

(define-event-translator trial:gamepad-release alloy:button-up
  :device (trial:device ev)
  :button (trial:button ev))

(defclass ui (renderer event-bridge alloy:ui trial:entity)
  ())

(defmethod trial:render ((ui ui) target)
  (alloy:render ui ui))

(defmethod trial:register :after ((ui ui) (loop trial:event-loop))
  (trial:add-listener ui loop))

(defmethod trial:handle ((ev trial:event) (ui ui)))

(defmethod trial:handle ((ev trial:tick) (ui ui))
  (animation:update ui (float (trial:dt ev) 0f0)))

(defmethod trial:handle ((ev trial:resize) (ui ui))
  (alloy:suggest-size (alloy:px-size (trial:width ev) (trial:height ev)) ui))

(defmethod trial:stage ((ui ui) (area trial:staging-area))
  (trial:stage (alloy:layout-tree ui) area))

(defmethod alloy:clipboard ((ui ui))
  (trial:clipboard trial:*context*))

(defmethod (setf alloy:clipboard) (value (ui ui))
  (setf (trial:clipboard trial:*context*) value))

(defmethod trial:handle :after ((ev trial:key-release) (ui ui))
  (case (trial:key ev)
    (:insert
     (when (find :shift (trial:modifiers ev))
       (alloy:handle (make-instance 'alloy:paste-event :content (alloy:clipboard ui)) ui)))
    (:v
     (when (find :control (trial:modifiers ev))
       (alloy:handle (make-instance 'alloy:paste-event :content (alloy:clipboard ui)) ui)))
    (:c
     (when (find :control (trial:modifiers ev))
       (alloy:handle (make-instance 'alloy:copy-event) ui)))
    (:x
     (when (find :control (trial:modifiers ev))
       (alloy:handle (make-instance 'alloy:cut-event) ui)))))

