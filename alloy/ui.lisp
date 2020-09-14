#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass ui (renderer alloy:ui trial:entity)
  ())

(defmethod trial:render ((ui ui) target)
  (alloy:render ui ui))

(defmethod trial:register :after ((ui ui) (loop trial:event-loop))
  (trial:add-listener ui loop))

(defmethod trial:handle ((ev trial:event) (ui ui)))

(defmethod trial:handle ((ev trial:resize) (ui ui))
  (alloy:suggest-bounds (alloy:px-extent 0 0 (trial:width ev) (trial:height ev)) ui))

(defmethod trial:stage ((ui ui) (area trial:staging-area))
  (trial:stage (alloy:layout-tree ui) area))

(defmacro define-event-translator (trial-type alloy-type &body args)
  `(defmethod trial:handle ((ev ,trial-type) (ui ui))
     (alloy:handle (make-instance ',alloy-type ,@args) ui)))

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

;; paste-event?
