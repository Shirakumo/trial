#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass ui (renderer alloy:ui trial:entity)
  ())

(defmethod trial:paint ((ui ui) target)
  (alloy:render ui T))

(defmethod trial:register :after ((ui ui) (loop trial:event-loop))
  (trial:add-handler ui loop))

(defmethod trial:handle ((ev trial:event) (ui ui)))

(defmethod trial:handle ((ev trial:resize) (ui ui))
  (alloy:suggest-bounds (alloy:extent 0 0 (trial:width ev) (trial:height ev)) ui))

(defmacro define-event-translator (trial-type alloy-type &body args)
  `(defmethod trial:handle ((ev ,trial-type) (ui ui))
     (alloy:handle (make-instance ',alloy-type ,@args) T ui)))

(defun vec->point (vec)
  (alloy:point (vx vec) (vy vec)))

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
  :delta (trial:delta ev)
  :location (vec->point (trial:pos ev)))
