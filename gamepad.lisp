#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass gamepad-input-handler ()
  ((last-device-probe :initform 0 :accessor last-device-probe)))

(defmethod start :after ((handler gamepad-input-handler))
  (gamepad:init))

(defmethod stop :after ((handler gamepad-input-handler))
  (gamepad:shutdown))

(defmethod poll-input :after ((handler gamepad-input-handler))
  (handler-bind ((gamepad:gamepad-error (lambda (e)
                                          (invoke-restart 'gamepad:drop-device))))
    (flet ((process (event)
             (typecase event
               (gamepad:button-down
                (handle (make-instance 'gamepad-press
                                       :button (or (gamepad:event-label event)
                                                   (gamepad:event-code event))
                                       :device (gamepad:event-device event))
                        handler))
               (gamepad:button-up
                (handle (make-instance 'gamepad-release
                                       :button (or (gamepad:event-label event)
                                                   (gamepad:event-code event))
                                       :device (gamepad:event-device event))
                        handler))
               (gamepad:axis-move
                (handle (make-instance 'gamepad-move
                                       :pos (gamepad:event-value event)
                                       :old-pos (gamepad:event-old-value event)
                                       :axis (or (gamepad:event-label event)
                                                 (gamepad:event-code event))
                                       :device (gamepad:event-device event))
                        handler)))))
      (dolist (device (gamepad:list-devices))
        (gamepad:poll-events device #'process))))
  (when (< internal-time-units-per-second
           (- (get-internal-real-time) (last-device-probe handler)))
    (setf (last-device-probe handler) (get-internal-real-time))
    (gamepad:poll-devices)))
