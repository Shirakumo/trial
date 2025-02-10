(in-package #:org.shirakumo.fraf.trial.examples)

(define-example input
  :title "Input Events"
  :description ""
  :slots ((events :accessor events))
  (enter (make-instance '2d-camera) scene)
  (enter (make-instance 'render-pass) scene))

(define-handler (input-scene mouse-event) (pos)
  (debug-draw pos :size 16
                  :color (typecase mouse-event
                           (mouse-press #.(vec 1 0 0))
                           (mouse-release #.(vec 0 1 0))
                           (mouse-double-click #.(vec 1 1 0))
                           (T #.(vec 1 1 1)))))

(define-handler (input-scene input-event :after) ()
  (let ((label (prompt-string input-event)))
    (when label
      (alloy:enter (make-instance 'input-label :value label)
                   (events input-scene)))))

(defmethod setup-ui ((scene input-scene) panel)
  (let* ((layout (make-instance 'alloy:vertical-linear-layout))
         (focus (make-instance 'alloy:vertical-focus-list))
         (keyboard (make-instance 'alloy:virtual-keyboard :layout-parent layout :focus-parent focus))
         (events (make-instance 'alloy:flow-layout :layout-parent layout :min-size (alloy:size 50 50))))
    (setf (events scene) events)
    (alloy:finish-structure panel layout focus)))

(defclass input-label (alloy:label*)
  ((timeout :initform 2.0 :accessor timeout)
   (alloy:sizing-strategy :initform (load-time-value (make-instance 'alloy:proportional)))))

(defmethod animation:update :after ((label input-label) dt)
  (when (<= (decf (timeout label) dt) 0)
    (alloy:leave label T))
  (alloy:mark-for-render label))

(presentations:define-realization (ui input-label)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern colors:white)
  ((:label simple:text)
   (alloy:margins 2) alloy:text
   :halign :middle
   :valign :middle
   :font "PromptFont"
   :pattern colors:white
   :size (alloy:un 24)))

(presentations:define-update (ui input-label)
  (:background
   :pattern (let ((gray (max 0.0 (1- (timeout alloy:renderable)))))
              (colored:color gray gray gray))))
