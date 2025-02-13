(in-package #:org.shirakumo.fraf.trial.examples)

(define-example input
  :title "Input Events"
  :description "Shows input events that Trial receives."
  :slots ((events :accessor events)
          (gamepads :accessor gamepads))
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

(define-handler (input-scene gamepad-removed :after) (device)
  (alloy:do-elements (element (gamepads input-scene))
    (when (eql device (alloy:value element))
      (alloy:leave element T))))

(define-handler (input-scene gamepad-added :after) (device)
  (alloy:enter (make-instance 'gamepad-label :value device) (gamepads input-scene)))

(defmethod setup-ui ((scene input-scene) panel)
  (let* ((layout (make-instance 'alloy:vertical-linear-layout))
         (focus (make-instance 'alloy:vertical-focus-list))
         (keyboard (make-instance 'alloy:virtual-keyboard :layout-parent layout :focus-parent focus))
         (gamepads (make-instance 'alloy:vertical-linear-layout :layout-parent layout))
         (events (make-instance 'alloy:flow-layout :layout-parent layout :min-size (alloy:size 50 50))))
    (setf (events scene) events)
    (setf (gamepads scene) gamepads)
    (dolist (device (org.shirakumo.fraf.gamepad:list-devices))
      (alloy:enter (make-instance 'gamepad-label :value device) gamepads))
    (alloy:finish-structure panel layout focus)))

(defclass gamepad-label (alloy:direct-value-component)
  ())

(defmethod alloy:text ((label gamepad-label))
  (let ((device (alloy:value label)))
    (format NIL "~@[~a~]~@[:~a~]~@[:~a~]~@[ ~a~]~@[ ~a~]"
            (org.shirakumo.fraf.gamepad:vendor device)
            (org.shirakumo.fraf.gamepad:product device)
            (org.shirakumo.fraf.gamepad:version device)
            (org.shirakumo.fraf.gamepad:name device)
            (org.shirakumo.fraf.gamepad:driver device))))

(presentations:define-realization (ui gamepad-label)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern (colored:color 0.2 0.2 0.2))
  ((:label simple:text)
   (alloy:margins 5) alloy:text
   :halign :left
   :valign :middle
   :pattern colors:white
   :size (alloy:un 18)))

(defclass input-label (alloy:direct-value-component)
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
   (alloy:margins 5) alloy:text
   :halign :middle
   :valign :middle
   :font "PromptFont"
   :pattern colors:white
   :size (alloy:un 25)))

(presentations:define-update (ui input-label)
  (:background
   :pattern (let ((gray (max 0.0 (1- (timeout alloy:renderable)))))
              (colored:color gray gray gray))))
