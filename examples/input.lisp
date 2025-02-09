(in-package #:org.shirakumo.fraf.trial.examples)

(define-example input
  :title "Input Events"
  :description ""
  (enter (make-instance '2d-camera) scene)
  (enter (make-instance 'render-pass) scene))

(define-handler (input-scene mouse-event) (pos)
  (debug-draw pos :size 16
                  :color (typecase mouse-event
                           (mouse-press #.(vec 1 0 0))
                           (mouse-release #.(vec 0 1 0))
                           (mouse-double-click #.(vec 1 1 0))
                           (T #.(vec 1 1 1)))))

(defmethod setup-ui ((scene input-scene) panel)
  (let* ((layout (make-instance 'alloy:vertical-linear-layout))
         (focus (make-instance 'alloy:vertical-focus-list)))
    (make-instance 'alloy:virtual-keyboard :layout-parent layout :focus-parent focus)
    (alloy:finish-structure panel layout focus)))

