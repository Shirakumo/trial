(in-package #:trial)

(define-pool workbench
  :base 'trial)

(progn
  (defmethod setup-scene ((main main) scene)
    (let ((window (make-instance 'trial-ui::ui-window
                                 :name :ui
                                 :extent (vec4 0 0 (width *context*) (height *context*))
                                 :layout (make-instance 'trial-ui::horizontal-layout :alignment :top)))
          (context (make-instance 'trial-ui::ui-context)))
      (enter (make-instance 'trial-ui::spacer :preferred-size (vec 0.1 0.1)) window)
      (enter (make-instance 'trial-ui::text-field :text "1") window)
      (enter (make-instance 'trial-ui::spacer :preferred-size (vec 0.1 0.1)) window)
      (enter (make-instance 'trial-ui::text-field :text "2") window)
      (enter (make-instance 'trial-ui::spacer :preferred-size (vec 0.1 0.1)) window)
      (enter window scene)
      (enter window context)
      (add-handler context scene))
    (enter (make-instance '2d-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))

(define-handler (controller resize) (ev width height)
  (let ((ui (unit :ui *loop*)))
    (setf (vz (extent ui)) width)
    (setf (vw (extent ui)) height)
    (trial-ui::note-extent-change ui nil)))
