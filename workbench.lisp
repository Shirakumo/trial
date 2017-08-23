(in-package #:trial)

(define-pool workbench
  :base 'trial)

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (let ((window (make-instance 'trial-ui::ui-window
                                   :name :ui
                                   :extent (vec4 0 0 (width *context*) (height *context*))
                                   :layout (make-instance 'trial-ui::horizontal-layout :alignment :top)
                                   :color (vec 0.1 0.1 0.1 1))))
        (enter (make-instance 'trial-ui::spacer :preferred-size (vec 0.1 0.1)) window)
        (enter (make-instance 'trial-ui::text-field) window)
        (enter (make-instance 'trial-ui::spacer :preferred-size (vec 0.1 0.1)) window)
        (let ((subwindow (make-instance 'trial-ui::ui-window
                                        :color (vec 0.2 0.2 0.2 1)
                                        :layout (make-instance 'trial-ui::vertical-layout))))
          (enter (make-instance 'trial-ui::text-field :text "A") subwindow)
          (enter (make-instance 'trial-ui::spacer :preferred-size (vec 0.1 0.1)) subwindow)
          (enter (make-instance 'trial-ui::text-field :text "B") subwindow)
          (enter (make-instance 'trial-ui::spacer :preferred-size (vec 0.1 0.1)) subwindow)
          (enter (make-instance 'trial-ui::text-field :text "C") subwindow)
          (enter subwindow window))
        (enter (make-instance 'trial-ui::spacer :preferred-size (vec 0.1 0.1)) window)
        (enter (make-instance 'trial-ui::label :text "2") window)
        (enter (make-instance 'trial-ui::spacer :preferred-size (vec 0.1 0.1)) window)
        (enter window scene))
      (enter (make-instance '2d-camera) scene)))

  (maybe-reload-scene))

(define-handler (controller resize) (ev width height)
  (let ((ui (unit :ui *loop*)))
    (setf (vz (extent ui)) width)
    (setf (vw (extent ui)) height)
    (trial-ui::note-extent-change ui nil)))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))

