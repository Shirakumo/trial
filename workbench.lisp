(in-package #:trial)

(define-pool workbench
  :base 'trial)

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (enter (make-instance 'trial-ui::ui-window
                            :name :ui
                            :extent (vec4 0 0 (width *context*) (height *context*))
                            :layout (make-instance 'trial-ui::horizontal-layout :alignment :top)
                            :color (vec 0.1 0.1 0.1 1)
                            :children (list (make-instance 'trial-ui::spacer
                                                           :preferred-size (vec 0.1 0.1))
                                            (make-instance 'trial-ui::ui-window
                                                           :color (vec 0.2 0.2 0.2 1)
                                                           :layout (make-instance 'trial-ui::vertical-layout)
                                                           :children (list (make-instance 'trial-ui::label
                                                                                          :text "A"
                                                                                          :color (vec 1 0 0 1)
                                                                                          :preferred-size (vec 0.5 0.5))
                                                                           (make-instance 'trial-ui::spacer
                                                                                          :preferred-size (vec 0.1 0.1))
                                                                           (make-instance 'trial-ui::label
                                                                                          :text "B"
                                                                                          :color (vec 0 1 0 1)
                                                                                          :preferred-size (vec 0.7 0.5))
                                                                           (make-instance 'trial-ui::spacer
                                                                                          :preferred-size (vec 0.1 0.1))
                                                                           (make-instance 'trial-ui::label
                                                                                          :text "C"
                                                                                          :color (vec 0 0 1 1)
                                                                                          :preferred-size (vec 2.0 0.5))))
                                            (make-instance 'trial-ui::spacer
                                                           :preferred-size (vec 0.1 0.1))
                                            (make-instance 'trial-ui::label
                                                           :text "Whoa"
                                                           :color (vec 0 1 0 1)
                                                           :preferred-size (vec 1 1.5))
                                            (make-instance 'trial-ui::spacer
                                                           :preferred-size (vec 0.1 0.1))
                                            (make-instance 'trial-ui::label
                                                           :text "Hey there"
                                                           :color (vec 0 0 1 1)
                                                           :preferred-size (vec 1 0.2))
                                            (make-instance 'trial-ui::spacer
                                                           :preferred-size (vec 0.1 0.1))))
             scene)
      (enter (make-instance '2d-camera) scene)))

  (maybe-reload-scene))

(define-handler (controller resize) (ev width height)
  (let ((ui (unit :ui *loop*)))
    (setf (vz (trial-ui::extent ui)) (/ width 2))
    (setf (vw (trial-ui::extent ui)) (/ height 2))
    (trial-ui::note-extent-change ui nil)))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))

