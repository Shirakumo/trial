(in-package #:org.shirakumo.fraf.trial.particle-studio)

(defvar *emitter-counter* 1)

(define-shader-pass ui (trial-alloy:base-ui)
  ())

(defmethod setup-ui ((ui ui))
  (trial-alloy:show-panel 'base-panel))

(defclass base-panel (trial-alloy:fullscreen-panel)
  ((sidebar :accessor sidebar)))

(defmethod initialize-instance :after ((panel base-panel) &key)
  (let* ((focus (make-instance 'alloy:focus-list))
         (layout (make-instance 'alloy:border-layout))
         (menu (alloy:with-menu
                 ("File"
                  ("New")
                  ("Load...")
                  :separator
                  ("Save")
                  ("Save As...")
                  :separator
                  ("Quit" (quit *context*)))
                 ("Edit"
                  ("Add Emitter" (add-emitter panel))))))
    (setf (sidebar panel) (make-instance 'alloy:section-list))
    (let ((side (make-instance 'alloy:sidebar :focus-parent focus :layout (sidebar panel))))
      (alloy:enter side layout :place :west :size (alloy:un 350)))
    (alloy:enter menu layout :place :north :size (alloy:un 30))
    (alloy:enter menu focus)
    (alloy:finish-structure panel layout focus)))

(defmethod add-emitter ((panel base-panel))
  (let ((emitter (make-instance 'cpu-particle-emitter
                                :particle-rate 1.0
                                :name (generate-name "EMITTER"))))
    (enter-and-load emitter (scene +main+) +main+)
    (alloy:enter (make-instance 'emitter-widget :emitter emitter)
                 (sidebar panel) :label (string (name emitter)))
    emitter))

(defclass emitter-widget (alloy:structure)
  ((emitter :initarg :emitter :initform NIL :accessor emitter)))

(defmethod initialize-instance :after ((panel emitter-widget) &key emitter)
  (let ((layout (make-instance 'alloy:vertical-linear-layout))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(160 T) :row-sizes '(20) :cell-margins (alloy:margins 2)
                                                    :layout-parent layout))
          (row -1))
      (macrolet ((wheel (place title start end &rest args)
                   `(progn
                      (alloy:enter ,title layout :row (incf row) :col 0)
                      (alloy:represent (,place emitter) 'alloy:ranged-wheel
                                       :range '(,start . ,end) ,@args :layout-parent layout :focus-parent focus))))
        (let* ((burst 10)
               (button (make-instance 'alloy:button* :value "Burst" :focus-parent focus :on-activate
                                      (lambda () (emit emitter burst)))))
          (alloy:enter button layout :row (incf row) :col 0)
          (alloy:represent burst 'alloy:ranged-wheel :range '(1 . 200) :layout-parent layout :focus-parent focus))
        (wheel particle-rate "Particle Rate" 0 10000)
        (wheel particle-lifespan "Lifespan" 0.0 100.0 :step 0.1)
        (wheel particle-lifespan-randomness "Lifespan Random" 0.0 1.0 :step 0.1)
        (wheel particle-velocity "Velocity" 0.0 100.0)
        (wheel particle-randomness "Randomness" 0.0 1.0 :step 0.1)
        (wheel particle-size "Size" 0.01 10.0 :step 0.1)
        (wheel particle-scaling "Scaling" 0.0 10.0 :step 0.1)
        (wheel particle-rotation "Rotation" 0.0 10.0 :step 0.1)
        (wheel particle-motion-blur "Motion Blur" 0.0 1.0 :step 0.1)
        (alloy:enter "Texture" layout :row (incf row) :col 0)
        (alloy:represent (texture emitter) T :layout-parent layout :focus-parent focus)
        (alloy:enter "Display Mode" layout :row (incf row) :col 0)
        (alloy:represent (particle-mode emitter) 'alloy:combo-set
                         :value-set '(:quad :billboard) :layout-parent layout :focus-parent focus)
        (alloy:enter "Blend Mode" layout :row (incf row) :col 0)
        (alloy:represent (blend-mode emitter) 'alloy:combo-set
                         :value-set '(:add :normal :invert :darken :multiply :screen) :layout-parent layout :focus-parent focus)
        (alloy:enter "Texture Flip" layout :row (incf row) :col 0)
        (alloy:represent (particle-flip emitter) 'alloy:combo-set
                         :value-set '(NIL :x :y T) :layout-parent layout :focus-parent focus)
        (alloy:enter "Color" layout :row (incf row) :col 0)
        (let* ((color (particle-color emitter))
               (c (alloy:represent color T :layout-parent layout :focus-parent focus)))
          (alloy:on alloy:value (v c)
            (setf (particle-color emitter) color)))
        (alloy:enter "Emitter Shape" layout :row (incf row) :col 0)
        (let* ((shape :square)
               (c (alloy:represent shape 'alloy:combo-set
                                   :value-set '(:square :disc :sphere :cube :point) :layout-parent layout :focus-parent focus)))
          (alloy:on alloy:value (v c)
            (setf (vertex-array emitter)
                  (ecase v
                    (:square (// 'trial 'unit-square))
                    (:disc (// 'trial 'unit-disc))
                    (:sphere (// 'trial 'unit-sphere))
                    (:cube (// 'trial 'unit-cube))
                    (:point (// 'trial 'point))))))
        (alloy:enter "Force Fields" layout :row (incf row) :col 0)
        (make-instance 'alloy:button* :value "Add" :focus-parent focus :on-activate
                       (lambda () (add-force-field panel)))))
    (let ((sections (make-instance 'alloy:section-list :layout-parent layout :focus-parent focus))
          (fields (particle-force-fields emitter)))
      (loop for i from 0 below (trial::particle-force-field-count fields)
            for field = (aref (trial::particle-force-fields fields) i)
            for widget = (make-instance 'force-field-widget :field field :i (1+ i) :emitter emitter)
            do (alloy:enter widget sections :label (format NIL "Force Field ~d" i))))
    (alloy:finish-structure panel layout focus)))

(defmethod add-force-field ((widget emitter-widget))
  (let ((sections (make-instance 'alloy:section-list :layout-parent layout :focus-parent focus))
        (fields (particle-force-fields emitter)))
    (loop for i from 0 below (trial::particle-force-field-count fields)
          for field = (aref (trial::particle-force-fields fields) i)
          for widget = (make-instance 'force-field-widget :field field :i (1+ i) :emitter emitter)
          do (alloy:enter widget sections :label (format NIL "Force Field ~d" i)))))
