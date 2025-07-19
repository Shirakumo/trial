(in-package #:org.shirakumo.fraf.trial.particle-studio)

(define-shader-pass ui (trial-alloy:base-ui)
  ())

(defmethod setup-ui ((ui ui))
  )

(defclass force-field-widget (alloy:structure)
  ())

(defmethod initialize-instance :after ((widget force-field-widget) &key field i layout-parent focus-parent emitter)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(120 T) :row-sizes '(30) :layout-parent layout-parent))
        (focus (make-instance 'alloy:vertical-focus-list :focus-parent focus-parent))
        (row -1))
    (alloy:enter (format NIL "Field #~a" i) layout :row (incf row) :col 0)
    (macrolet ((field (label &rest represent)
                 `(let ((label (alloy:enter ,label layout :row (incf row) :col 0))
                        (field (alloy:represent ,@represent
                                                :layout-parent layout :focus-parent focus)))
                    (declare (ignore label))
                    (alloy:on alloy:value (value field)
                      (declare (ignore value))
                      (setf (particle-force-fields emitter) (particle-force-fields emitter))))))
      (field "Type" (slot-value field 'type) 'alloy:combo-set
             :value-set (loop for type in (trial::list-particle-force-field-types)
                              collect (cons (trial::id type) (string-capitalize (name type)))))
      (field "Position" (slot-value field 'position) T
             :layout-parent layout :focus-parent focus)
      (field "Normal" (slot-value field 'normal) T
             :layout-parent layout :focus-parent focus)
      (field "Strength" (slot-value field 'strength) 'alloy:ranged-wheel
             :range '(-100.0 . 100.0) :layout-parent layout :focus-parent focus)
      (alloy:enter "Range" layout :row (incf row) :col 0)
      (let ((range (alloy:represent (slot-value field 'trial::range) 'alloy:ranged-wheel
                                    :range '(0.0 . 1000.0) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (v range)
          (setf (slot-value field 'trial::inv-range) (if (= 0.0 v) 0.0 (/ v)))
          (setf (particle-force-fields emitter) (particle-force-fields emitter)))))
    (alloy:finish-structure widget layout focus)))

(defclass emitter-widget (alloy:structure)
  ())

(defmethod initialize-instance :after ((panel emitter-widget) &key emitter)
  (let ((layout (make-instance 'grid-layout* :col-sizes '(120 T) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list))
        (row -1))
    (alloy:enter layout constraint :constraints `((:right 0) (:top 0) (:width 300)))
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
      (wheel particle-lifespan "Lifespan" 0.0 100.0)
      (wheel particle-lifespan-randomness "Lifespan Random" 0.0 1.0)
      (wheel particle-velocity "Velocity" 0.0 100.0)
      (wheel particle-randomness "Randomness" 0.0 1.0)
      (wheel particle-size "Size" 0.01 10.0)
      (wheel particle-scaling "Scaling" 0.0 10.0)
      (wheel particle-rotation "Rotation" 0.0 10.0)
      (wheel particle-motion-blur "Motion Blur" 0.0 1.0)
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
                  (:point (// 'trial 'point))))))))
  ;; Force fields panel
  (let ((layout (make-instance 'grid-layout* :col-sizes '(T) :row-sizes '(200)))
        (focus (make-instance 'alloy:vertical-focus-list :focus-parent focus))
        (fields (particle-force-fields emitter)))
    (alloy:enter layout constraint :constraints `((:top 0) (:right 300) (:width 300) (:height 400)))
    (loop for i from 0 below (trial::particle-force-field-count fields)
          for field = (aref (trial::particle-force-fields fields) i)
          do (make-instance 'force-field-widget :field field :i (1+ i) :layout-parent layout :focus-parent focus :emitter emitter)))
  (alloy:finish-structure panel constraint focus))

