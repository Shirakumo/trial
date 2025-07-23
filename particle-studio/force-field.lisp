(in-package #:org.shirakumo.fraf.trial.particle-studio)

(defclass force-field (alloy:structure)
  ((emitter :initarg :emitter :accessor emitter)
   (i :initarg :i :accessor i)))

(defmethod initialize-instance :after ((widget force-field) &key i layout-parent focus-parent emitter)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(160 T) :row-sizes '(20) :cell-margins (alloy:margins 2)
                                                  :layout-parent layout-parent))
        (focus (make-instance 'alloy:vertical-focus-list :focus-parent focus-parent))
        (field (aref (particle-force-fields emitter) i))
        (row -1))
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
