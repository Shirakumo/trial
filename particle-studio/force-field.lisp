(in-package #:org.shirakumo.fraf.trial.particle-studio)

(defclass force-field (alloy:structure)
  ((emitter :initarg :emitter :accessor emitter)
   (i :accessor i)))

(defmethod initialize-instance :after ((widget force-field) &key i emitter)
  (let* ((i (or i (particle-force-field-count (particle-force-fields emitter))))
         (layout (make-instance 'alloy:grid-layout :col-sizes '(160 T) :row-sizes '(20) :cell-margins (alloy:margins 2)))
         (focus (make-instance 'alloy:vertical-focus-list))
         (field (aref (particle-force-fields (particle-force-fields emitter)) i))
         (row -1))
    (setf (i widget) i)
    (macrolet ((field (label slot &rest represent)
                 `(let ((label (alloy:enter ,label layout :row (incf row) :col 0))
                        (component (alloy:represent ,slot ,@represent
                                                    :layout-parent layout :focus-parent focus)))
                    (declare (ignore label))
                    (alloy:on alloy:value (value component)
                      (declare (ignorable value))
                      (setf ,slot value)
                      (setf (particle-force-field-list emitter) (particle-force-field-list emitter))))))
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
    (alloy:finish-structure widget layout focus)
    ;; Register
    (enter widget emitter)))

(defmethod enter ((field force-field) (emitter cpu-particle-emitter))
  (setf (particle-force-field-count (particle-force-fields emitter))
        (max (particle-force-field-count (particle-force-fields emitter))
             (1+ (i field))))
  (alloy:enter field (sections emitter) :label (format NIL "Force Field ~d" (1+ (i field)))))

(defun delete-nth (i list)
  (if (= 0 i)
      (rest list)
      (let ((cons (nthcdr (1- i) list)))
        (when (rest cons)
          (setf (cdr cons) (cddr cons)))
        list)))

(defmethod leave ((field force-field) (emitter cpu-particle-emitter))
  (setf (particle-force-field-list emitter)
        (delete-nth (i field) (particle-force-field-list emitter)))
  ;; We have to reconstruct all the fields. Sad, I know.
  (alloy:clear (sections emitter))
  (loop for i from 0 below (particle-force-field-count (particle-force-fields emitter))
        do (make-instance 'force-field :emitter emitter :i i)))
