(in-package #:org.shirakumo.fraf.trial)

(defclass static (single-resource-asset)
  ())

(defmethod shared-initialize :after ((static static) slots &key)
  (when (typep (input static) 'resource)
    (setf (slot-value static 'resource) (input static))))

(defmethod coerce-asset-input ((static static) input)
  input)

(defmethod generate-resources ((generator static) (input resource) &key)
  input)

(defmethod generate-resources ((generator static) (input symbol) &rest args &key &allow-other-keys)
  (apply #'ensure-instance (slot-value generator 'resource) input args))

(defmethod unload ((generator static))
  (when (allocated-p (slot-value generator 'resource))
    (deallocate (slot-value generator 'resource))))
