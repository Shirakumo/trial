(in-package #:org.shirakumo.fraf.trial)

(defclass static (asset)
  ())

(defmethod coerce-asset-input ((static static) input)
  (check-type input resource)
  input)

(defmethod generate-resources ((generator static) (input resource) &key)
  input)

(defmethod generate-resources ((generator static) (input symbol) &rest args &key &allow-other-keys)
  (apply #'make-instance input args))

(defmethod list-resources ((generator static))
  (list (input generator)))

(defmethod resource ((generator static) (name (eql T)))
  (input generator))

(defmethod unload ((generator static))
  (when (allocated-p (input generator))
    (deallocate (input generator))))
