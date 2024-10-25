(in-package #:org.shirakumo.fraf.trial)

(defmethod deallocate (data)
  (typecase data
    (cffi:foreign-pointer
     (cffi:foreign-free data))
    (T
     (no-applicable-method #'deallocate data))))

(defmethod deallocate ((mem memory-region))
  (mem:deallocate T mem))

(defmethod deallocate ((source texture-source))
  (deallocate (pixel-data source)))

(defmethod deallocate ((list list))
  (mapc #'deallocate list))

(macrolet ((emit (type)
             (dolist (type (org.shirakumo.type-templates:instances type))
               `(defmethod mem:call-with-memory-region (function (data ,(org.shirakumo.type-templates:lisp-type type)) &rest args)
                  (apply #'mem:call-with-memory-region function ,(org.shirakumo.type-templates:place-form type :arr 'data) args)))))
  (emit org.shirakumo.fraf.math.internal:vec-type)
  (emit org.shirakumo.fraf.math.internal:mat-type)
  (emit org.shirakumo.fraf.math.internal:quat-type))
