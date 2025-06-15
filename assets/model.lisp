(in-package #:org.shirakumo.fraf.trial)

(define-standard-save-function save-model)
(define-standard-load-function load-model)

(defgeneric optimize-model (source type &key &allow-other-keys))

(defmethod optimize-model (source (type string) &rest args &key &allow-other-keys)
  (apply #'optimize-model source (ensure-file-type type) args))

(defmethod optimize-model (source (type (eql T)) &rest args &key &allow-other-keys)
  (apply #'optimize-model source (ensure-file-type source) args))

(defclass model-loader (compiled-generator)
  ())

(defmethod generate-resources ((loader model-loader) input &rest args &key load-scene)
  (with-new-value-restart (input) (use-value "Specify a new model source.")
    (with-retry-restart (retry "Retry loading the model source.")
      (let ((model (apply #'load-model input T :generator loader args)))
        (flet ((load-scene (value)
                 (enter value (scene +main+))))
          (etypecase load-scene
            (null)
            ((eql T)
             (loop for scene being the hash-values of (scenes model)
                   do (load-scene scene)))
            ((or string symbol)
             (load-scene (find-scene load-scene model)))))
        model))))

(defmethod compile-resources ((loader model-loader) input &rest args &key &allow-other-keys)
  (apply #'optimize-model input T args))

(defclass model-file (file-input-asset multi-resource-asset model-loader model)
  ())

(defmethod generate-resources ((asset model-file) input &rest args)
  (apply #'call-next-method asset input :model asset args)
  (list-resources asset))

(defmethod unload :after ((asset model-file))
  (clear asset))
