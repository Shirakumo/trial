(in-package #:org.shirakumo.fraf.trial)

(defgeneric load-model (source type &key generator model &allow-other-keys))
(defgeneric save-model (source target type &key &allow-other-keys))
(defgeneric optimize-model (source type &key &allow-other-keys))

(defmethod load-model (source (type string) &rest args &key &allow-other-keys)
  (apply #'load-model source (or (cl-ppcre:register-groups-bind (type) ("^[^/]*/([^+/]+)" type) (kw type)) (kw type)) args))

(defmethod load-model ((source pathname) (type (eql T)) &rest args &key &allow-other-keys)
  (apply #'load-model source (pathname-type source) args))

(defmethod load-model (source (type symbol) &key &allow-other-keys)
  (let ((types (delete T (list-eql-specializers #'load-model 1))))
    (if (find type types)
        (error "Don't know how to load~%  ~a~%from ~a"
               source type)
        (error "Don't know how to load from ~a~%known types are:~%  ~a~%Did you load the respective format system?"
               type types))))

(defmethod save-model (source target (type string) &rest args &key &allow-other-keys)
  (apply #'save-model source target (or (cl-ppcre:register-groups-bind (type) ("^[^/]*/([^+/]+)" type) (kw type)) (kw type)) args))

(defmethod save-model (source (target pathname) (type (eql T)) &rest args &key &allow-other-keys)
  (apply #'save-model source target (pathname-type source) args))

(defmethod save-model (source target (type symbol) &key &allow-other-keys)
  (let ((types (delete T (list-eql-specializers #'save-model 2))))
    (if (find type types)
        (error "Don't know how to save~%  ~a~%to ~a~%  ~a~%"
               source type target)
        (error "Don't know how to save to ~a~%known types are:~%  ~a~%Did you load the respective format system?"
               type types))))

(defmethod optimize-model (source (type string) &rest args &key &allow-other-keys)
  (apply #'optimize-model source (or (cl-ppcre:register-groups-bind (type) ("^[^/]*/([^+/]+)" type) (kw type)) (kw type)) args))

(defmethod optimize-model (source (type (eql T)) &rest args &key &allow-other-keys)
  (apply #'optimize-model source (pathname-type source) args))

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
