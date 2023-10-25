(in-package #:org.shirakumo.fraf.trial)

(defgeneric load-model (source type &key generator &allow-other-keys))
(defgeneric save-model (source target type &key &allow-other-keys))

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

(defclass model-loader (resource-generator)
  ())

(defmethod generate-resources ((loader model-loader) input &rest args)
  (with-new-value-restart (input) (use-value "Specify a new model source.")
    (with-retry-restart (retry "Retry loading the model source.")
      (apply #'load-model input T :generator loader args))))

(defclass model-file (file-input-asset multi-resource-asset model-loader)
  ((model :initform NIL :accessor model)))

(defmethod generate-resources ((asset model-file) input &key load-scene)
  (let ((model (call-next-method)))
    (setf (model asset) model)
    (flet ((load-scene (scene)
             (enter scene (scene +main+))))
      (etypecase load-scene
        (null)
        ((eql T)
         (loop for scene being the hash-values of (scenes asset)
               do (load-scene scene)))
        ((or string symbol)
         (load-scene (find-scene load-scene model)))))
    (alexandria:hash-table-values (meshes model))))

(defmethod unload :after ((asset model-file))
  (setf (model asset) NIL))

(defmacro %define-model-file-delegate (name read)
  (let ((find (intern (format NIL "~a-~a" 'find name))))
    `(progn
       (defmethod ,read ((asset model-file))
         (check-loaded asset)
         (,read (model asset)))
       
       (defmethod ,find (name (asset model-file) &optional (errorp T))
         (check-loaded asset)
         (,find name (model asset) errorp))
       
       (defmethod (setf ,find) (value name (asset model-file))
         (check-loaded asset)
         (setf (,find name (model asset)) value)))))

(%define-model-file-delegate material materials)
(%define-model-file-delegate mesh meshes)
(%define-model-file-delegate clip clips)
(%define-model-file-delegate scene scenes)

(defmethod skeleton ((asset model-file))
  (check-loaded asset)
  (skeleton (model asset)))

(defmethod node (name (asset model-file))
  (check-loaded asset)
  (node name (model asset)))
