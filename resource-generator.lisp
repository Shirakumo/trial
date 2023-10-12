(in-package #:org.shirakumo.fraf.trial)

(defclass resource-generator ()
  ())

(defgeneric generate-resources (generator input &key &allow-other-keys))
(defgeneric resource (generator identifier))

(defmethod resource ((generator resource-generator) id) NIL)

(defmethod generate-resources ((generator symbol) input &rest args)
  (apply #'generate-resources (make-instance generator) input args))

(defclass compiled-generator (resource-generator)
  ())

(defgeneric compile-resources (generator source &key))

(defmethod generate-resources :before ((generator compiled-generator) source &key compile)
  (when compile
    (compile-resources generator source)))

(defun recompile-needed-p (targets sources)
  (let ((latest (loop for source in (enlist sources)
                      maximize (file-write-date source))))
    (loop for target in (enlist targets)
          thereis (or (null (probe-file target))
                      (< (file-write-date target) latest)))))
