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

(defgeneric transcode (source source-type target target-type &key))

(defmethod transcode (source (source-type (eql T)) target target-type &rest args &key &allow-other-keys)
  (apply #'transcode source (ensure-file-type source) target target-type args))

(defmethod transcode (source source-type target (target-type (eql T)) &rest args &key &allow-other-keys)
  (apply #'transcode source source-type target (ensure-file-type target) args))

(defmethod transcode (source (source-type string) target target-type &rest args &key &allow-other-keys)
  (apply #'transcode source (ensure-file-type source-type) target target-type args))

(defmethod transcode (source source-type target (target-type string) &rest args &key &allow-other-keys)
  (apply #'transcode source source-type target (ensure-file-type target-type) args))

(defmethod transcode :before (source (source-type symbol) target (target-type symbol) &key &allow-other-keys)
  (unless (eql T target-type)
    (v:info :trial.resource "Transcoding ~a to ~a" source target-type)))

(defmethod transcode (source (source-type symbol) target (target-type symbol) &key &allow-other-keys)
  (if (next-method-p)
      (call-next-method)
      (let ((types (list-eql-specializers #'transcode 1 3)))
        (error "Don't know how to transcode from ~a to ~a~%known transcoder types are:~%~{  ~a~%~}Did you load the respective format system?"
               source-type target-type types))))
