#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass shader (asset)
  ((shader-type :initarg :type :accessor shader-type)))

(defmethod initialize-instance :before ((asset shader) &key type)
  (check-shader-type type))

(defmethod coerce-input ((asset shader) (file pathname))
  (alexandria:read-file-into-string file))

(defmethod coerce-input ((asset shader) (source string))
  source)

(defmethod finalize-resource ((type (eql 'shader)) resource)
  (gl:delete-shader resource))

(defmethod load progn ((asset shader))
  (let ((source (with-output-to-string (output)
                  (dolist (source (coerced-inputs asset))
                    (write-sequence source output))))
        (shader (gl:create-shader (shader-type asset))))
    (setf (resource asset) shader)
    (with-cleanup-on-failure (offload asset)
      (with-new-value-restart (source input-source) (use-source "Supply new source code directly.")
        (gl:shader-source shader source)
        (gl:compile-shader shader)
        (unless (gl:get-shader shader :compile-status)
          (error "Failed to compile ~a: ~%~a~%Shader source:~%~a"
                 asset (gl:get-shader-info-log shader)
                 source))
        (v:debug :trial.asset "Compiled shader ~a: ~%~a" shader source)))))
