#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass shader-program (asset)
  ((uniform-map :initform NIL :accessor uniform-map)))

(defmethod coerce-input ((asset shader-program) (shader shader))
  shader)

(defun check-shader-compatibility (shaders)
  (loop with table = (make-hash-table :test 'eql)
        for shader in shaders
        do (if (gethash (shader-type shader) table)
               (error "Cannot compile two shaders of the same type into a single program~%  ~a~%  ~a"
                      (gethash (shader-type shader) table) shader)
               (setf (gethash (shader-type shader) table) shader))
        finally (return shaders)))

(defmethod finalize-resource ((type (eql 'shader-program)) resource)
  (gl:delete-program resource))

(defmethod load progn ((asset shader-program))
  (let ((shaders (coerced-inputs asset)))
    (check-shader-compatibility shaders)
    (let ((program (gl:create-program)))
      (setf (resource asset) program)
      (setf (uniform-map asset) (make-hash-table :test 'equal))
      (with-cleanup-on-failure (offload asset)
        (dolist (shader shaders)
          (load shader)
          (gl:attach-shader program (resource shader)))
        (gl:link-program program)
        (dolist (shader shaders)
          (gl:detach-shader program (resource shader)))
        (unless (gl:get-program program :link-status)
          (error "Failed to link ~a: ~%~a"
                 asset (gl:get-program-info-log program)))))))

(defmethod uniform-location ((asset shader-program) (name string))
  (or (gethash name (uniform-map asset))
      (setf (gethash name (uniform-map asset))
            (gl:get-uniform-location (resource asset) name))))

(defmethod uniform-location ((asset shader-program) (name symbol))
  (uniform-location asset (symbol->c-name name)))

(defmethod (setf uniform) (data (asset shader-program) (name symbol))
  (setf (uniform asset (symbol->c-name name)) data))

(defmethod (setf uniform) (data (asset shader-program) (name string))
  (let ((location (uniform-location asset name)))
    (etypecase data
      (fixnum (gl:uniformi location data))
      (single-float (gl:uniformf location data))
      (vec2 (gl:uniformf location (vx data) (vy data)))
      (vec3 (gl:uniformf location (vx data) (vy data) (vz data)))
      (vec4 (gl:uniformf location (vx data) (vy data) (vz data) (vw data)))
      (mat2 (gl:uniform-matrix-2fv location (marr2 data)))
      (mat3 (gl:uniform-matrix-3fv location (marr3 data)))
      (mat4 (gl:uniform-matrix-4fv location (marr4 data)))
      (matn (ecase (mrows data)
              (2 (ecase (mcols data)
                   (3 (gl:uniform-matrix-2x3-fv location (marrn data)))
                   (4 (gl:uniform-matrix-2x4-fv location (marrn data)))))
              (3 (ecase (mcols data)
                   (2 (gl:uniform-matrix-3x2-fv location (marrn data)))
                   (4 (gl:uniform-matrix-3x4-fv location (marrn data)))))
              (4 (ecase (mcols data)
                   (2 (gl:uniform-matrix-4x2-fv location (marrn data)))
                   (3 (gl:uniform-matrix-4x3-fv location (marrn data))))))))))
