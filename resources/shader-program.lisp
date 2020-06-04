#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass shader-program (gl-resource)
  ((uniform-map :initform (make-hash-table :test 'equal) :accessor uniform-map)
   (shaders :initarg :shaders :accessor shaders)
   (buffers :initarg :buffers :accessor buffers))
  (:default-initargs
   :shaders (error "SHADERS required.")
   :buffers ()))

(defun check-shader-compatibility (shaders)
  (loop with table = (make-hash-table :test 'eql)
        for shader in shaders
        do (if (gethash (shader-type shader) table)
               (error "Cannot compile two shaders of the same type into a single program~%  ~a~%  ~a"
                      (gethash (shader-type shader) table) shader)
               (setf (gethash (shader-type shader) table) shader))
        finally (return shaders)))

(defmethod destructor ((program shader-program))
  (let ((prog (gl-name program)))
    (lambda () (when prog (gl:delete-program prog)))))

(defmethod dependencies ((program shader-program))
  (copy-list (shaders program)))

(defun link-program (program shaders)
  (let ((prog (gl-name program)))
    (dolist (shader shaders)
      (check-allocated shader)
      (gl:attach-shader prog (gl-name shader)))
    (gl:link-program prog)
    (dolist (shader shaders)
      (gl:detach-shader prog (gl-name shader)))
    (unless (gl:get-program prog :link-status)
      (error "Failed to link ~a: ~%~a"
             program (gl:get-program-info-log prog)))
    (v:debug :trial.asset "Linked ~a with ~a." program shaders)
    (loop for buffer in (buffers program)
          for i from 0
          do (bind buffer program i))))

(defmethod (setf shaders) :before (shaders (program shader-program))
  (when (allocated-p program)
    ;; If we're already hot, relink immediately.
    (handler-bind ((resource-not-allocated (constantly-restart 'continue)))
      (link-program program shaders))))

(defmethod (setf buffers) :before (buffers (program shader-program))
  (when (allocated-p program)
    (loop for buffer in buffers
          for i from 0
          do (bind buffer program i))))

(defmethod allocate ((program shader-program))
  (let ((shaders (shaders program)))
    (check-shader-compatibility shaders)
    (let ((prog (gl:create-program)))
      (with-cleanup-on-failure (progn (gl:delete-program prog)
                                      (setf (data-pointer program) NIL))
        (setf (data-pointer program) prog)
        (link-program program shaders)))))

(defmethod deallocate :after ((program shader-program))
  (clrhash (uniform-map program)))

(declaim (inline %set-uniform))
(defun %set-uniform (location data)
  (declare (optimize speed))
  (declare (type (signed-byte 32) location))
  (etypecase data
    (vec4 (%gl:uniform-4f location (vx data) (vy data) (vz data) (vw data)))
    (vec3 (%gl:uniform-3f location (vx data) (vy data) (vz data)))
    (vec2 (%gl:uniform-2f location (vx data) (vy data)))
    (mat4 #+sbcl
          (let ((data (marr4 data)))
            (sb-sys:with-pinned-objects (data)
              (%gl:uniform-matrix-4fv location 1 T (sb-sys:vector-sap data))))
          #-sbcl
          (gl:uniform-matrix-4fv location (marr4 data)))
    (mat3 #+sbcl
          (let ((data (marr3 data)))
            (sb-sys:with-pinned-objects (data)
              (%gl:uniform-matrix-3fv location 1 T (sb-sys:vector-sap data))))
          #-sbcl
          (gl:uniform-matrix-3fv location (marr3 data)))
    (mat2 #+sbcl
          (let ((data (marr2 data)))
            (sb-sys:with-pinned-objects (data)
              (%gl:uniform-matrix-2fv location 1 T (sb-sys:vector-sap data))))
          #-sbcl
          (gl:uniform-matrix-2fv location (marr2 data)))
    (single-float (%gl:uniform-1f location data))
    (double-float (%gl:uniform-1d location data))
    (fixnum (%gl:uniform-1i location data))
    (matn (ecase (mrows data)
            (2 (ecase (mcols data)
                 (3 (gl:uniform-matrix-2x3-fv location (marrn data)))
                 (4 (gl:uniform-matrix-2x4-fv location (marrn data)))))
            (3 (ecase (mcols data)
                 (2 (gl:uniform-matrix-3x2-fv location (marrn data)))
                 (4 (gl:uniform-matrix-3x4-fv location (marrn data)))))
            (4 (ecase (mcols data)
                 (2 (gl:uniform-matrix-4x2-fv location (marrn data)))
                 (3 (gl:uniform-matrix-4x3-fv location (marrn data)))))))))

(declaim (inline uniform-location))
(defun uniform-location (program name)
  (or (gethash name (uniform-map program))
      (setf (gethash name (uniform-map program))
            (gl:get-uniform-location (gl-name program) name))))

(defun (setf uniform) (data asset name)
  (declare (optimize speed))
  (let* ((name (etypecase name
                 (string name)
                 (symbol (symbol->c-name name))))
         (location (uniform-location asset name)))
    (%set-uniform location data)))

(define-compiler-macro (setf uniform) (&environment env data asset name)
  (let ((nameg (gensym "NAME"))
        (assetg (gensym "ASSET"))
        (locationg (gensym "LOCATION")))
    (cond ((constantp name env)
           `(let ((,nameg (load-time-value
                           (etypecase ,name
                             (string ,name)
                             (symbol (symbol->c-name ,name)))))
                  (,assetg ,asset))
              (%set-uniform (uniform-location ,assetg ,nameg) ,data)))
          (T
           `(let* ((,nameg (etypecase ,name
                             (string ,name)
                             (symbol (symbol->c-name ,name))))
                   (,locationg (uniform-location ,asset ,nameg)))
              (%set-uniform ,locationg ,data))))))

(defmethod uniforms ((program shader-program))
  (let ((count (gl:get-program (gl-name program) :active-uniforms)))
    (loop for i from 0 below count
          collect (multiple-value-bind (size type name) (gl:get-active-uniform (gl-name program) i)
                    (list :index i
                          :size size
                          :type type
                          :name name
                          :location (uniform-location program name))))))
