#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-global +current-shader-program+ NIL)

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

(defmethod dependencies ((program shader-program))
  (append (shaders program)
          (buffers program)))

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
          do (bind buffer program))
    (clrhash (uniform-map program))))

(defmethod (setf shaders) :before (shaders (program shader-program))
  (when (allocated-p program)
    ;; If we're already hot, relink immediately.
    (handler-bind ((resource-not-allocated (constantly-restart 'continue)))
      (link-program program shaders))))

(defmethod (setf buffers) :before (buffers (program shader-program))
  (when (allocated-p program)
    (loop for buffer in buffers
          do (bind buffer program))))

(defmethod allocate ((program shader-program))
  (let ((shaders (shaders program)))
    (check-shader-compatibility shaders)
    (let ((prog (gl:create-program)))
      (with-cleanup-on-failure (progn (gl:delete-program prog)
                                      (setf (data-pointer program) NIL))
        (setf (data-pointer program) prog)
        (link-program program shaders)))))

(defmethod deallocate ((program shader-program))
  (clrhash (uniform-map program))
  (gl:delete-program (gl-name program)))

#+sbcl
(declaim (notinline %gl-uniform-matrix-4fv))
#+sbcl
(defun %gl-uniform-matrix-4fv (loc arr)
  (let ((ptr (%gl::gl-get-proc-address "glUniformMatrix4fv")))
    (compile '%gl-uniform-matrix-4fv
             `(lambda (loc arr)
                (declare (type (unsigned-byte 32) loc))
                (declare (type (simple-array single-float (16)) arr))
                (declare (optimize speed (safety 0)))
                (sb-sys:with-pinned-objects (arr)
                  (cffi:foreign-funcall-pointer
                   ,ptr () :int loc %gl::sizei 1 %gl::boolean T :pointer (sb-sys:vector-sap arr) :void)))))
  (%gl-uniform-matrix-4fv loc arr))

(declaim (inline %set-uniform))
(defun %set-uniform (location data)
  (declare (optimize speed))
  (declare (type (signed-byte 32) location))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (etypecase data
    (vec4 (%gl:uniform-4f location (vx data) (vy data) (vz data) (vw data)))
    (vec3 (%gl:uniform-3f location (vx data) (vy data) (vz data)))
    (vec2 (%gl:uniform-2f location (vx data) (vy data)))
    (mat4 #+sbcl
          (%gl-uniform-matrix-4fv location (marr4 data))
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
                 (3 (%gl:uniform-matrix-2x3-fv location 1 T (marrn data)))
                 (4 (%gl:uniform-matrix-2x4-fv location 1 T (marrn data)))))
            (3 (ecase (mcols data)
                 (2 (%gl:uniform-matrix-3x2-fv location 1 T (marrn data)))
                 (4 (%gl:uniform-matrix-3x4-fv location 1 T (marrn data)))))
            (4 (ecase (mcols data)
                 (2 (%gl:uniform-matrix-4x2-fv location 1 T (marrn data)))
                 (3 (%gl:uniform-matrix-4x3-fv location 1 T (marrn data)))))))
    (simple-vector
     (etypecase (svref data 0)
       (mat4 (cffi:with-foreign-object (dat :float (* 16 (length data)))
               (loop for i from 0 below (length data)
                     do (static-vectors:replace-foreign-memory
                         (cffi:inc-pointer dat (* 16 4 i))
                         (sb-sys:vector-sap (marr4 (svref data i)))
                         (* 16 4)))
               (%gl:uniform-matrix-4fv location (length data) T dat)))))))

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
                           (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                             (etypecase ,name
                               (string ,name)
                               (symbol (symbol->c-name ,name))))))
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

(defmethod activate ((program shader-program))
  (unless (eq +current-shader-program+ program)
    (setf +current-shader-program+ program)
    (gl:use-program (gl-name program))))
