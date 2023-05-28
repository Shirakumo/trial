#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-global +current-shader-program+ NIL)

(defclass shader-program (gl-resource)
  ((uniform-map :initform (make-hash-table :test 'equal) :accessor uniform-map)
   (binding-point-allocation-pointer :initform 0 :accessor binding-point-allocation-pointer)
   (shaders :initarg :shaders :initform (arg! :shaders) :accessor shaders)
   (buffers :initarg :buffers :initform () :accessor buffers)))

(defmethod describe-object ((program shader-program) stream)
  (call-next-method)
  (format stream "~&~%Buffers:~{~%  ~a~}~%~%Shaders:~%"
          (or (buffers program) (list "None")))
  (dolist (shader (shaders program))
    (format stream "~&  ~s~%" (shader-type shader))
    (format-with-line-numbers (shader-source shader) stream)))

(defun check-shader-compatibility (shaders)
  (loop with table = (make-hash-table :test 'eql)
        for shader in shaders
        do (if (gethash (shader-type shader) table)
               (error "Cannot compile two shaders of the same type into a single program~%  ~a~%  ~a"
                      (gethash (shader-type shader) table) shader)
               (setf (gethash (shader-type shader) table) shader))
           (check-allocated shader)
        finally (return shaders)))

(defmethod dependencies ((program shader-program))
  (append (shaders program)
          (buffers program)))

(defun link-program (program shaders &optional (program-id (data-pointer program)))
  (dolist (shader shaders)
    (etypecase shader
      (integer (gl:attach-shader program-id shader))
      (shader (gl:attach-shader program-id (gl-name shader)))))
  (gl:link-program program-id)
  (dolist (shader shaders)
    (gl:detach-shader program-id (gl-name shader)))
  (unless (gl:get-program program-id :link-status)
    (error "Failed to link ~a: ~%~a"
           program (gl:get-program-info-log program-id)))
  (v:debug :trial.asset "Linked ~a with ~a." program shaders)
  (setf (data-pointer program) program-id)
  (clrhash (uniform-map program))
  (loop for buffer in (buffers program)
        do (bind buffer program)))

(defmethod (setf shaders) :before ((shaders cons) (program shader-program))
  (when (allocated-p program)
    ;; If we're already hot, relink immediately.
    (handler-bind ((resource-not-allocated (constantly-restart 'continue)))
      (check-shader-compatibility shaders)
      (link-program program shaders))))

(defmethod (setf shaders) ((other shader-program) (program shader-program))
  (setf (shaders program) (shaders other)))

(defmethod (setf buffers) :before (buffers (program shader-program))
  (when (allocated-p program)
    (loop for buffer in buffers
          do (bind buffer program))))

(defmethod allocate ((program shader-program))
  (let ((shaders (shaders program)))
    (check-shader-compatibility shaders)
    (let ((program-id (gl:create-program)))
      (with-cleanup-on-failure (gl:delete-program program-id)
        (link-program program shaders program-id)))))

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

(declaim (inline %set-dquat))
(defun %set-dquat (dquat dat i)
  (let ((qreal (qreal dquat))
        (qdual (qdual dquat)))
    (setf (cffi:mem-aref dat :float (+ i 0)) (qx qreal))
    (setf (cffi:mem-aref dat :float (+ i 1)) (qy qreal))
    (setf (cffi:mem-aref dat :float (+ i 2)) (qz qreal))
    (setf (cffi:mem-aref dat :float (+ i 3)) (qw qreal))
    (setf (cffi:mem-aref dat :float (+ i 4)) (qx qdual))
    (setf (cffi:mem-aref dat :float (+ i 5)) (qy qdual))
    (setf (cffi:mem-aref dat :float (+ i 6)) (qz qdual))
    (setf (cffi:mem-aref dat :float (+ i 7)) (qw qdual))))

(declaim (inline %set-uniform))
(defun %set-uniform (location data)
  (declare (optimize speed))
  (declare (type (signed-byte 32) location))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  ;; FIXME: this kinda blows, man.
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
    (quat (%gl:uniform-4f location (qx data) (qy data) (qz data) (qw data)))
    (dquat (cffi:with-foreign-object (dat :float 8)
             (%set-dquat data dat 0)
             (%gl:uniform-matrix-2x4-fv location 1 NIL dat)))
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
               (%gl:uniform-matrix-4fv location (length data) T dat)))
       (mat3 (cffi:with-foreign-object (dat :float (* 9 (length data)))
               (loop for i from 0 below (length data)
                     do (static-vectors:replace-foreign-memory
                         (cffi:inc-pointer dat (* 9 4 i))
                         (sb-sys:vector-sap (marr3 (svref data i)))
                         (* 9 4)))
               (%gl:uniform-matrix-4fv location (length data) T dat)))
       (dquat (cffi:with-foreign-object (dat :float (* 8 (length data)))
                (loop for i from 0 below (length data)
                      do (%set-dquat (aref data i) dat (* 8 i)))
                (%gl:uniform-matrix-2x4-fv location (length data) NIL dat)))))))

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
