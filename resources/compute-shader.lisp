#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass compute-shader (shader-program)
  ((shader-source :initarg :source :initform (arg! :source) :accessor shader-source)
   (shaders :initform ())
   (work-groups :initarg :work-groups :initform (vec 1 1 1) :accessor work-groups)
   (barrier :initform 4294967295)))

(defmethod initialize-instance :after ((shader compute-shader) &key)
  (unless (integerp (slot-value shader 'barrier))
    (setf (barrier shader) (slot-value shader 'barrier))))

(defmethod shared-initialize :after ((shader compute-shader) slots &key (barrier NIL barrier-p))
  (when barrier-p (setf (barrier shader) barrier)))

(defmethod print-object ((shader compute-shader) stream)
  (print-unreadable-object (shader stream :type T :identity T)
    (format stream "~:[~;ALLOCATED~]" (allocated-p shader))))

(defmethod barrier ((shader compute-shader))
  (cffi:foreign-bitfield-symbols '%gl::MemoryBarrierMask (slot-value shader 'barrier)))

(defmethod (setf barrier) ((bits list) (shader compute-shader))
  (setf (slot-value shader 'barrier) (cffi:foreign-bitfield-value '%gl::MemoryBarrierMask bits)))

(defmethod (setf barrier) ((bits symbol) (shader compute-shader))
  (setf (slot-value shader 'barrier) (cffi:foreign-bitfield-value '%gl::MemoryBarrierMask (list bits))))

(defmethod allocate ((shader compute-shader))
  (let ((source (shader-source shader))
        (shdr (gl:create-shader :compute-shader))
        (prog (gl:create-program)))
    (with-cleanup-on-failure (progn (gl:delete-shader shdr)
                                    (gl:delete-program prog)
                                    (setf (data-pointer shader) NIL))
      (with-new-value-restart (source input-source) (use-source "Supply new source code directly.")
        (unless (search "#version " source)
          (setf source (format NIL "~a~%~a" (glsl-version-header *context*) source))
          (when (eql :es (profile *context*))
            (setf source (glsl-toolkit:transform source :es (version *context*)))))
        (gl:shader-source shdr source)
        (gl:compile-shader shdr)2
        (unless (gl:get-shader shdr :compile-status)
          (error 'shader-compilation-error :shader shader :log (gl:get-shader-info-log shdr)))
        (v:debug :trial.asset "Compiled shader ~a: ~%~a" shader source)
        (link-program shader (list shdr))
        (gl:delete-shader shdr)
        (setf (data-pointer shader) prog)))))

(defmethod render ((shader compute-shader) (target null))
  (let ((work-groups (work-groups shader))
        (barrier (slot-value shader 'barrier)))
    (etypecase work-groups
      (vec3
       (%gl:dispatch-compute
        (truncate (vx work-groups))
        (truncate (vy work-groups))
        (truncate (vz work-groups))))
      (integer
       (%gl:dispatch-compute-indirect work-groups))
      (buffer-object
       (%gl:bind-buffer :dispatch-indirect-buffer (gl-name work-groups))
       (%gl:dispatch-compute-indirect 0)))
    (when (/= 0 barrier)
      (%gl:memory-barrier barrier))))

(defmethod render ((shader compute-shader) (offset integer))
  (let ((work-groups (work-groups shader))
        (barrier (slot-value shader 'barrier)))
    (etypecase work-groups
      (buffer-object
       (%gl:bind-buffer :dispatch-indirect-buffer (gl-name work-groups))
       (%gl:dispatch-compute-indirect offset)))
    (when (/= 0 barrier)
      (%gl:memory-barrier barrier))))

(defmethod render ((shader compute-shader) (target buffer-object))
  (let ((work-groups (work-groups shader))
        (barrier (slot-value shader 'barrier)))
    (%gl:bind-buffer :dispatch-indirect-buffer (gl-name work-groups))
    (%gl:dispatch-compute-indirect (etypecase work-groups
                                     (integer work-groups)
                                     (null 0)))
    (when (/= 0 barrier)
      (%gl:memory-barrier barrier))))
