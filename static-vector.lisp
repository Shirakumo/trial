(in-package #:org.shirakumo.fraf.trial)

(defvar *static-vector-map* (tg:make-weak-hash-table :weakness :key :test 'eq))

(declaim (inline mark-static-vector))
(defun mark-static-vector (vector)
  (setf (gethash vector *static-vector-map*) T)
  vector)

(defun make-static-vector (length &rest args)
  (mark-static-vector (apply #'static-vectors:make-static-vector length args)))

(define-compiler-macro make-static-vector (length &rest args)
  `(mark-static-vector (static-vectors:make-static-vector ,length ,@args)))

(declaim (inline static-vector-p))
(defun static-vector-p (vec)
  (gethash vec *static-vector-map*))

(deftype static-vector ()
  '(satisfies static-vector-p))

(declaim (inline maybe-free-static-vector))
(defun maybe-free-static-vector (vector)
  (when (static-vector-p vector)
    (static-vectors:free-static-vector vector)
    (remhash vector *static-vector-map*)))

(defmacro with-static-vector ((name size &rest args) &body body)
  `(let ((,name (make-static-vector ,size ,@args)))
     (unwind-protect
          (progn ,@body)
       (maybe-free-static-vector ,name))))

(defmethod finalize ((vector vector))
  (maybe-free-static-vector vector))

(defstruct (static-vector-memory-region 
            (:include mem:memory-region)
            (:constructor %make-static-vector-memory-region))
  (vector NIL :type T))

(defmethod finalize ((region static-vector-memory-region))
  (maybe-free-static-vector (static-vector-memory-region-vector region)))

(defun make-static-vector-memory-region (length/vec)
  (etypecase length/vec
    (integer
     (setf length/vec (make-static-vector length/vec)))
    (vector))
  (%make-static-vector-memory-region
   :pointer (static-vector-pointer length/vec)
   :size (length length/vec)
   :vector length/vec))
