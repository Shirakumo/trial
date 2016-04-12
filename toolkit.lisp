#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defparameter *time-units* #+sbcl 1000000
              #-sbcl internal-time-units-per-second)

(declaim (inline current-time))
(defun current-time ()
  #+sbcl (let ((usec (nth-value 1 (sb-ext:get-time-of-day))))
           (declare (type (unsigned-byte 31) usec))
           usec)
  #-sbcl (get-internal-real-time))

(defun enlist (item &rest items)
  (if (listp item) item (list* item items)))

(defun unlist (item)
  (if (listp item) (first item) item))

(defmacro with-primitives (primitive &body body)
  `(progn
     (gl:begin ,primitive)
     (unwind-protect
          (progn ,@body)
       (gl:end))))

(defmacro with-pushed-matrix (&body body)
  `(progn (gl:push-matrix)
          (unwind-protect
               (progn ,@body)
            (gl:pop-matrix))))

(defun matrix-4x4 (&rest elements)
  (let ((m (make-array '(4 4) :element-type 'single-float :initial-element 0.0f0)))
    (loop for x in elements
          for i from 0
          do (setf (row-major-aref m i) x))
    m))

(defun one-of (thing &rest options)
  (find thing options))

(define-compiler-macro one-of (thing &rest options)
  (let ((thing-var (gensym "THING")))
    `(let ((,thing-var ,thing))
       (or ,@(loop for option in options
                   collect `(eql ,thing-var ,option))))))
