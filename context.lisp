#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass context ()
  ((current-thread :initform (bt:current-thread) :accessor current-thread)))

(defmethod acquire-context ((context context))
  (if (or (null (current-thread context))
          (eql (current-thread context) (bt:current-thread)))
      (v:debug :trial.context "Acquiring context ~a in ~a"
               context (bt:current-thread))
      (v:warn :trial.context "Acquiring context ~a in ~a while already acquired in ~a."
              context (bt:current-thread) (current-thread context)))
  (q+:make-current context)
  (setf (current-thread context) (bt:current-thread)))

(defmethod release-context ((context context))
  (if (eql (current-thread context) (bt:current-thread))
      (v:debug :trial.context "Releasing context ~a from ~a"
               context (bt:current-thread))
      (v:severe :trial.context "Releasing context ~a from thread ~a while acquired in ~a."
                context (bt:current-thread) (current-thread context)))
  (q+:done-current context)
  (setf (current-thread context) NIL))

(defmacro with-context ((context) &body body)
  (let ((cont (gensym "CONTEXT")))
    `(let ((,cont ,context))
       (acquire-context ,cont)
       (unwind-protect
            (progn ,@body)
         (release-context ,cont)))))
