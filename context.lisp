#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass context ()
  ((context :initform NIL :reader context)
   (current-thread :initform (bt:current-thread) :accessor current-thread)
   (lock :initform (bt:make-lock "Context lock") :reader context-lock)))

(defmethod initialize-instance ((context context) &key (accum NIL)
                                                       (alpha T)
                                                       (depth T)
                                                       (direct-rendering T)
                                                       (double-buffer T)
                                                       (overlay NIL)
                                                       (plane 0)
                                                       (profile :compatibility)
                                                       (rgba T)
                                                       (sample-buffers T)
                                                       (samples 2)
                                                       (stencil T)
                                                       (stereo NIL)
                                                       (swap-interval T)
                                                       (version (list 3 2)))
  (let ((format (q+:make-qglformat)))
    (setf (q+:accum format) accum)
    (setf (q+:alpha format) alpha)
    (setf (q+:depth format) depth)
    (setf (q+:direct-rendering format) direct-rendering)
    (setf (q+:double-buffer format) double-buffer)
    (setf (q+:overlay format) overlay)
    (setf (q+:plane format) plane)
    (setf (q+:profile format) (ecase profile
                                ((NIL :none :no-profile) (q+:qglformat.no-profile))
                                ((:core) (q+:qglformat.core-profile))
                                (:compatibility (q+:qglformat.compatibility-profile))))
    (setf (q+:rgba format) rgba)
    (setf (q+:sample-buffers format) sample-buffers)
    (setf (q+:samples format) samples)
    (setf (q+:stencil format) stencil)
    (setf (q+:version format) (values (first version) (second version)))
    (setf (slot-value context 'context) (q+:make-qglcontext format))
    (when (next-method-p)
      (call-next-method))))

(defmethod acquire-context ((context context) &key reacquire force)
  (let ((current (current-thread context))
        (this (bt:current-thread)))
    (when (or reacquire (not (eql current this)))
      (cond (force
             (v:warn :trial.context "Force acquiring ~a from ~a~@[, stealing it from ~a~]."
                     context this current))
            ((not (eql current this))
             (v:debug :trial.context "Acquiring ~a from ~a."
                      context this)
             (acquire-lock-with-starvation-test (context-lock context))))
      (q+:make-current (context context))
      (setf (current-thread context) this))))

(defmethod release-context ((context context) &key force)
  (let ((current (current-thread context))
        (this (bt:current-thread)))
    (when (or force (eql current this))
      (cond (force
             (v:warn :trial.context "Force releasing ~a from ~a~@[, stealing it from ~a~]."
                     context this current))
            ((eql current this)
             (v:debug :trial.context "Releasing ~a from ~a."
                      context this)))
      (bt:release-lock (context-lock context))
      (setf (current-thread context) NIL))))

(defmacro with-context ((context &key reacquire force) &body body)
  (let ((cont (gensym "CONTEXT"))
        (forc (gensym "FORCE")))
    `(let ((,cont ,context)
           (,forc ,force))
       (acquire-context ,cont :reacquire ,reacquire :force ,forc)
       (unwind-protect
            (progn ,@body)
         (release-context ,cont :force ,forc)))))
