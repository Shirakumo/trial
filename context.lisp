#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass context ()
  ((context :initform NIL :reader context)
   (current-thread :initform (bt:current-thread) :accessor current-thread)))

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

(defmethod acquire-context ((context context))
  (if (or (null (current-thread context))
          (eql (current-thread context) (bt:current-thread)))
      (v:debug :trial.context "Acquiring context ~a in ~a"
               context (bt:current-thread))
      (v:warn :trial.context "Acquiring context ~a in ~a while already acquired in ~a."
              context (bt:current-thread) (current-thread context)))
  (q+:make-current (context context))
  (setf (current-thread context) (bt:current-thread)))

(defmethod release-context ((context context))
  (if (eql (current-thread context) (bt:current-thread))
      (v:debug :trial.context "Releasing context ~a from ~a"
               context (bt:current-thread))
      (v:severe :trial.context "Releasing context ~a from thread ~a while acquired in ~a."
                context (bt:current-thread) (current-thread context)))
  (q+:done-current (context context))
  (setf (current-thread context) NIL))

(defmacro with-context ((context) &body body)
  (let ((cont (gensym "CONTEXT")))
    `(let ((,cont ,context))
       (acquire-context ,cont)
       (unwind-protect
            (progn ,@body)
         (release-context ,cont)))))
