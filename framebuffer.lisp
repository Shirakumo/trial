#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defun attachment-value (attachment)
  (ecase attachment
    ((:depth-stencil :depth-and-stencil :combined-depth-stencil)
     (q+:qglframebufferobject.combined-depth-stencil))
    (:depth
     (q+:qglframebufferobject.depth))
    ((:none :no-attachment NIL)
     (q+:qglframebufferobject.no-attachment))))

(define-finalizable framebuffer ()
  ((buffer-object :initarg :buffer-object :accessor buffer-object :finalized T)
   (buffer-format :initform NIL :accessor buffer-format :finalized T))
  (:default-initargs
   :buffer-object NIL))

(defmethod initialize-instance :after ((buffer framebuffer) &key width height mipmap (samples 0)
                                                                 (attachment :combined-depth-stencil)
                                       &allow-other-keys)
  (unless (buffer-object buffer)
    (unless (and width height)
      (error "WIDTH and HEIGHT required."))
    (let ((format (q+:make-qglframebufferobjectformat)))
      (setf (q+:mipmap format) mipmap)
      (setf (q+:samples format) samples)
      (setf (q+:attachment format) (attachment-value attachment))
      (setf (buffer-object buffer) (q+:make-qglframebufferobject width height format))))
  (setf (buffer-format buffer) (q+:format (buffer-object buffer))))

(defmethod reinitialize-instance :after ((buffer framebuffer) &key width height (mipmap NIL mipmap-p)
                                                                   (samples NIL samples-p) (attachment NIL attachment-p)
                                         &allow-other-keys)
  (let ((format (q+:make-qglframebufferobjectformat (buffer-format buffer))))
    (unless (and width height)
      (error "WIDTH and HEIGHT required."))
    (when mipmap-p (setf (q+:mipmap format) mipmap))
    (when samples-p (setf (q+:samples format) samples))
    (when attachment-p (setf (q+:attachment format) (attachment-value attachment)))
    (let ((new (q+:make-qglframebufferobject width height format)))
      (finalize (buffer-object buffer))
      (finalize (buffer-format buffer))
      (setf (buffer-object buffer) new)
      (setf (buffer-format buffer) format))))

(defmethod render :around (scene (buffer framebuffer))
  (q+:bind (buffer-object buffer))
  (unwind-protect
       (call-next-method)
    (q+:release (buffer-object buffer))))

(defmethod call-with-framebuffer-bound (function (buffer framebuffer))
  (q+:bind (buffer-object buffer))
  (unwind-protect
       (funcall function)
    (q+:release (buffer-object buffer))))

(defmacro with-framebuffer-bound ((buffer) &body body)
  `(call-with-framebuffer-bound
    (lambda () ,@body) ,buffer))

(defmethod width ((buffer framebuffer))
  (q+:width (buffer-object buffer)))

(defmethod height ((buffer framebuffer))
  (q+:height (buffer-object buffer)))
