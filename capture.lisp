#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmacro define-event-type-serializer (type &body slots)
  `(define-object-type-serializer ,type (lambda (&rest args) (apply #'make-event ',type args))
     ,@slots))

(define-event-type-serializer tick
  (tt double-float)
  (dt single-float)
  (fc integer))

(define-event-type-serializer key-press
  (key symbol)
  (repeat boolean))

(define-event-type-serializer key-release
  (key symbol)
  (repeat boolean))

(define-event-type-serializer text-entered
  (text string))

(define-event-type-serializer mouse-press
  (pos vec2)
  (button keyword))

(define-event-type-serializer mouse-release
  (pos vec2)
  (button keyword))

(define-event-type-serializer mouse-scroll
  (pos vec2)
  (delta single-float))

(define-event-type-serializer mouse-move
  (pos vec2)
  (old-pos vec2))

(define-event-type-serializer gamepad-press
  (device gamepad:device)
  (button keyword))

(define-event-type-serializer gamepad-release
  (device gamepad:device)
  (button keyword))

(define-event-type-serializer gamepad-move
  (device gamepad:device)
  (axis keyword)
  (old-pos single-float)
  (pos single-float))

(defclass capture-object ()
  ((capture-stream :initform NIL :accessor capture-stream)
   (file :initarg :file :initform (make-pathname :name (format NIL "capture ~a" (format-timestring :as :filename)) :type "dat") :accessor file)
   (fc :initform 0 :accessor fc)))

(defmethod start ((capture capture-object))
  (unless (capture-stream capture)
    (setf (capture-stream capture) (open (file capture) :element-type '(unsigned-byte 8) :direction :io))))

(defmethod stop ((capture capture-object))
  (when (capture-stream capture)
    (close (capture-stream capture))
    (setf (capture-stream capture) NIL)))

(defmethod active-p ((capture capture-object))
  (not (null (capture-stream capture))))

(defclass capture (capture-object)
  ())

(defmethod handle ((event event) (capture capture))
  (let ((stream (capture-stream capture)))
    (when (typep event 'tick)
      (setf (fc capture) (fc event)))
    (let ((struct (gethash (type-of event) +type-serialize-info+)))
      (when struct
        (write-byte (serialize-info-id struct) stream)
        (funcall (serialize-info-writer struct) event stream)))))

(defclass replay (capture-object)
  ((buffer :initform NIL :accessor buffer)))

(defmethod handle ((loop event-loop) (replay replay))
  (let ((stream (capture-stream replay)))
    (when (buffer replay)
      (issue loop (shiftf (buffer replay) NIL)))
    (loop for event = (funcall (serialize-info-reader (gethash (read-byte stream) +type-serialize-info+)) stream)
          do (when (typep event 'tick)
               (setf (buffer replay) event)
               (setf (fc replay) (fc event))
               (return))
             (issue loop event))))
