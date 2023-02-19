#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmacro define-event-type-serializer (type &body slots)
  `(define-object-type-serializer ,type (lambda (&rest args) (apply #'make-event ',type args))
     ,@slots))

(define-type-serializer frame-count-change
  (lambda (s) (nibbles:read-ub32/le s))
  (lambda (v s) (nibbles:write-ub32/le v s)))

(define-event-type-serializer key-press
  (key keyword)
  (repeat-p boolean))

(define-event-type-serializer key-release
  (key keyword)
  (repeat-p boolean))

(define-event-type-serializer text-entered
  (text string)
  (replace-p boolean))

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

(defun capture-pathname ()
  (make-pathname :name (format NIL "capture ~a" (format-timestring :as :filename)) :type "dat"))

(defvar *capture-header* "TRIAL-CAPTURE")

(defclass capture (entity listener)
  ((name :initform 'capture)
   (capture-stream :initform NIL :accessor capture-stream)
   (file :initarg :file :initform (capture-pathname) :accessor file)
   (fc :initform NIL :accessor fc)))

(defmethod start ((capture capture))
  (when (capture-stream capture)
    (error "Already started."))
  (let ((stream (open (file capture)
                      :element-type '(unsigned-byte 8)
                      :direction :io)))
    (setf (capture-stream capture) stream)
    (loop for c across *capture-header*
          do (write-byte (char-code c) stream))
    (nibbles:write-ub32/le 0 stream))
  capture)

(defmethod stop ((capture capture))
  ;; Finalize the length
  (let ((stream (capture-stream capture)))
    (when stream
      (file-position stream (length *capture-header*))
      (nibbles:write-ub32/le (car (fc capture)) stream)
      (close (capture-stream capture))
      (setf (capture-stream capture) NIL)))
  capture)

(defmethod active-p ((capture capture))
  (not (null (capture-stream capture))))

(defmethod handle ((event tick) (capture capture))
  (unless (fc capture)
    (setf (fc capture) (cons -1 0)))
  (setf (cdr (fc capture)) (fc event)))

(defmethod handle ((event event) (capture capture))
  (let ((stream (capture-stream capture))
        (struct (serialize-info (type-of event)))
        (fc (fc capture)))
    (when (and struct fc)
      (unless (= (car fc) (cdr fc))
        (let ((info (serialize-info 'frame-count-change)))
          (write-byte (serialize-info-id info) stream)
          (funcall (serialize-info-writer info) (cdr (fc capture)) stream))
        (setf (car fc) (cdr fc)))
      (write-byte (serialize-info-id struct) stream)
      (funcall (serialize-info-writer struct) event stream))))

(defmethod start-capture ((scene scene) &optional (file (capture-pathname)))
  (register (start (make-instance 'capture :file file)) scene))

(defmethod start-capture ((scene (eql T)) &optional (file (capture-pathname)))
  (start-capture (scene +main+) file))

(defmethod stop-capture ((scene scene))
  (stop (deregister (unit 'capture scene) scene)))

(defmethod stop-capture ((scene (eql T)))
  (stop-capture (scene +main+)))

(defclass replay (unit listener)
  ((name :initform 'replay)
   (file :initarg :file :accessor file)
   (capture-stream :initform NIL :accessor capture-stream)
   (buffer :initform NIL :accessor buffer)
   (duration :initform NIL :accessor duration)
   (fc :initform (cons NIL NIL) :accessor fc)))

(defmethod start ((replay replay))
  (let ((stream (open (file replay) :element-type '(unsigned-byte 8) :direction :input)))
    (loop for c across *capture-header*
          do (assert (char= c (code-char (read-byte stream)))))
    (setf (capture-stream replay) stream)
    (setf (duration replay) (nibbles:read-ub32/le stream))
    (when (= 0 (duration replay))
      (v:warn :trial.capture "Capture file was not finalized.")
      (setf (duration replay) most-positive-fixnum))
    (let ((fc (funcall (serialize-info-reader (gethash (read-byte stream) +type-serialize-info+)) stream)))
      (setf (fc replay) (cons fc fc)))
    replay))

(defmethod stop ((replay replay))
  (when (capture-stream replay)
    (close (capture-stream replay))
    (setf (capture-stream replay) NIL))
  replay)

(defmethod handle ((ev tick) (replay replay))
  (let ((fc (fc replay)))
    (incf (car fc))
    (when (= (duration replay) (car fc))
      (v:info :trial.capture "Capture replay completed.")
      (setf (cdr fc) most-positive-fixnum)
      (stop replay)
      (deregister replay (scene +main+)))
    (when (< (cdr fc) (car fc))
      (let ((stream (capture-stream replay)))
        (loop for object = (funcall (serialize-info-reader (gethash (read-byte stream) +type-serialize-info+)) stream)
              do (etypecase object
                   (integer
                    (setf (cdr fc) object)
                    (return))
                   (event
                    (handle object +main+))))))))

(defmethod start-replay (file (scene scene))
  (register (start (make-instance 'replay :file file)) scene))

(defmethod start-replay (file (scene (eql T)))
  (start-replay file (scene +main+)))

(defmethod stop-replay ((scene scene))
  (stop (deregister (unit 'replay scene) scene)))

(defmethod stop-replay ((scene (eql T)))
  (stop-replay (scene +main+)))
