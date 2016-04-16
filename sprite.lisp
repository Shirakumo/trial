#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject sprite-animation (textured-subject)
  ((duration :initarg :duration :accessor duration)
   (frames :initarg :frames :accessor frames)
   (frame :initarg :frame :accessor frame)
   (next :initarg :next :accessor next)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height))
  (:default-initargs
   :duration (error "DURATION required.")
   :frames (error "FRAMES required.")
   :frame 0
   :next NIL))

(defmethod paint ((subject sprite-animation) target)
  (with-slots (width height frames frame) subject
    (let ((frame-start (/ frame frames))
          (frame-end (/ (1+ frame) frames))
          (width (or width (q+:width (data (texture subject)))))
          (height (or height (/ (q+:height (data (texture subject))) frames))))
      (with-primitives :quads
        (gl:tex-coord 0 frame-start)
        (gl:vertex 0 0)
        (gl:tex-coord 1 frame-start)
        (gl:vertex width 0)
        (gl:tex-coord 1 frame-end)
        (gl:vertex width height)
        (gl:tex-coord 0 frame-end)
        (gl:vertex 0 height)))))

(define-subject sprite-subject (clocked-subject)
  ((animations :initform (make-hash-table :test 'eql) :accessor animations)
   (animation :initform NIL :accessor animation)))

(defmethod initialize-instance :after ((subject sprite-subject) &key animations animation width height)
  (loop for animation in animations
        do (destructuring-bind (name duration frames &key next texture (width width) (height height)) animation
             (setf (gethash name (animations subject))
                   (make-instance 'sprite-animation
                                  :duration duration
                                  :frames frames
                                  :texture texture
                                  :next next
                                  :width width
                                  :height height))))
  (setf (animation subject) (or animation (first (first animations)))))

(defmethod (setf animation) ((value symbol) (subject sprite-subject))
  (setf (animation subject) (or (gethash value (animations subject))
                                (error "No such animation ~s on ~a" value subject))))

(defmethod enter :after ((subject sprite-subject) scene)
  (start subject))

(define-handler (sprite-subject advance-frame tick) (ev)
  (with-slots (animations animation) sprite-subject
    (let ((frame (round (* (/ (clock sprite-subject) (duration animation))
                           (frames animation)))))
      (when (<= (frames animation) frame)
        (setf frame 0)
        (reset sprite-subject)
        (when (next animation)
          (setf animation (gethash (next animation) animations))))
      (setf (frame animation) frame))))

(defmethod paint ((subject sprite-subject) target)
  (paint (animation subject) target))


