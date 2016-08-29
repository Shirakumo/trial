#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass sprite-animation (face-entity)
  ((duration :initarg :duration :accessor duration)
   (frames :initarg :frames :accessor frames)
   (frame :initarg :frame :accessor frame)
   (next :initarg :next :accessor next))
  (:default-initargs
   :duration (error "DURATION required.")
   :frames (error "FRAMES required.")
   :frame 0
   :next NIL))

(defmethod (setf frame) :after (frame (subject sprite-animation))
  (setf (vy (tex-location subject)) (- 1 (/ (1+ frame) (frames subject))))
  (setf (vy (tex-bounds subject)) (- 1 (/ frame (frames subject)))))

(define-subject sprite-subject (clocked-subject bound-entity)
  ((animations :initform (make-hash-table :test 'eql) :accessor animations)
   (animation :initform NIL :accessor animation)))

(defmethod initialize-instance :after ((subject sprite-subject) &key animations animation bounds)
  (loop for animation in animations
        do (destructuring-bind (name duration frames &key next texture (bounds bounds)) animation
             (setf (gethash name (animations subject))
                   (make-instance 'sprite-animation
                                  :name name
                                  :duration duration
                                  :frames frames
                                  :texture texture
                                  :next next
                                  :bounds bounds))))
  (setf (animation subject) (or animation (first (first animations)))))

(defmethod (setf animation) ((value symbol) (subject sprite-subject))
  (setf (animation subject) (or (gethash value (animations subject))
                                (error "No such animation ~s on ~a" value subject))))

(defmethod (setf animation) ((animation sprite-animation) (subject sprite-subject))
  (unless (eql animation (animation subject))
    (start subject)
    (reset subject)
    (setf (slot-value subject 'animation) animation)))

(defmethod enter :after ((subject sprite-subject) scene)
  (start subject))

(define-handler (sprite-subject advance-frame tick) (ev)
  (with-slots (animations animation) sprite-subject
    (let ((frame (round (* (/ (clock sprite-subject) (duration animation))
                           (frames animation)))))
      (cond ((and (<= (frames animation) frame) (next animation))
             (setf (frame animation) 0)
             (reset sprite-subject)
             (setf animation (gethash (next animation) animations)))
            ((<= (frames animation) frame)
             (stop sprite-subject))
            (T
             (setf (frame animation) frame))))))

(defmethod paint ((subject sprite-subject) target)
  (paint (animation subject) target))


