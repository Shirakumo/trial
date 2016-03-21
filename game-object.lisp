#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject game-object ()
  ((angle :initform 0)
   (angle-delta :initform 1)
   (location :initform (vec 250 250 0) :accessor location)
   (velocity :initform (vec 0 0 0) :accessor velocity)
   (path :initarg :path :initform NIL :accessor path)
   (image :initform NIL :accessor image :finalized T))
  (:default-initargs :path (error "Must define a file path.")))

(defmethod initialize-instance :after ((obj game-object) &key)
  (setf (image obj) (load-image-buffer (path obj))))

(defmethod draw ((obj game-object))
  (let ((image (q+:texture (image obj)))
        (size (q+:size (image obj)))
        (location (location obj)))
    (gl:push-matrix)
    (gl:translate (vx location) (vy location) 0)
    (gl:rotate (slot-value obj 'angle) 0 0 1)
    (q+:draw-texture *main-window*
                     (q+:make-qpointf (- (/ (q+:width size) 2))
                                      (- (/ (q+:height size) 2)))
                     image)
    (gl:pop-matrix)))

(define-handler (game-object update tick) (ev)
  (incf (slot-value game-object 'angle) (slot-value game-object 'angle-delta))
  (nv+ (location game-object) (velocity game-object)))

(define-handler (game-object catty-go key-press) (ev key)
  (case key
    (:left (setf (vx (velocity game-object)) -5))
    (:right (setf (vx (velocity game-object)) 5))
    (:up (setf (vy (velocity game-object)) -5))
    (:down (setf (vy (velocity game-object)) 5))))

(define-handler (game-object catty-stop key-release) (ev key)
  (case key
    (:left (setf (vx (velocity game-object)) 0))
    (:right (setf (vx (velocity game-object)) 0))
    (:up (setf (vy (velocity game-object)) 0))
    (:down (setf (vy (velocity game-object)) 0))))
