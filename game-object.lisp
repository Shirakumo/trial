#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass game-object ()
  ((angle :initform 0)
   (angle-delta :initform 1)
   (location :initform (q+:make-qpointf 250 250) :accessor location)
   (velocity :initform (q+:make-qpointf 0 0) :accessor velocity)
   (path :initarg :path :initform NIL :accessor path)
   (image :initform NIL :accessor image))
  (:default-initargs :path (error "Must define a file path.")))

(defmethod initialize-instance :after ((obj game-object) &key)
  (setf (image obj) (load-image-buffer (path obj)))
  (define-handler (catty-go key-press) (ev)
    (case (key ev)
      (:left (q+:set-x (velocity obj) -5))
      (:right (q+:set-x (velocity obj) 5))
      (:up (q+:set-y (velocity obj) -5))
      (:down (q+:set-y (velocity obj) 5))))
  (define-handler (catty-stop key-release) (ev)
    (case (key ev)
      (:left (q+:set-x (velocity obj) 0))
      (:right (q+:set-x (velocity obj) 0))
      (:up (q+:set-y (velocity obj) 0))
      (:down (q+:set-y (velocity obj) 0)))))

(defmethod finalize ((obj game-object))
  (finalize (image obj))
  (finalize (location obj))
  (finalize (velocity obj))
  (setf (image obj) NIL)
  (setf (path obj) NIL))

(defmethod update ((obj game-object) &key)
  (incf (slot-value obj 'angle) (slot-value obj 'angle-delta))
  (q+:set-x (location obj) (+ (q+:x (location obj)) (q+:x (velocity obj))))
  (q+:set-y (location obj) (+ (q+:y (location obj)) (q+:y (velocity obj)))))

(defmethod draw ((obj game-object) &key)
  (let ((image (q+:texture (image obj)))
        (size (q+:size (image obj)))
        (location (location obj)))
    (gl:push-matrix)
    (gl:translate (q+:x location) (q+:y location) 0)
    (gl:rotate (slot-value obj 'angle) 0 0 1)
    (q+:draw-texture *main-window*
                     (q+:make-qpointf (- (/ (q+:width size) 2))
                                      (- (/ (q+:height size) 2)))
                     image)
    (gl:pop-matrix)))
