#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *color-id-counter* 0)
(defvar *color-id-map* (trivial-garbage:make-weak-hash-table :test 'eql :weakness :value))

(defun ensure-color (color)
  (etypecase color
    ((unsigned-byte 32) color)
    (cons
     (let ((id 0))
       (setf (ldb (byte 8 0) id) (or (fourth color) 255))
       (setf (ldb (byte 8 8) id) (third color))
       (setf (ldb (byte 8 16) id) (second color))
       (setf (ldb (byte 8 24) id) (first color))
       id))
    ((vector integer 3)
     (let ((id 0))
       (setf (ldb (byte 8 0) id) 255)
       (setf (ldb (byte 8 8) id) (aref color 2))
       (setf (ldb (byte 8 16) id) (aref color 1))
       (setf (ldb (byte 8 24) id) (aref color 0))
       id))
    ((vector integer 4)
     (let ((id 0))
       (setf (ldb (byte 8 0) id) (aref color 3))
       (setf (ldb (byte 8 8) id) (aref color 2))
       (setf (ldb (byte 8 16) id) (aref color 1))
       (setf (ldb (byte 8 24) id) (aref color 0))
       id))))

;; FIXME: To fix the 2^32 limit issue, upon finalization used IDs should
;; be put onto a free queue (or something), which can be popped from
;; once the counter limit is reached.
(defun register-object-color (object &optional id)
  (let ((id (ensure-color (or id (incf *color-id-counter*)))))
    (setf (gethash id *color-id-map*) object)
    id))

(defun color->object (id)
  (gethash (ensure-color id) *color-id-map*))

(define-finalizable selection-buffer (framebuffer)
  ())

(defmethod render (scene (buffer selection-buffer))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer :depth-buffer)
  ;; Disable blending and textures to ensure we have just 32bit colours.
  (gl:disable :blend :texture-2d :multisample)
  (gl:enable :depth-test :cull-face)
  (with-pushed-matrix
    ;; FIXME: Multiple cameras? Camera not named this?
    (handle (make-instance 'tick) (unit :camera scene))
    (paint scene buffer)))

(defmethod paint :around (thing (buffer selection-buffer))
  ;; FIXME: Nested structures?
  (when (or (typep thing 'selectable-subject)
            (typep thing 'container))
    (call-next-method)))

(defmethod paint ((buffer selection-buffer) thing)
  (gl:bind-texture :texture-2d (texture (selection controller)))
  (with-primitives :quads
    (gl:tex-coord 1 1)
    (gl:vertex (width main) 0)
    (gl:tex-coord 0 1)
    (gl:vertex 0 0)
    (gl:tex-coord 0 0)
    (gl:vertex 0 (height main))
    (gl:tex-coord 1 0)
    (gl:vertex (width main) (height main)))
  (gl:bind-texture :texture-2d 0))

(defmethod object-at-point ((buffer selection-buffer) x y)
  (with-framebuffer-bound (buffer)
    (color->object (gl:read-pixels x y 1 1 :rgba :unsigned-byte))))

(define-subject selectable-subject ()
  ((color-id :initarg :color-id :accessor color-id))
  (:default-initargs
   :color-id NIL))

(defmethod initialize-instance :after ((subject selectable-subject) &key)
  (setf (color-id subject) (register-object-color subject (color-id subject))))

(defmethod paint :before ((subject selectable-subject) (buffer selection-buffer))
  (gl:color (/ (ldb (byte 8 24) (color-id subject)) 255)
            (/ (ldb (byte 8 16) (color-id subject)) 255)
            (/ (ldb (byte 8  8) (color-id subject)) 255)
            (/ (ldb (byte 8  0) (color-id subject)) 255)))
