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

(define-subject selection-buffer (framebuffer hud-entity)
  ((resource :initform (tg:make-weak-hash-table :weakness :key)))
  (:default-initargs
   :width (width *context*)
   :height (height *context*)
   :home :trial
   :name :selection-buffer))

(defmethod initialize-instance :after ((buffer selection-buffer) &key)
  (restore buffer))

(define-handler (selection-buffer mouse-release mouse-release 1000) (ev pos)
  (let* ((x (round (vx pos)))
         (y (- (height selection-buffer) (round (vy pos)))))
    (render *loop* selection-buffer)
    (v:info :test "CLICK: ~a/~a => ~a" x y (object-at-point selection-buffer x y))))

(defmethod render (scene (buffer selection-buffer))
  (unless (and (= (width *context*) (width buffer))
               (= (height *context*) (height buffer)))
    ;; Size might have changed since we last updated...
    (reinitialize-instance buffer :width (width *context*) :height (height *context*)))
  (when (q+:bind (data buffer))
    (unwind-protect
         (progn
           (gl:clear-color 0.0 0.0 0.0 0.0)
           (gl:clear :color-buffer :depth-buffer)
           ;; Disable blending and textures to ensure we have just 32bit colours.
           (gl:disable :blend :texture-2d :multisample)
           (gl:enable :depth-test :cull-face)
           (with-pushed-matrix
               ;; FIXME: Multiple cameras? Camera not named this?
               (handle (make-instance 'tick) (unit :camera scene))
             (paint scene buffer)))
      (q+:release (data buffer)))))

(defmethod paint :around (thing (buffer selection-buffer))
  (when (or (typep thing 'selectable-entity)
            (typep thing 'container))
    (call-next-method)))

#+trial-debug-selection-buffer
(defmethod paint-hud ((buffer selection-buffer) target)
  (gl:bind-texture :texture-2d (q+:texture (data buffer)))
  (with-primitives :quads
    (gl:tex-coord 1 1)
    (gl:vertex (width *context*) 0)
    (gl:tex-coord 0 1)
    (gl:vertex 0 0)
    (gl:tex-coord 0 0)
    (gl:vertex 0 (height *context*))
    (gl:tex-coord 1 0)
    (gl:vertex (width *context*) (height *context*)))
  (gl:bind-texture :texture-2d 0))

(defmethod object-at-point ((buffer selection-buffer) x y)
  (when (q+:bind (data buffer))
    (unwind-protect
         (color->object (gl:read-pixels x y 1 1 :rgba :unsigned-byte))
      (q+:release (data buffer)))))

(defclass selectable-entity (entity)
  ((color-id :initarg :color-id :accessor color-id))
  (:default-initargs
   :color-id NIL))

(defmethod initialize-instance :after ((entity selectable-entity) &key)
  (setf (color-id entity) (register-object-color entity (color-id entity))))

(defmethod paint :before ((entity selectable-entity) (buffer selection-buffer))
  (gl:color (/ (ldb (byte 8 24) (color-id entity)) 255)
            (/ (ldb (byte 8 16) (color-id entity)) 255)
            (/ (ldb (byte 8  8) (color-id entity)) 255)
            (/ (ldb (byte 8  0) (color-id entity)) 255)))
