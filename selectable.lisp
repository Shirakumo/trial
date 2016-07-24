#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

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

(define-subject selection-buffer (framebuffer hud-entity unsavable)
  ((resource :initform (tg:make-weak-hash-table :weakness :key))
   (selected :initform NIL :accessor selected)
   (name-map :initform (trivial-garbage:make-weak-hash-table :test 'eql :weakness :value) :reader name-map)
   (color-map :initform (trivial-garbage:make-weak-hash-table :test 'eql :weakness :key) :reader color-map)
   (next-id :initform 0 :accessor next-id))
  (:default-initargs
   :width (width *context*)
   :height (height *context*)
   :home :trial
   :name :selection-buffer))

(defmethod initialize-instance :after ((buffer selection-buffer) &key)
  (restore buffer))

;; FIXME: To fix the 2^32 limit issue, upon finalization used IDs should
;; be put onto a free queue (or something), which can be popped from
;; once the counter limit is reached.
(defmethod register-object-color ((buffer selection-buffer) object &optional id)
  (when (= 0 (object->color buffer object))
    (let ((id (ensure-color (or id (incf (next-id buffer))))))
      (setf (gethash id (name-map buffer)) object)
      (setf (gethash object (color-map buffer)) id)
      id)))

(defmethod color->object ((buffer selection-buffer) id)
  (gethash (ensure-color id) (name-map buffer)))

(defmethod object->color ((buffer selection-buffer) object)
  (gethash object (color-map buffer) 0))

(define-handler (selection-buffer mouse-release mouse-release 1000) (ev pos)
  (let* ((x (round (vx pos)))
         (y (- (height selection-buffer) (round (vy pos)))))
    (render *loop* selection-buffer)
    (let ((object (object-at-point selection-buffer x y))
          (previous (selected selection-buffer)))
      (when object
        (when (and previous (typep previous 'selectable-entity))
          (setf (selected previous) NIL))
        (setf (selected selection-buffer) object)
        (when (typep object 'selectable-entity)
          (setf (selected object) T))))))

(define-handler (selection-buffer enter) (ev entity)
  (when (typep entity 'selectable-entity)
    (register-object-color selection-buffer entity (color-id entity))))

(defmethod enter ((buffer selection-buffer) (scene scene))
  (do-container-tree (unit scene)
    (when (typep unit 'selectable-entity)
      (register-object-color buffer unit (color-id unit)))))

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
           ;; FIXME: How do we know what to deactivate and reactivate?
           ;; Disable blending and textures to ensure we have just 32bit colours.
           (gl:disable :blend :texture-2d :multisample)
           (gl:enable :depth-test :cull-face)
           (with-pushed-matrix
             ;; FIXME: Multiple cameras? Camera not named this?
             (setup-perspective (unit :camera scene) (make-instance 'resize :width (width *context*) :height (height *context*)))
             (project-view (unit :camera scene) (make-instance 'tick))
             (paint scene buffer))
           ;; Reenable
           (gl:enable :blend :texture-2d :multisample))
      (q+:release (data buffer)))))

(defmethod paint :before (thing (buffer selection-buffer))
  (let ((color (object->color buffer thing)))
    (gl:color (/ (ldb (byte 8 24) color) 255)
              (/ (ldb (byte 8 16) color) 255)
              (/ (ldb (byte 8  8) color) 255)
              (/ (ldb (byte 8  0) color) 255))))

(defmethod object-at-point ((buffer selection-buffer) x y)
  (when (q+:bind (data buffer))
    (unwind-protect
         (color->object buffer (gl:read-pixels x y 1 1 :rgba :unsigned-byte))
      (q+:release (data buffer)))))

(define-subject global-selection-buffer (selection-buffer)
  ())

(define-handler (global-selection-buffer enter) (ev entity)
  (register-object-color global-selection-buffer entity (color-id entity)))

(defmethod enter ((buffer global-selection-buffer) (scene scene))
  (do-container-tree (unit scene)
    (register-object-color buffer unit (color-id unit))))

(defclass selectable-entity (entity)
  ((color-id :initarg :color-id :accessor color-id)
   (selected :initform NIL :accessor selected))
  (:default-initargs
   :color-id NIL))

#+trial-debug-selection-buffer
(defmethod paint ((buffer selection-buffer) (hud hud))
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

#+trial-debug-selection-buffer
(defmethod paint :around ((entity selectable-entity) target)
  (cond ((selected entity)
         (gl:polygon-mode :back :line)
         (gl:cull-face :front)
         (gl:depth-func :lequal)
         (gl:color 255 0 0)
         (gl:line-width 5)
         (call-next-method)
         (gl:cull-face :back)
         (gl:polygon-mode :back :fill)
         (gl:color 255 255 255)
         (call-next-method))
        (T (call-next-method))))
