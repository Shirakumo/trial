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

(defmethod object-at-point ((buffer selection-buffer) x y)
  (when (q+:bind (data buffer))
    (unwind-protect
         (color->object buffer (gl:read-pixels x y 1 1 :rgba :unsigned-byte))
      (q+:release (data buffer)))))

(defmethod object-at-mouse ((buffer selection-buffer) pos)
  (let* ((x (round (vx pos)))
         (y (- (height buffer) (round (vy pos)))))
    (render *loop* buffer)
    (object-at-point buffer x y)))

(defclass mouse-press-entity (event)
  ((entity :initarg :entity :reader entity)
   (button :initarg :button :reader button)))

(defclass mouse-release-entity (event)
  ((entity :initarg :entity :reader entity)
   (button :initarg :button :reader button)))

(define-handler (selection-buffer mouse-press) (ev pos)
  (let ((object (object-at-mouse selection-buffer pos)))
    (when object (issue *loop* 'mouse-press-entity :entity object :button (button ev)))))

(define-handler (selection-buffer mouse-release mouse-release 1000) (ev pos)
  (let ((object (object-at-mouse selection-buffer pos)))
    (when (eql (button ev) :left)
      (setf (selected selection-buffer) object))
    (when object (issue *loop* 'mouse-release-entity :entity object :button (button ev)))))

(define-handler (selection-buffer enter) (ev entity)
  (when (typep entity 'color-id-entity)
    (register-object-color selection-buffer entity (color-id entity))))

(defmethod enter ((buffer selection-buffer) (scene scene))
  (do-container-tree (unit scene)
    (when (typep unit 'color-id-entity)
      (register-object-color buffer unit (color-id unit)))))

(defmethod render (scene (buffer selection-buffer))
  (unless (and (= (width *context*) (width buffer))
               (= (height *context*) (height buffer)))
    ;; Size might have changed since we last updated...
    (reinitialize-instance buffer :width (width *context*) :height (height *context*)))
  (when (q+:bind (data buffer))
    (gl:push-attrib :all-attrib-bits)
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
             (paint scene buffer)))
      (gl:pop-attrib)
      (q+:release (data buffer)))))

(defmethod paint :before (thing (buffer selection-buffer))
  (let ((color (object->color buffer thing)))
    (gl:color (/ (ldb (byte 8 24) color) 255)
              (/ (ldb (byte 8 16) color) 255)
              (/ (ldb (byte 8  8) color) 255)
              (/ (ldb (byte 8  0) color) 255))))

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

(define-subject global-selection-buffer (selection-buffer)
  ())

(define-handler (global-selection-buffer enter) (ev entity)
  (register-object-color global-selection-buffer entity (color-id entity)))

(defmethod enter ((buffer global-selection-buffer) (scene scene))
  (do-container-tree (unit scene)
    (register-object-color buffer unit (color-id unit))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass color-id-entity (entity)
    ((color-id :initarg :color-id :accessor color-id))
    (:default-initargs
     :color-id NIL)))

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

(define-subject selectable-entity (color-id-entity)
  ((selected :initform NIL :accessor selected)))

(define-handler (selectable-entity mouse-release) (ev)
  (when (eql (button ev) :left)
    (setf (selected selectable-entity) NIL)))

(define-handler (selectable-entity mouse-release-entity) (ev entity)
  (when (and (eql (button ev) :left)
             (eql entity selectable-entity))
    (setf (selected entity) T)))

(define-subject draggable-entity (color-id-entity)
  ((held :initform NIL :accessor held)))

(define-handler (draggable-entity mouse-release) (ev)
  (setf (held draggable-entity) NIL))

(define-handler (draggable-entity mouse-press-entity) (ev entity)
  (when (and (button ev) :left
             (eql entity draggable-entity))
    (setf (held entity) T)))

(define-handler (draggable-entity mouse-move) (ev old-pos pos)
  (when (held draggable-entity)
    (drag draggable-entity old-pos pos)))

(defgeneric drag (object from to)
  (:method ((entity draggable-entity) from to)))
