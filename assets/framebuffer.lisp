#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass framebuffer (asset)
  ())

(defmethod coerce-input ((asset framebuffer) (texture texture))
  (list texture :attachment :color-attachment0))

(defmethod coerce-input ((asset framebuffer) (spec list))
  spec)

(defmethod finalize-resource ((type (eql 'framebuffer)) resource)
  (gl:delete-framebuffers (list resource)))

(defmethod load progn ((asset framebuffer))
  (let ((buffer (setf (resource asset) (gl:gen-framebuffer))))
    (with-cleanup-on-failure (offload asset)
      (gl:bind-framebuffer :framebuffer buffer)
      (unwind-protect
           (dolist (input (coerced-inputs asset))
             (destructuring-bind (texture &key (level 0) layer attachment &allow-other-keys) input
               (check-framebuffer-attachment attachment)
               (check-type texture texture)
               (v:debug :trial.asset "Attaching ~a as ~a to ~a." texture attachment asset)
               (if layer
                   (%gl:framebuffer-texture-layer :framebuffer attachment (resource texture) level layer)
                   (%gl:framebuffer-texture :framebuffer attachment (resource texture) level))
               (let ((completeness (gl:check-framebuffer-status :framebuffer)))
                 (unless (or (eql :framebuffer-complete completeness)
                             (eql :framebuffer-complete-oes completeness))
                   (error "Failed to attach ~a as ~s to ~a: ~s"
                          texture attachment asset completeness)))))
        (gl:bind-framebuffer :framebuffer 0)))))

(defmethod resize ((asset framebuffer) width height)
  (let ((loaded (resource asset)))
    (when loaded (offload asset))
    (dolist (input (inputs asset))
      (resize (if (listp input) (first input) input) width height))
    (when loaded (load asset))))


(defclass framebuffer-bundle (asset)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (framebuffer :initform NIL :accessor framebuffer)
   (textures :initform () :accessor textures))
  (:default-initargs
   :width (error "WIDTH required.")
   :height (error "HEIGHT required.")))

(defmethod finalize-resource ((type (eql 'framebuffer-bundle)) resource)
  (mapcar #'offload resource))

;; FIXME: gc?
(defmethod offload progn ((asset framebuffer-bundle))
  (mapcar #'offload (list* (framebuffer asset) (textures asset)))
  (setf (framebuffer asset) NIL)
  (setf (textures asset) ())
  (setf (resource asset) NIL))

(defmethod install-finalizer ((asset framebuffer-bundle))
  (let ((type (type-of asset))
        (resource (list* (framebuffer asset) (textures asset))))
    (tg:finalize asset (lambda () (finalize-resource type resource)))))

(defmethod coerce-input ((asset framebuffer-bundle) (attachment symbol))
  (coerce-input asset (list :attachment attachment)))

(defmethod coerce-input ((asset framebuffer-bundle) (spec cons))
  (let* ((attachment (getf spec :attachment))
         (texspec (remf* spec :level :layer :attachment :bits)))
    (check-framebuffer-attachment attachment)
    (list* (apply #'make-instance 'texture
                  :input (list (getf spec :bits)
                               (width asset) (height asset)
                               (case attachment
                                 (:depth-attachment :depth-component)
                                 (:depth-stencil-attachment :depth-stencil)
                                 (T :rgba)))
                  texspec)
           spec)))

(defmethod load progn ((asset framebuffer-bundle))
  (let ((inputs (coerced-inputs asset)))
    (with-cleanup-on-failure (mapc #'offload (textures asset))
      (dolist (input inputs)
        (push (load (first input)) (textures asset)))
      (setf (textures asset) (nreverse (textures asset)))
      (let ((buffer (make-asset 'framebuffer inputs)))
        (setf (framebuffer asset) (load buffer))
        (setf (resource asset) (resource buffer))))))

(defmethod resize ((asset framebuffer-bundle) width height)
  (let ((loaded (resource asset)))
    (when loaded (offload asset))
    (setf (width asset) width)
    (setf (height asset) height)
    (when loaded (load asset))))
