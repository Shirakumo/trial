#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass framebuffer (gl-resource)
  ((attachments :initarg :attachments :accessor attachments))
  (:default-initargs
   :attachments (error "ATTACHMENTS required.")))

(defmethod print-object ((framebuffer framebuffer) stream)
  (print-unreadable-object (framebuffer stream :type T :identity T)
    (format stream "~:{~a ~}" (attachments framebuffer))))

(defmethod destructor ((framebuffer framebuffer))
  (let ((fbo (gl-name framebuffer)))
    (lambda () (when fbo (gl:delete-framebuffers (list fbo))))))

(defmethod dependencies ((framebuffer framebuffer))
  (mapcar #'second (attachments framebuffer)))

(defmethod allocate ((framebuffer framebuffer))
  (let ((fbo (gl:gen-framebuffer))
        (color-attachments (loop for attachment in (attachments framebuffer)
                                 unless (find (first attachment) '(:depth-attachment :stencil-attachment :depth-stencil-attachment))
                                 collect (first attachment))))
    (with-cleanup-on-failure (gl:delete-framebuffers (list fbo))
      (gl:bind-framebuffer :framebuffer fbo)
      (unwind-protect
           (dolist (attachment (attachments framebuffer))
             (destructuring-bind (attachment texture &key (level 0) layer &allow-other-keys) attachment
               (check-framebuffer-attachment attachment)
               (check-type texture texture)
               (check-allocated texture)
               (v:debug :trial.framebuffer "Attaching ~a~@[:~a~] as ~a to ~a."
                        texture layer attachment framebuffer)
               (if layer
                   (%gl:framebuffer-texture-layer :framebuffer attachment (gl-name texture) level layer)
                   (%gl:framebuffer-texture :framebuffer attachment (gl-name texture) level))
               (let ((completeness (gl:check-framebuffer-status :framebuffer)))
                 (unless (find completeness '(:framebuffer-complete :framebuffer-complete-oes))
                   (error "Failed to attach ~a as ~s to ~a: ~s"
                          texture attachment framebuffer completeness)))))
        (cond (color-attachments
               (gl:draw-buffers color-attachments))
              (T
               (gl:draw-buffer :none)
               (gl:read-buffer :none)))
        (gl:bind-framebuffer :framebuffer 0)
        (setf (data-pointer framebuffer) fbo)))))

(defmethod resize ((framebuffer framebuffer) width height)
  (dolist (attachment (attachments framebuffer))
    (resize (second attachment) width height)))
