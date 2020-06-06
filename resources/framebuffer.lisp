#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass framebuffer (gl-resource)
  ((attachments :initarg :attachments :accessor attachments)
   (width :initarg :width :initform NIL :accessor width)
   (height :initarg :height :initform NIL :accessor height))
  (:default-initargs
   :attachments (error "ATTACHMENTS required.")))

(defmethod print-object ((framebuffer framebuffer) stream)
  (print-unreadable-object (framebuffer stream :type T :identity T)
    (format stream "~:{~a ~}" (attachments framebuffer))))

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
           (progn
             (dolist (attachment (attachments framebuffer))
               (destructuring-bind (attachment texture &key (level 0) layer &allow-other-keys) attachment
                 (check-framebuffer-attachment attachment)
                 (check-type texture texture)
                 (check-allocated texture)
                 (v:debug :trial.framebuffer "Attaching ~a~@[:~a~] as ~a to ~a."
                          texture layer attachment framebuffer)
                 (cond ((null (width framebuffer))
                        (setf (width framebuffer) (width texture)))
                       ((/= (width framebuffer) (width texture))
                        (error "Cannot attach~%  ~a~%to~%  ~a~%, as the width is mismatched."
                               texture framebuffer)))
                 (cond ((null (height framebuffer))
                        (setf (height framebuffer) (height texture)))
                       ((/= (height framebuffer) (height texture))
                        (error "Cannot attach~%  ~a~%to~%  ~a~%, as the height is mismatched."
                               texture framebuffer)))
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
             (unless (and (width framebuffer) (height framebuffer))
               (error "The framebuffer has no attachments and no default width and height set!"))
             (unless (attachments framebuffer)
               (when-gl-extension :gl-arb-framebuffer-no-attachments
                 (%gl:framebuffer-parameter-i :framebuffer :framebuffer-default-width (width framebuffer))
                 (%gl:framebuffer-parameter-i :framebuffer :framebuffer-default-height (height framebuffer)))))
        (gl:bind-framebuffer :framebuffer 0)
        (setf (data-pointer framebuffer) fbo)))))

(defmethod deallocate ((framebuffer framebuffer))
  (gl:delete-framebuffers (list (gl-name framebuffer))))

(defmethod resize ((framebuffer framebuffer) width height)
  (dolist (attachment (attachments framebuffer))
    (resize (second attachment) width height))
  (setf (width framebuffer) width)
  (setf (height framebuffer) height))

(defmethod activate ((framebuffer framebuffer))
  (gl:bind-framebuffer :framebuffer (gl-name framebuffer))
  (gl:viewport 0 0 (width framebuffer) (height framebuffer))
  ;; FIXME: Figure out which to clearq depending on framebuffer attachments
  (gl:clear :color-buffer :depth-buffer :stencil-buffer))

(defmethod blit-to-screen ((framebuffer framebuffer))
  (gl:bind-framebuffer :read-framebuffer (gl-name framebuffer))
  (gl:bind-framebuffer :draw-framebuffer 0)
  (%gl:blit-framebuffer 0 0 (width framebuffer) (height framebuffer) 0 0 (width *context*) (height *context*)
                        (cffi:foreign-bitfield-value '%gl::ClearBufferMask :color-buffer)
                        (cffi:foreign-enum-value '%gl:enum :nearest)))
