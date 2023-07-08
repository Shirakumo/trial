#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass framebuffer (gl-resource)
  ((attachments :initform () :initarg :attachments :accessor attachments)
   (clear-bits :initform 17664 :accessor clear-bits)
   (width :initarg :width :initform NIL :accessor width)
   (height :initarg :height :initform NIL :accessor height)))

(defmethod shared-initialize :after ((framebuffer framebuffer) slots &key clear-bits)
  (when clear-bits (setf (clear-bits framebuffer) clear-bits)))

(defmethod print-object ((framebuffer framebuffer) stream)
  (print-unreadable-object (framebuffer stream :type T :identity T)
    (format stream "~:{~a ~}~:[~; ALLOCATED~]" (attachments framebuffer) (allocated-p framebuffer))))

(defmethod clear-bits ((framebuffer framebuffer))
  (cffi:foreign-bitfield-symbols '%gl::ClearBufferMask (slot-value framebuffer 'clear-bits)))

(defmethod (setf clear-bits) ((bits list) (framebuffer framebuffer))
  (setf (clear-bits framebuffer) (cffi:foreign-bitfield-value '%gl::ClearBufferMask bits)))

(defmethod dependencies ((framebuffer framebuffer))
  (append (call-next-method)
          (mapcar #'second (attachments framebuffer))))

(defun check-framebuffer-size (framebuffer texture)
  (cond ((null (width framebuffer))
         (setf (width framebuffer) (width texture)))
        ((/= (width framebuffer) (width texture))
         (error "Cannot attach~%  ~a~%to~%  ~a~%, as the width is mismatched."
                texture framebuffer)))
  (cond ((null (height framebuffer))
         (setf (height framebuffer) (height texture)))
        ((/= (height framebuffer) (height texture))
         (error "Cannot attach~%  ~a~%to~%  ~a~%, as the height is mismatched."
                texture framebuffer))))

(defun bind-framebuffer-attachments (framebuffer attachments)
  (let ((color-attachments (loop for attachment in attachments
                                 unless (find (first attachment) '(:depth-attachment :stencil-attachment :depth-stencil-attachment))
                                 collect (first attachment))))
    (unless (equal color-attachments (remove-duplicates color-attachments))
      (error "Duplicate color attachments:~%  ~a" color-attachments))
    (gl:bind-framebuffer :framebuffer (gl-name framebuffer))
    (with-unwind-protection (gl:bind-framebuffer :framebuffer 0)
      (dolist (attachment attachments)
        (destructuring-bind (attachment texture &key (level 0) layer &allow-other-keys) attachment
          (check-framebuffer-attachment attachment)
          (check-type texture texture)
          (check-allocated texture)
          (check-framebuffer-size framebuffer texture)
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
      (unless (and (width framebuffer) (height framebuffer))
        (error "The framebuffer has no attachments and no default width and height set!"))
      (unless attachments
        (when-gl-extension :gl-arb-framebuffer-no-attachments
          (%gl:framebuffer-parameter-i :framebuffer :framebuffer-default-width (width framebuffer))
          (%gl:framebuffer-parameter-i :framebuffer :framebuffer-default-height (height framebuffer)))))))

(defmethod (setf attachments) :before (attachments (framebuffer framebuffer))
  (when (allocated-p framebuffer)
    (with-cleanup-on-failure (bind-framebuffer-attachments framebuffer (attachments framebuffer))
      (bind-framebuffer-attachments framebuffer attachments))))

(defmethod bind ((texture texture) (framebuffer framebuffer))
  (check-framebuffer-size framebuffer texture)
  (gl:bind-framebuffer (gl-name framebuffer))
  (case (internal-format texture)
    ((:depth-component :depth-component16 :depth-component24 :depth-component32 :depth-component32f)
     (%gl:framebuffer-texture :framebuffer :depth-attachment (gl-name texture) 0))
    ((:stencil-index :stencil-index1 :stencil-index4 :stencil-index8 :stencil-index16)
     (%gl:framebuffer-texture :framebuffer :stencil-attachment (gl-name texture) 0))
    ((:depth-stencil :depth24-stencil8 :depth32f-stencil8)
     (%gl:framebuffer-texture :framebuffer :depth-stencil-attachment (gl-name texture) 0))
    (T
     (%gl:framebuffer-texture :framebuffer :color-attachment0 (gl-name texture) 0))))

(defmethod allocate ((framebuffer framebuffer))
  (let ((fbo (gl:gen-framebuffer)))
    (with-cleanup-on-failure (gl:delete-framebuffers (list fbo))
      (setf (data-pointer framebuffer) fbo)
      (bind-framebuffer-attachments framebuffer (attachments framebuffer)))))

(defmethod deallocate ((framebuffer framebuffer))
  (gl:delete-framebuffers (list (gl-name framebuffer))))

(defmethod resize ((framebuffer framebuffer) width height)
  (let ((width (max 1 width))
        (height (max 1 height)))
    (dolist (attachment (attachments framebuffer))
      (resize (second attachment) width height))
    (setf (width framebuffer) width)
    (setf (height framebuffer) height)))

;; TODO: avoid rebinding framebuffer if already bound
(defmethod activate ((framebuffer framebuffer))
  (gl:bind-framebuffer :framebuffer (gl-name framebuffer))
  (gl:viewport 0 0 (width framebuffer) (height framebuffer))
  (let ((bits (slot-value framebuffer 'clear-bits)))
    (when (< 0 bits) (%gl:clear bits))))

(defmethod clear ((framebuffer framebuffer))
  (gl:bind-framebuffer :framebuffer (gl-name framebuffer))
  (let ((bits (slot-value framebuffer 'clear-bits)))
    (when (< 0 bits) (%gl:clear bits))))

(defmethod render ((source framebuffer) (target integer))
  (gl:bind-framebuffer :read-framebuffer (gl-name source))
  (gl:bind-framebuffer :draw-framebuffer target)
  (%gl:blit-framebuffer 0 0 (trial:width source) (trial:height source)
                        0 0 (trial:width source) (trial:height source)
                        '(:color-buffer :depth-buffer :stencil-buffer) :linear))

(defmethod render ((source framebuffer) (target framebuffer))
  (gl:bind-framebuffer :read-framebuffer (gl-name source))
  (gl:bind-framebuffer :draw-framebuffer (gl-name target))
  (%gl:blit-framebuffer 0 0 (trial:width source) (trial:height source)
                        0 0 (trial:width target) (trial:height target)
                        '(:color-buffer :depth-buffer :stencil-buffer) :linear))

(defmethod render ((source framebuffer) (target null))
  (render source 0))

(defmethod blit-to-screen ((framebuffer framebuffer))
  (render framebuffer 0))

(defgeneric capture (thing &key &allow-other-keys))
(defmethod capture ((framebuffer framebuffer) &key (x 0) (y 0) (width (width framebuffer)) (height (height framebuffer))
                                                   (target-width width) (target-height height) file)
  (if (and (= width target-width) (= height target-height))
      (save-image framebuffer file T :x x :y y :width width :height height)
      (let ((dummy (gl:gen-framebuffer))
            (dummy-tex (gl:gen-texture)))
        (unwind-protect
             (progn
               ;; Create dummy framebuffer
               (gl:bind-texture :texture-2d dummy-tex)
               (gl-extension-case
                 (:gl-arb-texture-storage
                  (%gl:tex-storage-2d :texture-2d 1 :rgb8 target-width target-height))
                 (T
                  (%gl:tex-image-2d :texture-2d 0 (cffi:foreign-enum-value '%gl:enum :rgb8) target-width target-height 0 :rgb :unsigned-byte (cffi:null-pointer))))
               (gl:bind-framebuffer :draw-framebuffer dummy)
               (%gl:framebuffer-texture :draw-framebuffer :color-attachment0 dummy-tex 0)
               (gl:draw-buffers '(:color-attachment0))
               (gl:bind-framebuffer :read-framebuffer (gl-name framebuffer))
               ;; Blit over what we need
               (%gl:blit-framebuffer x y width height 0 0 target-width target-height
                                     (cffi:foreign-bitfield-value '%gl::ClearBufferMask :color-buffer)
                                     (cffi:foreign-enum-value '%gl:enum :linear))
               ;; Capture the buffer
               (save-image (make-instance 'framebuffer :data-pointer dummy :width target-width :height target-height) file T))
          (gl:delete-framebuffers (list dummy))
          (gl:delete-texture dummy-tex)))))

(defmethod capture ((framebuffer null) &rest args)
  (unless (visible-p *context*)
    (error "Cannot capture invisible backbuffer."))
  (apply #'capture (make-instance 'framebuffer :data-pointer 0 :width (width *context*) :height (height *context*)) args))

(defmethod save-image ((source framebuffer) target type &rest args &key attachment (pixel-type :unsigned-byte) (pixel-format :rgb)
                                                                        (x 0) (y 0) width height)
  (if (attachments source)
      (apply #'save-image (if attachment
                              (or (second (find attachment (attachments source) :key #'second))
                                  (error "No ~s attachment on ~s" attachment source))
                              (first (attachments source)))
             target type args)
      (let* ((width (or width (- (width source) x)))
             (height (or height (- (height source) y)))
             (size (* (pixel-data-stride pixel-type pixel-format) width height)))
        (mem:with-memory-region (region size)
          (gl:bind-framebuffer :read-framebuffer (gl-name source))
          (%gl:read-pixels 0 0 width height pixel-format pixel-type (memory-region-pointer region))
          (apply #'save-image region target type :width width :height height :pixel-type pixel-type :pixel-format pixel-format args)))))
