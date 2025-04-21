(in-package #:org.shirakumo.fraf.trial)

(defclass framebuffer (gl-resource)
  ((attachments :initform () :initarg :attachments :accessor attachments)
   (clear-bits :initform 17664 :accessor clear-bits)
   (clear-color :initform (vec4 0) :accessor clear-color)
   (width :initarg :width :initform NIL :accessor width)
   (height :initarg :height :initform NIL :accessor height)))

(defmethod shared-initialize :after ((framebuffer framebuffer) slots &key (clear-bits NIL clear-bits-p) clear-color)
  (when clear-bits-p (setf (clear-bits framebuffer) clear-bits))
  (when clear-color (setf (clear-color framebuffer) clear-color)))

(defmethod print-object ((framebuffer framebuffer) stream)
  (print-unreadable-object (framebuffer stream :type T :identity T)
    (format stream "~@[~d~]x~@[~d~]~:{ ~a~}~:[~; ALLOCATED~]"
            (width framebuffer) (height framebuffer) (attachments framebuffer) (allocated-p framebuffer))))

(defmethod describe-object :after ((framebuffer framebuffer) stream)
  (when (gl-name framebuffer)
    (with-context ()
      (format stream "~&~%Attachments:~%")
      (with-gl-binding (:framebuffer (gl-name framebuffer))
        (cffi:with-foreign-objects ((param :int))
          (dolist (attachment *framebuffer-attachment-list*)
            (%gl:get-framebuffer-attachment-parameter-iv :draw-framebuffer attachment :framebuffer-attachment-object-name param)
            (let ((name (cffi:mem-ref param :int)))
              (when (< 0 name)
                (format stream "  ~a~30t~3d~%" attachment name)))))))))

(defmethod clear-bits ((framebuffer framebuffer))
  (cffi:foreign-bitfield-symbols '%gl::ClearBufferMask (slot-value framebuffer 'clear-bits)))

(defmethod (setf clear-bits) ((bits list) (framebuffer framebuffer))
  (setf (clear-bits framebuffer) (cffi:foreign-bitfield-value '%gl::ClearBufferMask bits))
  bits)

(defmethod (setf clear-color) ((vec vec3) (framebuffer framebuffer))
  (vsetf (clear-color framebuffer)
         (vx vec) (vy vec) (vz vec) 0.0)
  vec)

(defmethod (setf clear-color) ((null null) (framebuffer framebuffer))
  (v<- (clear-color framebuffer) 0)
  null)

(defmethod dependencies ((framebuffer framebuffer))
  (append (call-next-method)
          (mapcar #'second (attachments framebuffer))))

(defun bind-framebuffer-attachments (framebuffer attachments)
  (let ((color-attachments (loop for attachment in attachments
                                 unless (find (first attachment) '(:depth-attachment :stencil-attachment :depth-stencil-attachment))
                                 collect (first attachment))))
    (unless (equal color-attachments (remove-duplicates color-attachments))
      (error "Duplicate color attachments:~%  ~a" color-attachments))
    (with-gl-binding (:framebuffer (gl-name framebuffer))
      (dolist (attachment attachments)
        (destructuring-bind (attachment texture &key (level 0) layer &allow-other-keys) attachment
          (check-framebuffer-attachment attachment)
          (check-type texture texture)
          (check-allocated texture)
          #-nx
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
              (error "Failed to attach ~a as ~s to ~a~@[/~a~]: ~s"
                     texture attachment framebuffer layer completeness)))))
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
  (when (and (allocated-p framebuffer) (not (equal attachments (attachments framebuffer))))
    ;; TODO: usually attachments are only switched out, making the default generic method way too expensive.
    (with-cleanup-on-failure (bind-framebuffer-attachments framebuffer (attachments framebuffer))
      (bind-framebuffer-attachments framebuffer attachments))))

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

(defmethod activate ((framebuffer framebuffer))
  (gl:bind-framebuffer :framebuffer (gl-name framebuffer))
  (gl:viewport 0 0 (width framebuffer) (height framebuffer))
  (let ((bits (slot-value framebuffer 'clear-bits)))
    (when (< 0 bits)
      (let ((c (clear-color framebuffer)))
        (%gl:clear-color (vx c) (vy c) (vz c) (vw c)))
      (%gl:clear bits))))

(defmethod bind ((framebuffer framebuffer) target)
  (gl:bind-framebuffer target (gl-name framebuffer))
  (gl:viewport 0 0 (width framebuffer) (height framebuffer)))

(defmethod deactivate ((framebuffer framebuffer))
  (gl:bind-framebuffer :framebuffer 0))

;; FIXME: this should ideally be more generic, with blitting from one to another framebuffer
;;        and handling the screen as a special framebuffer instance that's always around.
(defmethod blit-to-screen ((framebuffer framebuffer))
  (gl:bind-framebuffer :read-framebuffer (gl-name framebuffer))
  (gl:bind-framebuffer :draw-framebuffer 0)
  ;; Compute offsets so that the blit always happens to the center of the screen.
  (let* ((x- 0) (x+ (width *context*))
         (y- 0) (y+ (height *context*))
         (src-aspect (/ (width framebuffer) (height framebuffer)))
         (dst-aspect (/ (width *context*) (height *context*))))
    (cond ((< src-aspect dst-aspect) ;; Capped by height
           (gl:clear :color-buffer)
           (let ((width (* (width framebuffer) (/ (height *context*) (height framebuffer)))))
             (setf x- (truncate (- (width *context*) width) 2))
             (setf x+ (+ x- width))))
          ((< dst-aspect src-aspect) ;; Capped by width
           (gl:clear :color-buffer)
           (let ((height (* (height framebuffer) (/ (width *context*) (width framebuffer)))))
             (setf y- (truncate (- (height *context*) height) 2))
             (setf y+ (+ y- height)))))
    ;; TODO: cache this somehow.
    (let ((tex (second (find :color-attachment0 (attachments framebuffer) :key #'first))))
      (%gl:blit-framebuffer 0 0 (width framebuffer) (height framebuffer) x- y- x+ y+
                            (cffi:foreign-bitfield-value '%gl::ClearBufferMask :color-buffer)
                            (case (mag-filter tex)
                              (:nearest (cffi:foreign-enum-value '%gl:enum :nearest))
                              (T (cffi:foreign-enum-value '%gl:enum :linear)))))))

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
                              (second (first (attachments source))))
             target type args)
      (let* ((width (or width (- (width source) x)))
             (height (or height (- (height source) y)))
             (size (* (pixel-data-stride pixel-type pixel-format) width height)))
        (mem:with-memory-region (region size)
          (gl:bind-framebuffer :read-framebuffer (gl-name source))
          (%gl:read-pixels 0 0 width height pixel-format pixel-type (memory-region-pointer region))
          (apply #'save-image region target type :width width :height height :pixel-type pixel-type :pixel-format pixel-format args)))))
