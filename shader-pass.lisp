#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass shader-pass-class (shader-entity-class flow:static-node-class)
    ()))

(defmethod c2mop:validate-superclass ((class shader-pass-class) (superclass T))
  NIL)

(defmethod c2mop:validate-superclass ((class T) (superclass shader-pass-class))
  NIL)

(defmethod c2mop:validate-superclass ((class shader-pass-class) (superclass standard-class))
  T)

;; FIXME: change texspec on per-instance basis to allow customising stuff
;;        like texture size.
(defclass texture-port (flow:port)
  ((texture :initform NIL :accessor texture)
   (texspec :initarg :texspec :accessor texspec)
   (unit-id :initform NIL :accessor unit-id))
  (:default-initargs
   :texspec ()))

(flow:define-port-value-slot texture-port texture texture)

;; FIXME: What about binding multiple levels and layers of the same texture?
(defclass image-port (texture-port)
  ((binding :initarg :binding :initform 0 :accessor binding)
   (access :initarg :access :initform (error "ACCESS required.") :accessor access)))

(defmethod check-consistent ((port image-port)))

(defclass image-in (image-port flow:in-port flow:1-port)
  ((access :initform :read-only)))

(defclass image-out (image-port flow:out-port flow:n-port)
  ((access :initform :write-only)))

;; FIXME: check for duplicate inputs/outputs.
(defclass uniform-port (flow:port)
  ((uniform-name :initarg :uniform :initform NIL :accessor uniform-name)))

(defmethod initialize-instance :after ((port uniform-port) &key)
  (unless (uniform-name port)
    (setf (uniform-name port) (symbol->c-name (flow:name port)))))

(defclass input (flow:in-port flow:1-port texture-port uniform-port)
  ())

(defmethod check-consistent ((input input))
  (unless (flow:connections input)
    (error "Pipeline is not consistent.~%~
            Pass ~s is missing a connection to its input ~s."
           (flow:node input) input))
  (let ((other (flow:left (first (flow:connections input)))))
    (unless (or (not (texspec input))
                (join-texspec (normalized-texspec (texspec input))
                              (normalized-texspec (texspec other))))
      (error "Pipeline is not consistent.~%~
              Pass ~s' input ~s~%  ~s~%is not texture compatible with output ~s'~%  ~s."
             (flow:node input) input (normalized-texspec (texspec input))
             other (normalized-texspec (texspec other))))))

(defclass output (flow:out-port flow:n-port texture-port)
  ((attachment :initarg :attachment :accessor attachment))
  (:default-initargs :attachment :color-attachment0))

(defmethod check-consistent ((output output))
  ())

(defmethod (setf texture) :after ((new-texture texture) (port output))
  (let ((fb (framebuffer (flow:node port))))
    (when fb
      (setf (attachments fb)
            (loop for (attachment texture . args) in (attachments fb)
                  collect (list* attachment
                                 (if (eql attachment (attachment port))
                                     new-texture
                                     texture)
                                 args))))))

(defclass fixed-input (input)
  ())

(defmethod shared-initialize :after ((input fixed-input) slots &key texture)
  (when texture
    (setf (texture input) (eval texture))))

(defmethod stage ((input fixed-input) (area staging-area))
  (stage (texture input) area))

(defmethod check-consistent ((input fixed-input))
  (unless (texture input)
    (error "Pass ~s is missing an input texture ~s."
           (flow:node input) input)))

(defclass static-input (flow:out-port flow:n-port texture-port uniform-port)
  ())

(defmethod check-consistent ((input static-input)))

(define-shader-entity shader-pass (flow:static-node)
  ((framebuffer :initform NIL :accessor framebuffer)
   (active-p :initform T :accessor active-p))
  (:metaclass shader-pass-class)
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod shared-initialize :after ((pass shader-pass) slots &key)
  (loop with texture-index = (max 16 (gl:get-integer :max-texture-image-units))
        for port in (flow:ports pass)
        do (typecase port
             (texture-port
              (setf (unit-id port) (decf texture-index))))))

(defclass transformed () ())
(defclass renderable () ())
(defclass dynamic-renderable (renderable) ())

(defgeneric apply-transforms (object)
  (:method-combination progn :most-specific-last))
(defgeneric bind-textures (object))
(defgeneric object-renderable-p (object pass))
(defgeneric compute-shader (shader-type pass object))

(defmethod object-renderable-p (object (pass shader-pass)) NIL)
(defmethod object-renderable-p ((renderable renderable) (pass shader-pass)) T)

(defmethod apply-transforms progn ((object renderable)))
(defmethod bind-textures ((object renderable)))
(defmethod bind-textures ((object standalone-shader-entity)))

(defmethod stage ((pass shader-pass) (area staging-area))
  (dolist (port (flow:ports pass))
    (stage port area))
  (stage (framebuffer pass) area))

(defmethod check-consistent ((pass shader-pass))
  (dolist (port (flow:ports pass))
    (check-consistent port)))

(defmethod compute-shader (type pass object)
  ())

(defmethod compute-shader (type (pass shader-pass) object)
  (append (call-next-method) (enlist (effective-shader type pass))))

(defmethod compute-shader (type pass (object shader-entity))
  (append (call-next-method) (enlist (effective-shader type object))))

(defmethod compute-shader (type pass (object shader-entity-class))
  (append (call-next-method) (enlist (effective-shader type object))))

(defmethod make-pass-shader-program ((pass shader-pass) object)
  ;; TODO: alias program against identical programs
  (let* ((shaders ())
         (buffers (delete-duplicates (append (buffers object) (buffers pass))))
         (directives (append (compute-preprocessor-directives pass)
                             (when (typep object 'shader-entity)
                               (compute-preprocessor-directives object))
                             (mapcar #'gl-source buffers))))
    (loop for type in *shader-type-list*
          for inputs = (compute-shader type pass object)
          do (when inputs
               (let ((input (glsl-toolkit:merge-shader-sources
                             (list `(glsl-toolkit:shader ,@directives)
                                   (glsl-toolkit:combine-methods inputs))
                             :min-version (glsl-target-version T))))
                 (push (make-instance 'shader :source input :type type) shaders))))
    (make-instance 'shader-program :shaders shaders :buffers buffers)))

(defmethod make-pass-shader-program ((pass shader-pass) (entity standalone-shader-entity))
  (shader-program entity))

(defmethod finalize :after ((pass shader-pass))
  (when (framebuffer pass)
    (finalize (framebuffer pass))))

(defmethod enter ((container container) (pass shader-pass))
  (when (next-method-p)
    (call-next-method))
  (for:for ((object over container))
    (enter object pass)))

(defmethod leave ((container container) (pass shader-pass))
  (when (next-method-p)
    (call-next-method))
  (for:for ((object over container))
    (leave object pass)))

(defmethod render (object (pass shader-pass))
  (render object (shader-program-for-pass pass object)))

(defmethod width ((pass shader-pass))
  (width (framebuffer pass)))

(defmethod height ((pass shader-pass))
  (height (framebuffer pass)))

(defmacro define-shader-pass (name direct-superclasses direct-slots &rest options)
  (setf direct-superclasses (append direct-superclasses (list 'shader-pass)))
  (unless (find :metaclass options :key #'car)
    (push '(:metaclass shader-pass-class) options))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(defmethod prepare-pass-program ((pass shader-pass) program)
  (activate program)
  (loop for slot in (c2mop:class-slots (class-of pass))
        when (flow:port-type slot)
        do (let ((port (flow::port-slot-value pass slot)))
             (typecase port
               (uniform-port
                (if (texture port)
                    (setf (uniform program (uniform-name port)) (unit-id port))
                    (setf (uniform program (uniform-name port)) (slot-value pass (c2mop:slot-definition-name slot)))))))))

(defmethod bind-textures ((pass shader-pass))
  ;; FIXME: I kinda hate this and we could definitely optimise the iteration away.
  (loop for slot in (c2mop:class-slots (class-of pass))
        when (flow:port-type slot)
        do (let ((port (flow::port-slot-value pass slot)))
             (typecase port
               (uniform-port
                (when (texture port)
                  (gl:active-texture (unit-id port))
                  (gl:bind-texture (target (texture port)) (gl-name (texture port)))))
               (image-port
                (when (texture port)
                  (%gl:bind-image-texture (binding port) (gl-name (texture port)) 0 T 0 (access port)
                                          (internal-format (texture port)))))))))

(defmethod prepare-pass-program :around ((pass shader-pass) (program shader-program))
  (unless (eq +current-shader-program+ program)
    (call-next-method))
  (update-uniforms pass program))

(defmethod blit-to-screen ((pass shader-pass))
  (blit-to-screen (framebuffer pass)))

(defmethod capture ((pass shader-pass) &rest args)
  (apply #'capture (framebuffer pass) args))

(defmethod render :before ((pass shader-pass) target)
  (activate (framebuffer pass))
  (bind-textures pass))

(defmethod render (object (pass shader-pass))
  (let ((program (shader-program-for-pass pass object)))
    (prepare-pass-program pass program)
    (render object program)))

(define-shader-pass per-object-pass (listener)
  ((program-table :initform (make-hash-table :test 'eq) :accessor program-table)
   (renderable-table :initform (make-hash-table :test 'eq) :accessor renderable-table)
   (frame :initform (map-into (make-array 128 :adjustable T :fill-pointer 0) (lambda () (cons NIL NIL))) :accessor frame)))

(defgeneric construct-frame (pass))
(defgeneric render-frame (pass frame))
(defgeneric sort-frame (pass frame))
(defgeneric render-with (pass object program))

(defmethod camera ((pass shader-pass))
  (camera (scene +main+)))

(defmethod scene ((pass shader-pass))
  (scene +main+))

(defmethod sort-frame ((pass per-object-pass) frame)
  frame)

(defmethod map-visible (function (camera camera) (pass per-object-pass))
  (loop for object being the hash-keys of (renderable-table pass) using (hash-value program)
        do (when (and (not (typep object 'class))
                      (in-view-p object camera))
             (funcall function object))))

(defmethod map-visible (function (camera null) (pass per-object-pass))
  (loop for object being the hash-keys of (renderable-table pass) using (hash-value program)
        do (when (not (typep object 'class))
             (funcall function object))))

(defmethod shader-program-for-pass ((pass per-object-pass) object)
  (gethash object (renderable-table pass)))

(defmethod enter (object (pass per-object-pass))
  (when (next-method-p)
    (call-next-method))
  (when (or (object-renderable-p object pass)
            (typep object 'shader-entity-class))
    (let ((renderable-table (renderable-table pass)))
      (unless (gethash object renderable-table)
        (let* ((program-table (program-table pass))
               (target (if (typep object 'dynamic-renderable)
                           object
                           (effective-shader-class object)))
               (program (gethash target renderable-table)))
          (unless program
            (setf program (make-pass-shader-program pass target))
            (unless (gethash program program-table)
              (setf (gethash program program-table) (cons 0 NIL))
              (setf (gethash target renderable-table) program)))
          (incf (car (gethash program program-table)))
          (setf (gethash object renderable-table) program))))))

(defmethod leave (object (pass per-object-pass))
  (when (next-method-p)
    (call-next-method))
  (let* ((renderable-table (renderable-table pass))
         (program (gethash object renderable-table)))
    (when program
      (decf (car (gethash program (program-table pass))))
      (remhash object renderable-table))))

(defmethod stage ((pass per-object-pass) (area staging-area))
  (call-next-method)
  (loop for program being the hash-keys of (program-table pass) using (hash-value (count . cached))
        do (cond ((<= count 0)
                  (remhash program (program-table pass))
                  (loop for other being the hash-values of (renderable-table pass) using (hash-key key)
                        do (when (eq other program) (remhash key (renderable-table pass)))))
                 (T
                  (stage program area)))))

(defmethod stage ((object shader-entity) (pass per-object-pass))
  (unless (typep object 'dynamic-renderable)
    (stage (effective-shader-class object) pass)))

(defmethod stage ((object shader-entity-class) (pass per-object-pass))
  (enter object pass))

;; FIXME: Maybe consider determining effective class for each
;;        individual shader stage as they might each change
;;        at different levels and could thus be cached more
;;        effectively.
;; FIXME: Share SHADER assets between shader programs by caching
;;        them... somewhere somehow?
(defmethod handle ((ev class-changed) (pass per-object-pass))
  (call-next-method)
  (let ((class (changed-class ev))
        (program-table (program-table pass))
        (renderable-table (renderable-table pass)))
    ;; FIXME: What happens if the effective shader class changes?
    ;;        We might be leaking shader programs for stale classes then.
    (flet ((refresh (class)
             (let ((prev (gethash class renderable-table)))
               (v:info :trial.shader-pass "Refreshing shader program for ~a" class)
               (let ((new (make-pass-shader-program pass class)))
                 (setf (buffers prev) (buffers new))
                 (setf (shaders prev) (shaders new))
                 (setf (cdr (gethash prev program-table)) NIL)))))
      (cond ((eql class (class-of pass))
             ;; Pass changed, recompile everything
             (loop for object being the hash-keys of renderable-table
                   do (when (typep object 'standard-class)
                        (refresh object))))
            ((and (gethash class renderable-table) (eql class (effective-shader-class class)))
             ;; Object changed, recompile it
             (refresh class))))))

(defmethod render ((pass per-object-pass) (_ null))
  (render-frame pass (construct-frame pass)))

(defmethod prepare-pass-program ((pass per-object-pass) program)
  (let ((entry (gethash program (program-table pass))))
    (cond ((cdr entry)
           (activate program))
          (T
           (call-next-method)
           (setf (cdr entry) T)))))

(defmethod construct-frame ((pass per-object-pass))
  (let* ((frame (frame pass))
         (index 0)
         (total (array-total-size frame))
         (renderable-table (renderable-table pass)))
    (flet ((store (object program)
             (when (<= total (incf index))
               (adjust-array frame (* 2 total))
               (loop for i from total below (* 2 total)
                     do (setf (aref frame i) (cons NIL NIL)))
               (setf total (* 2 total)))
             (let ((entry (aref frame (1- index))))
               (setf (car entry) object)
               (setf (cdr entry) program))))
      (do-visible (object (camera pass) (scene pass))
        (let ((program (gethash object renderable-table)))
          (when program
            (store object program)))))
    (setf (fill-pointer frame) index)
    (sort-frame pass frame)))

(defmethod render-frame ((pass per-object-pass) frame)
  (declare (type (and vector (not simple-vector)) frame))
  (loop for (object . program) across frame
        do (render-with pass object program)))

(defmethod render-with ((pass per-object-pass) object program)
  (restart-case
      (progn
        (prepare-pass-program pass program)
        (with-pushed-matrix ()
          (apply-transforms object)
          (bind-textures object)
          (update-uniforms object program)
          (render object program)))
    #-kandria-release
    (leave ()
      :report "Leave the object"
      (leave object T))))

(define-shader-pass single-shader-pass ()
  ((shader-program :initform NIL :accessor shader-program)))

(defmethod initialize-instance :after ((pass single-shader-pass) &key)
  (setf (shader-program pass) (make-class-shader-program pass)))

(defmethod stage ((pass single-shader-pass) (area staging-area))
  (call-next-method)
  (stage (shader-program pass) area))

(defmethod handle ((ev class-changed) (pass single-shader-pass))
  (when (eql (changed-class ev) (class-of pass))
    (let ((prev (shader-program pass))
          (new (make-class-shader-program pass)))
      (v:info :trial.shader-pass "Refreshing shader program for ~a" (class-of pass))
      (setf (buffers prev) (buffers new))
      (setf (shaders prev) (shaders new)))))

(defmethod shader-program-for-pass ((pass single-shader-pass) o)
  (shader-program pass))

(defmethod make-pass-shader-program ((pass single-shader-pass) o)
  (shader-program pass))

(defmethod render ((pass single-shader-pass) (_ null))
  (render pass (shader-program pass)))

(defmethod render :around ((pass single-shader-pass) (program shader-program))
  (prepare-pass-program pass program)
  (call-next-method))

(define-shader-pass post-effect-pass (single-shader-pass)
  ((vertex-array :initform (// 'trial 'fullscreen-square) :accessor vertex-array)))

(defmethod stage :after ((pass post-effect-pass) (area staging-area))
  (stage (vertex-array pass) area))

(defmethod object-renderable-p ((renderable renderable) (pass post-effect-pass)) NIL)
(defmethod enter (thing (pass post-effect-pass)))
(defmethod leave (thing (pass post-effect-pass)))
(defmethod handle ((event event) (pass post-effect-pass)))

(defmethod render ((pass post-effect-pass) (program shader-program))
  (render (vertex-array pass) program))

(define-class-shader (post-effect-pass :vertex-shader)
  "
layout (location = 0) in vec3 position;
layout (location = 2) in vec2 in_uv;
out vec2 uv;

void main(){
  gl_Position = vec4(position, 1.0f);
  uv = in_uv;
}")

(define-class-shader (post-effect-pass :fragment-shader)
  "
in vec2 uv;")

(define-shader-pass sample-reduction-pass (post-effect-pass)
  ((previous-pass :port-type input :texspec (:target :texture-2d-multisample))
   (color :port-type output :texspec (:target :texture-2d) :reader color)))

(define-class-shader (sample-reduction-pass :fragment-shader)
  "uniform sampler2DMS previous_pass;
in vec2 uv;
out vec4 color;

void main(){
  color = texture(previous_pass, uv);
}")

(define-shader-pass compute-pass (single-shader-pass)
  ((work-groups :initform (vec 1 1 1) :initarg :work-groups :accessor work-groups)
   (barrier :initform 4294967295)))

(defmethod shared-initialize :after ((pass compute-pass) slots &key (barrier NIL barrier-p))
  (when barrier-p (setf (barrier pass) barrier)))

(defmethod barrier ((pass compute-pass))
  (cffi:foreign-bitfield-symbols '%gl::MemoryBarrierMask (slot-value pass 'barrier)))

(defmethod (setf barrier) ((bits list) (pass compute-pass))
  (setf (slot-value pass 'barrier) (cffi:foreign-bitfield-value '%gl::MemoryBarrierMask bits)))

(defmethod render ((pass single-shader-pass) (program shader-program))
  (let ((work-groups (work-groups pass)))
    (etypecase work-groups
      (vec3
       (%gl:dispatch-compute
        (truncate (vx work-groups))
        (truncate (vy work-groups))
        (truncate (vz work-groups))))
      (integer
       (%gl:dispatch-compute-indirect work-groups))
      (buffer-object
       (%gl:bind-buffer :dispatch-indirect-buffer (gl-name work-groups))
       (%gl:dispatch-compute-indirect 0)))
    (when (/= 0 (barrier pass))
      (%gl:memory-barrier (barrier pass)))))

;; KLUDGE: this sucks as we override more than we need to.
(defmethod (setf class-shader) :before (shader type (class shader-pass-class))
  (when (and (c2mop:subclassp class (find-class 'compute-pass))
             (not (eql type :compute-shader)))
    (error "May only attach a compute shader to compute-passes.")))
