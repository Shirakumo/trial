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
   (texspec :initarg :texspec :accessor texspec))
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

(defclass static-input (input)
  ())

(defmethod check-consistent ((input static-input)))

(define-shader-entity shader-pass (flow:static-node)
  ((framebuffer :initform NIL :accessor framebuffer)
   (active-p :initform T :accessor active-p)
   (prepare-pass-program-fun :initform NIL :accessor prepare-pass-program-fun))
  (:metaclass shader-pass-class)
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod shared-initialize :after ((pass shader-pass) slots &key)
  (setf (prepare-pass-program-fun pass) NIL))

(defclass transformed () ())
(defclass renderable () ())

(defgeneric apply-transforms (object)
  (:method-combination progn :most-specific-last))
(defgeneric object-renderable-p (object pass))
(defgeneric compile-to-pass (scene pass))
(defgeneric compile-into-pass (object precedent pass))
(defgeneric remove-from-pass (object pass))
(defgeneric shader-program-for-pass (pass object))
(defgeneric make-pass-shader-program (pass object))
(defgeneric coerce-pass-shader (pass object type))

(defmethod object-renderable-p (object (pass shader-pass)) NIL)
(defmethod object-renderable-p ((renderable renderable) (pass shader-pass)) T)

(defmethod stage ((pass shader-pass) (area staging-area))
  (stage (framebuffer pass) area))

(define-class-shader (shader-pass :fragment-shader)
  "#version 330 core")

(defmethod check-consistent ((pass shader-pass))
  (dolist (port (flow:ports pass))
    (check-consistent port)))

(defmethod make-pass-shader-program (pass (class symbol))
  (make-pass-shader-program pass (find-class class)))

(defmethod make-pass-shader-program (pass (object shader-entity))
  (make-pass-shader-program pass (class-of object)))

(defmethod make-pass-shader-program ((pass shader-pass) (class shader-entity-class))
  (let ((shaders ())
        (buffers ()))
    (loop for type in *shader-type-list*
          for inputs = (coerce-pass-shader pass class type)
          for shader = (make-instance 'shader :source inputs :type type)
          do (when inputs (push shader shaders)))
    (loop for resource-spec in (effective-buffers class)
          do (push (apply #'// resource-spec) buffers))
    (loop for resource-spec in (effective-buffers pass)
          do (pushnew (apply #'// resource-spec) buffers))
    (make-instance 'shader-program
                   :shaders shaders
                   :buffers buffers)))

(defmethod finalize :after ((pass shader-pass))
  (when (framebuffer pass)
    (finalize (framebuffer pass))))

(defmethod render (object (pass shader-pass))
  (render object (shader-program-for-pass pass object)))

(defmethod width ((pass shader-pass))
  (width (framebuffer pass)))

(defmethod height ((pass shader-pass))
  (height (framebuffer pass)))

(defmacro define-shader-pass (&environment env name direct-superclasses direct-slots &rest options)
  (setf direct-superclasses (append direct-superclasses (list 'shader-pass)))
  (unless (find :metaclass options :key #'car)
    (push '(:metaclass shader-pass-class) options))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(defun generate-bind-pass-textures (&optional (units (gl:get* :max-texture-image-units)))
  (check-type units (integer 1))
  (let ((*print-case* (readtable-case *readtable*))
        (units (loop for i downfrom (1- units) to 0 collect i)))
    `(lambda (pass)
       (loop with texture-name = ',(loop for unit in units collect
                                         (kw (format NIL "~a~a" :texture unit)))
             for port in (flow:ports pass)
             do (typecase port
                  (uniform-port
                   (when (texture port)
                     (gl:active-texture (pop texture-name))
                     (gl:bind-texture :texture-2d (gl-name (texture port)))))
                  (image-port
                   (when (texture port)
                     (%gl:bind-image-texture (binding port) (gl-name (texture port)) 0 T 0 (access port)
                                             (internal-format (texture port))))))))))

(defun generate-prepare-pass-program (pass &optional (units (gl:get* :max-texture-image-units)))
  (check-type units (integer 1))
  (let ((*print-case* (readtable-case *readtable*))
        (units (loop for i downfrom (1- units) to 0 collect i)))
    `(lambda (program)
       (gl:use-program (gl-name program))
       ,@(loop with texture-index = units
               for slot in (c2mop:class-slots (class-of pass))
               when (flow:port-type slot)
               collect (let ((port (flow::port-slot-value pass slot)))
                         (typecase port
                           (uniform-port
                            (when (texture port)
                              `(setf (uniform program ,(uniform-name port)) ,(pop texture-index))))))))))

(defun bind-pass-textures (pass)
  (funcall (compile 'bind-pass-textures (generate-bind-pass-textures))
           pass))

(defmethod prepare-pass-program ((pass shader-pass) (program shader-program))
  (let ((fun (prepare-pass-program-fun pass)))
    (unless fun
      (setf fun (setf (prepare-pass-program-fun pass) (compile NIL (generate-prepare-pass-program pass)))))
    (funcall fun program)))

(defmethod blit-to-screen ((pass shader-pass))
  (blit-to-screen (framebuffer pass)))

(defmethod capture ((pass shader-pass) &rest args)
  (apply #'capture (framebuffer pass) args))

(defmethod render :before ((pass shader-pass) target)
  (activate (framebuffer pass))
  (bind-pass-textures pass))

(defmethod render (object (pass shader-pass))
  (let ((program (shader-program-for-pass pass object)))
    (prepare-pass-program pass program)
    (render object program)))

(define-shader-pass scene-pass (listener)
  ((actions :initform (make-instance 'flare-queue:queue) :accessor actions)
   (group-pointers :initform (make-hash-table :test 'eq) :accessor group-pointers)
   (guards :initform (cons NIL NIL) :accessor guards)))

;;; KLUDGE: The protocol that follows is EXTREMELY bad under parallel updates.
;;;         If we ever want to allow such (and it's very possible we do), this will
;;;         need to be revised thoroughly.
(defun push-pass-action (pass action)
  (flare-queue:cell-insert-before
   (flare-queue:make-cell (list* (fdefinition (first action)) (rest action)) NIL NIL)
   (cdr (guards pass)))
  (incf (slot-value (actions pass) 'flare-queue::size)))

(defun finish-pass-group (pass object)
  ;; TODO: optimisation, contract empty groups.
  ;;       Though this might be bad for future dynamic inserts, so I'm not sure.
  ;;       Would have to reconstruct the context when a previously empty group
  ;;       becomes populated.
  (destructuring-bind (start . end) (guards pass)
    ;; If the group is empty, insert a NOOP.
    (when (eql (flare-queue:right start) end)
      (push-pass-action pass '(null NIL)))
    (setf (gethash object (group-pointers pass)) (cons (flare-queue:right start) (flare-queue:left end)))
    (setf (guards pass) (cons (flare-queue:left end) end))))

(defmethod compile-to-pass (object (pass scene-pass))
  (when (object-renderable-p object pass)
    (let ((program (register-object-for-pass pass object)))
      (when (typep pass 'per-object-pass)
        (push-pass-action pass `(prepare-pass-program ,pass ,program)))
      (push-pass-action pass `(render ,object ,program)))))

(defmethod compile-to-pass :around ((object transformed) (pass scene-pass))
  ;; KLUDGE: early out to avoid allocating pointless push/pop pairs.
  (when (or (object-renderable-p object pass)
            (typep object 'flare:container))
    (push-pass-action pass `(push-matrix))
    (push-pass-action pass `(apply-transforms ,object))
    (call-next-method)
    (push-pass-action pass `(pop-matrix))))

(defmethod compile-to-pass :after ((object flare:container) (pass scene-pass))
  (for:for ((child over object))
    (compile-to-pass child pass)
    ;; KLUDGE: We can't do this in another method for the OBJECT, as the
    ;;         AROUND for TRANSFORMED must happen before we finish the group.
    (finish-pass-group pass child)))

(defmethod compile-to-pass :around ((scene scene) (pass scene-pass))
  (flare-queue:clear-queue (actions pass))
  (clrhash (group-pointers pass))
  (setf (guards pass) (cons (flare-queue::head (actions pass))
                            (flare-queue::tail (actions pass))))
  (call-next-method)
  (finish-pass-group pass scene))

(defmethod remove-from-pass ((entity entity) (pass scene-pass))
  (when (gethash entity (group-pointers pass))
    (destructuring-bind (start . end) (gethash entity (group-pointers pass))
      ;; The saved group is just a guard so fuse them together and the rest drops out magically.
      ;; FIXME: this does not adjust the queue's size!
      (flare-queue:remove-cells start end)
      (remhash entity (group-pointers pass)))))

(defmethod compile-into-pass :around ((entity entity) previous (pass scene-pass))
  (when (object-renderable-p entity pass)
    (call-next-method)))

(defmethod compile-into-pass ((entity entity) (container flare:container) (pass scene-pass))
  (let ((group-pointers (group-pointers pass)))
    (loop for prev = (preceding-entity entity container) then (preceding-entity prev container)
          for guards = (gethash prev group-pointers)
          while prev
          do (when guards
               (return (compile-into-pass entity prev pass)))
          finally (compile-into-pass entity NIL pass))))

(defmethod compile-into-pass ((entity entity) (previous entity) (pass scene-pass))
  (destructuring-bind (start . end) (gethash previous (group-pointers pass))
    (declare (ignore start))
    (setf (guards pass) (cons end (flare-queue:right end)))
    (compile-to-pass entity pass)
    (finish-pass-group pass entity)))

(defmethod compile-into-pass ((entity entity) (previous null) (pass scene-pass))
  (let ((container (container entity)))
    (destructuring-bind (start . end) (gethash container (group-pointers pass))
      (declare (ignore end))
      (when (typep container 'transformed)
        (setf start (flare-queue:right (flare-queue:right start))))
      (setf (guards pass) (cons start (flare-queue:right start)))
      (compile-to-pass entity pass)
      (finish-pass-group pass entity))))

(defmethod handle ((ev class-changed) (pass scene-pass))
  (call-next-method)
  ;; FIXME: Need to re-evaluate groups, but this can be difficult.
  ;;        If a class changes to be one that should now be included in the actions
  ;;        somehow, but was not before, it will not have a group that we can update.
  ;;        We'd also not know 'where' to insert the new group, but parent relations
  ;;        are typically not kept. What to do?
  )

(defmethod render ((pass scene-pass) target)
  (declare (optimize speed))
  (flare-queue:do-queue (action (actions pass))
    (apply (the function (car action)) (the list (cdr action)))))

(define-shader-pass per-object-pass ()
  ((assets :initform (make-hash-table :test 'eq) :accessor assets)))

(defmethod stage ((pass per-object-pass) (area staging-area))
  (call-next-method)
  (loop for asset being the hash-values of (assets pass)
        do (stage asset area)))

;; FIXME: Maybe consider determining effective class for each
;;        individual shader stage as they might each change
;;        at different levels and could thus be cached more
;;        effectively.
;; FIXME: Share SHADER assets between shader programs by caching
;;        them... somewhere somehow?
(defmethod handle ((ev class-changed) (pass per-object-pass))
  (call-next-method)
  (let ((class (changed-class ev))
        (assets (assets pass)))
    (when (typep class 'shader-entity-class)
      ;; FIXME: What happens if the effective shader class changes?
      ;;        We might be leaking shader programs for stale classes then.
      (flet ((refresh (class)
               (let ((prev (gethash class assets)))
                 (when prev
                   (v:info :trial.shader-pass "Refreshing shader program for ~a" class)
                   (let ((new (make-pass-shader-program pass class)))
                     (setf (buffers prev) (buffers new))
                     (setf (shaders prev) (shaders new)))))))
        (cond ((eql class (class-of pass))
               ;; Pass changed, recompile everything
               (loop for class being the hash-keys of assets
                     do (refresh class)))
              ((eql class (effective-shader-class class))
               ;; Object changed, recompile it
               (refresh class)))))))

(defmethod shader-program-for-pass ((pass per-object-pass) (entity shader-entity))
  (gethash (effective-shader-class entity) (assets pass)))

(defmethod coerce-pass-shader ((pass per-object-pass) class type)
  ;; FIXME: This re-introduces shaders from the pass that were suppressed in the
  ;;        object and vice-versa
  (let ((sources (remove NIL (list (effective-shader type class)
                                   (effective-shader type pass)))))
    (when sources
      (glsl-toolkit:merge-shader-sources sources))))

(defmethod register-object-for-pass ((pass per-object-pass) (class shader-entity-class))
  (let ((effective-class (effective-shader-class class)))
    (or (gethash effective-class (assets pass))
        (let ((program (make-pass-shader-program pass effective-class)))
          (when (gl-name (framebuffer pass))
            (mapc #'load (dependencies program))
            (load program))
          (setf (gethash effective-class (assets pass)) program)))))

(defmethod register-object-for-pass ((pass per-object-pass) (entity shader-entity))
  (register-object-for-pass pass (class-of entity)))

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

(defmethod register-object-for-pass ((pass single-shader-pass) o)
  (shader-program pass))

(defmethod shader-program-for-pass ((pass single-shader-pass) o)
  (shader-program pass))

(defmethod render ((pass single-shader-pass) (_ null))
  (render pass (shader-program pass)))

(defmethod render :around ((pass single-shader-pass) (program shader-program))
  (prepare-pass-program pass program)
  (call-next-method))

(define-shader-pass single-shader-scene-pass (single-shader-pass scene-pass)
  ())

(define-shader-pass post-effect-pass (single-shader-pass)
  ((vertex-array :initform (// 'trial 'fullscreen-square) :accessor vertex-array)))

(defmethod stage :after ((pass post-effect-pass) (area staging-area))
  (stage (vertex-array pass) area))

(defmethod compile-to-pass (object (pass post-effect-pass)))
(defmethod compile-into-pass (object container (pass post-effect-pass)))
(defmethod remove-from-pass (object (pass post-effect-pass)))
(defmethod handle ((event event) (pass post-effect-pass)))

(defmethod render ((pass post-effect-pass) (program shader-program))
  (let ((vao (vertex-array pass)))
    (with-pushed-attribs
      (disable :depth-test)
      (gl:bind-vertex-array (gl-name vao))
      (%gl:draw-elements :triangles (size vao) :unsigned-int (cffi:null-pointer))
      (gl:bind-vertex-array 0))))

(define-class-shader (post-effect-pass :vertex-shader)
  "
layout (location = 0) in vec3 position;
layout (location = 1) in vec2 in_tex_coord;
out vec2 tex_coord;

void main(){
  gl_Position = vec4(position, 1.0f);
  tex_coord = in_tex_coord;
}")

(define-class-shader (post-effect-pass :fragment-shader)
  "
in vec2 tex_coord;")

(define-shader-pass sample-reduction-pass (post-effect-pass)
  ((previous-pass :port-type input :texspec (:target :texture-2d-multisample))
   (color :port-type output :texspec (:target :texture-2d) :reader color)))

(define-class-shader (sample-reduction-pass :fragment-shader)
  "uniform sampler2DMS previous_pass;
in vec2 tex_coord;
out vec4 color;

void main(){
  color = texture(previous_pass, tex_coord);
}")
