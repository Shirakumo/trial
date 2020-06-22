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

(define-shader-entity shader-pass (flow:static-node)
  ((framebuffer :initform NIL :accessor framebuffer))
  (:metaclass shader-pass-class)
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defclass transformed () ())
(defclass renderable () ())

(defgeneric apply-transforms (object)
  (:method-combination progn :most-specific-last))
(defgeneric object-renderable-p (object pass))
(defgeneric compile-to-pass (scene pass))
(defgeneric compile-into-pass (scene container pass))
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

(defun generate-prepare-pass-program (&optional (units (gl:get* :max-texture-image-units)))
  (check-type units (integer 1))
  (let ((*print-case* (readtable-case *readtable*))
        (units (loop for i downfrom (1- units) to 0 collect i)))
    `(lambda (pass program)
       (gl:use-program (gl-name program))
       (loop with texture-index = ',units
             for port in (flow:ports pass)
             do (typecase port
                  (uniform-port
                   (when (texture port)
                     (setf (uniform program (uniform-name port)) (pop texture-index)))))))))

(defun bind-pass-textures (pass)
  (funcall (compile 'bind-pass-textures (generate-bind-pass-textures))
           pass))

(defun prepare-pass-program (pass program)
  (funcall (compile 'prepare-pass-program (generate-prepare-pass-program))
           pass program))

(defmethod blit-to-screen ((pass shader-pass))
  (blit-to-screen (framebuffer pass)))

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
   (group :initform (cons NIL NIL) :accessor group)))

;;; KLUDGE: The protocol that follows is EXTREMELY bad under parallel updates.
;;;         If we ever want to allow such (and it's very possible we do), this will
;;;         need to be revised thoroughly.
(defun push-pass-action (pass action)
  ;; Insert the new action after the end cell and update our end.
  ;; The flare API does not have a clean way of doing this. Sad!
  (setf (cdr (group pass))
        (flare-queue:cell-insert-after
         (flare-queue:make-cell action NIL NIL)
         (cdr (group pass))))
  (incf (slot-value (actions pass) 'flare-queue::size)))

(defun finish-pass-group (pass object)
  ;; TODO: optimisation, contract empty groups.
  ;;       Though this might be bad for future dynamic inserts, so I'm not sure.
  ;;       Would have to reconstruct the context when a previously empty group
  ;;       becomes populated.
  (setf (gethash object (group-pointers pass)) (group pass))
  (setf (group pass) (cons (cdr (group pass)) (cdr (group pass)))))

(defmethod compile-to-pass (object (pass scene-pass))
  (when (object-renderable-p object pass)
    (let ((program (register-object-for-pass pass object)))
      (push-pass-action pass `(prepare-pass-program ,pass ,program))
      (push-pass-action pass `(render ,object ,program)))))

(defmethod compile-to-pass :around ((object transformed) (pass scene-pass))
  (push-pass-action pass `(push-matrix))
  (push-pass-action pass `(apply-transforms ,object))
  (call-next-method)
  (push-pass-action pass `(pop-matrix)))

(defmethod compile-to-pass :after ((object flare:container) (pass scene-pass))
  (for:for ((child over object))
    (compile-to-pass child pass)
    ;; KLUDGE: We can't do this in another method for the OBJECT, as the
    ;;         AROUND for TRANSFORMED must happen before we finish the group.
    (finish-pass-group pass child)))

(defmethod compile-to-pass :around ((scene scene) (pass scene-pass))
  (flare-queue:clear-queue (actions pass))
  (clrhash (group-pointers pass))
  (setf (group pass) (cons (flare-queue:queue-last (actions pass))
                           (flare-queue:queue-last (actions pass))))
  (call-next-method)
  (finish-pass-group pass scene))

(defmethod remove-from-pass ((entity entity) (pass scene-pass))
  (when (gethash entity (group-pointers pass))
    (destructuring-bind (start . end) (gethash entity (group-pointers pass))
      ;; We know START is the cell before our content, and END the last cell of our content.
      (flare-queue:remove-cells start (flare-queue:right end))
      (remhash entity (group-pointers pass)))))

(defmethod compile-into-pass ((entity entity) (container flare:container) (pass scene-pass))
  ;; KLUDGE: We don't actually know /where/ exactly the entity was inserted.
  ;;         Figuring out where would either involve recomputing the entire container
  ;;         which is very costly especially at the scene root, or finding the next
  ;;         neighbour within the container that has a group and inserting before,
  ;;         which is better but still involves a linear search through the container.
  ;;         We opt for guessing that the entity is inserted at the end of the group.
  (destructuring-bind (start . end) (gethash container (group-pointers pass))
    (declare (ignore start))
    ;; KLUDGE: If it is a transformed container the last action is a pop-matrix.
    ;;         We need to insert before that.
    (when (typep container 'transformed)
      (setf end (flare-queue:left end)))
    (setf (group pass) (cons end end))
    (compile-to-pass entity pass)))

(defmethod handle ((ev class-changed) (pass scene-pass))
  (call-next-method)
  ;; FIXME: Need to re-evaluate groups, but this can be difficult.
  ;;        If a class changes to be one that should now be included in the actions
  ;;        somehow, but was not before, it will not have a group that we can update.
  ;;        We'd also not know 'where' to insert the new group, but parent relations
  ;;        are typically not kept. What to do?
  )

(defmethod render ((pass scene-pass) target)
  (flare-queue:do-queue (action (actions pass))
    (apply (car action) (cdr action))))

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
                     (if (allocated-p prev)
                         (with-context (*context*)
                           (setf (buffers prev) (buffers new))
                           (setf (shaders prev) (shaders new)))
                         (setf (gethash class assets) new)))))))
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
    (let* ((old (shader-program pass))
           (new (make-class-shader-program pass)))
      (when (and old (gl-name old))
        (with-context (*context*)
          (dolist (shader (dependencies new))
            (unless (gl-name shader) (load shader)))
          (load new)
          (deallocate old)))
      (setf (shader-program pass) new))))

(defmethod register-object-for-pass ((pass single-shader-pass) o))

(defmethod shader-program-for-pass ((pass single-shader-pass) o)
  (shader-program pass))

(defmethod render ((pass single-shader-pass) thing)
  (render pass (shader-program pass)))

(defmethod render :before ((pass single-shader-pass) (program shader-program))
  (gl:use-program (gl-name program))
  (prepare-pass-program pass program))

(define-shader-pass single-shader-scene-pass (single-shader-pass scene-pass)
  ())

(define-shader-pass post-effect-pass (single-shader-pass)
  ((vertex-array :initform (asset 'trial 'fullscreen-square) :accessor vertex-array)))

(defmethod compile-to-pass (object (pass post-effect-pass)))

(defmethod render ((pass post-effect-pass) thing)
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
   (color :port-type output :texspec (:target :texture-2d))))

(define-class-shader (sample-reduction-pass :fragment-shader)
  "uniform sampler2DMS previous_pass;
in vec2 tex_coord;
out vec4 color;

void main(){
  color = texture(previous_pass, tex_coord);
}")
