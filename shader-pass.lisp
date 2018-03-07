#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass shader-pass-class (shader-subject-class flow:static-node-class)
    ()))

(defmethod c2mop:validate-superclass ((class shader-pass-class) (superclass T))
  NIL)

(defmethod c2mop:validate-superclass ((class T) (superclass shader-pass-class))
  NIL)

(defmethod c2mop:validate-superclass ((class shader-pass-class) (superclass standard-class))
  T)

(defclass texture-port (flow:port)
  ((texture :initform NIL :accessor texture)
   (texspec :initarg :texspec :accessor texspec))
  (:default-initargs
   :texspec ()))

(flow:define-port-value-slot texture-port texture texture)

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
                (join-texspec (texspec input)
                              (texspec other)))
      (error "Pipeline is not consistent.~%~
              Pass ~s' input ~s is not texture compatible with output ~s."
             (flow:node input) input other))))

(defclass output (flow:out-port flow:n-port texture-port)
  ((attachment :initarg :attachment :accessor attachment))
  (:default-initargs :attachment :color-attachment0))

(defmethod check-consistent ((output output))
  ())

(defclass buffer (output uniform-port)
  ())

(define-shader-subject shader-pass (flow:static-node)
  ((framebuffer :initform NIL :accessor framebuffer)
   (uniforms :initarg :uniforms :initform () :accessor uniforms))
  (:metaclass shader-pass-class)
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod initialize-instance :after ((pass shader-pass) &key)
  (add-class-redefinition-listener pass (class-of pass)))

(define-class-shader (shader-pass :fragment-shader)
  "#version 330 core")

(defgeneric register-object-for-pass (pass object))
(defgeneric shader-program-for-pass (pass object))
(defgeneric make-pass-shader-program (pass class))
(defgeneric coerce-pass-shader (pass class type spec))

(defmethod make-pass-shader-program ((pass shader-pass) (class symbol))
  (make-pass-shader-program pass (find-class class)))

(defmethod make-pass-shader-program ((pass shader-pass) (class shader-entity-class))
  (let ((shaders ()))
    ;; FIXME: What if the pass defines types that the class does not?
    (loop for (type spec) on (effective-shaders class) by #'cddr
          for inputs = (coerce-pass-shader pass class type spec)
          for shader = (make-instance 'shader :source inputs :type type)
          do (push shader shaders))
    (make-instance 'shader-program :shaders shaders)))

(defmethod finalize :after ((pass shader-pass))
  (when (framebuffer pass)
    (finalize (framebuffer pass))))

(define-handler (shader-pass register-entity-for-enter enter) (ev entity)
  (unless (typep entity 'shader-pass)
    (let ((pass (register-object-for-pass shader-pass entity)))
      (when pass (load pass)))))

(defmacro define-shader-pass (&environment env name direct-superclasses direct-slots &rest options)
  (unless (find-if (lambda (c) (c2mop:subclassp (find-class c T env) 'shader-pass)) direct-superclasses)
    (setf direct-superclasses (append direct-superclasses (list 'shader-pass))))
  (unless (find :metaclass options :key #'car)
    (push '(:metaclass shader-pass-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

(defun generate-prepare-pass-program (&optional (units (gl:get* :max-texture-image-units)))
  (check-type units (integer 1))
  (let ((units (loop for i downfrom (1- units) to 0 collect i)))
    `(lambda (pass program)
       (loop with texture-index = ',units
             with texture-name = ',(loop for unit in units collect
                                         (intern (format NIL "~a~a" :texture unit) "KEYWORD"))
             for port in (flow:ports pass)
             do (when (typep port 'uniform-port)
                  (setf (uniform program (uniform-name port)) (pop texture-index))
                  (gl:active-texture (pop texture-name))
                  (gl:bind-texture :texture-2d (gl-name (texture port)))))
       (loop for (name value) in (uniforms pass)
             do (setf (uniform program name) value)))))

(defun prepare-pass-program (pass program)
  (funcall (compile 'prepare-pass-program (generate-prepare-pass-program))
           pass program))

(define-shader-pass per-object-pass ()
  ((assets :initform (make-hash-table :test 'eql) :accessor assets)))

(defmethod register-object-for-pass :after ((pass per-object-pass) (object shader-entity))
  (add-class-redefinition-listener pass (determine-effective-shader-class object))
  (add-class-redefinition-listener pass (class-of object)))

;; FIXME: Maybe consider determining effective class for each
;;        individual shader stage as they might each change
;;        at different levels and could thus be cached more
;;        effectively.
;; FIXME: Share SHADER assets between shader programs by caching
;;        them... somewhere somehow?
(defmethod notify-class-redefinition ((pass per-object-pass) (class shader-entity-class))
  ;; FIXME: What happens if the effective shader class changes?
  (let ((assets (assets pass)))
    (flet ((refresh (class)
             (let ((prev (gethash class assets))
                   (new (make-pass-shader-program pass class)))
               (if (and prev (allocated-p prev))
                   (with-context ((context (window :main))) ; FUCK
                     (with-simple-restart (continue "Ignore the change and continue with the hold shader.")
                       (allocate new)
                       (deallocate prev)
                       (setf (gethash class assets) new)))
                   (setf (gethash class assets) new)))))
      (cond ((eql class (class-of pass))
             ;; Pass changed, recompile everything
             (loop for class being the hash-keys of assets
                   do (refresh class)))
            ((eql class (determine-effective-shader-class class))
             ;; Object changed, recompile it
             (refresh class))))))

(defmethod shader-program-for-pass ((pass per-object-pass) (subject shader-entity))
  ;; FIXME: FIXME: FIXME: FIXME: THIS REALLY NEEDS A FIX AS IT IS SUPER INEFFICIENT!
  (gethash (determine-effective-shader-class subject) (assets pass)))

(defmethod coerce-pass-shader ((pass per-object-pass) class type spec)
  (glsl-toolkit:merge-shader-sources
   (list spec (getf (effective-shaders pass) type))))

(defmethod register-object-for-pass ((pass per-object-pass) o))

(defmethod register-object-for-pass ((pass per-object-pass) (container container))
  (for:for ((object over container))
    (register-object-for-pass pass object)))

(defmethod register-object-for-pass ((pass per-object-pass) (class shader-entity-class))
  (let ((effective-class (determine-effective-shader-class class)))
    (unless (gethash effective-class (assets pass))
      (setf (gethash effective-class (assets pass))
            (make-pass-shader-program pass effective-class)))))

(defmethod register-object-for-pass ((pass per-object-pass) (subject shader-entity))
  (register-object-for-pass pass (class-of subject)))

(defmethod paint :around ((subject shader-entity) (pass per-object-pass))
  (let ((program (shader-program-for-pass pass subject)))
    (gl:use-program (gl-name program))
    (prepare-pass-program pass program)
    (call-next-method)))

(define-shader-pass single-shader-pass (bakable)
  ((shader-program :initform NIL :accessor shader-program)))

(defmethod notify-class-redefinition ((pass single-shader-pass) class)
  (when (eql class (class-of pass))
    (let* ((program (shader-program pass))
           (loaded (and program (gl-name program))))
      (when loaded (deallocate program))
      (setf (shader-program pass) (make-class-shader-program pass))
      (when loaded
        (with-context ((context (window :main))) ; FIXME: FUCK
          (load (shader-program pass)))))))

(defmethod bake ((pass single-shader-pass))
  (setf (shader-program pass) (make-class-shader-program pass)))

(defmethod register-object-for-pass ((pass single-shader-pass) o))

(defmethod shader-program-for-pass ((pass single-shader-pass) o)
  (shader-program pass))

(defmethod paint-with :around ((pass single-shader-pass) thing)
  (let ((program (shader-program pass)))
    (gl:use-program (gl-name program))
    (prepare-pass-program pass program)
    (call-next-method)))

(define-shader-pass post-effect-pass (single-shader-pass)
  ((vertex-array :initform (asset 'trial 'fullscreen-square) :accessor vertex-array)))

(defmethod paint-with ((pass post-effect-pass) thing)
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
