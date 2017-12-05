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
  ((texture :initform NIL :accessor texture)))

(flow:define-port-value-slot texture-port texture texture)

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
           (flow:node input) input)))

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

(define-class-shader (shader-pass :fragment-shader)
  "#version 330 core")

;; FIXME: check for duplicate inputs/outputs.

(defgeneric register-object-for-pass (pass object))
(defgeneric shader-program-for-pass (pass object))
(defgeneric coerce-pass-shader (pass class type spec))
(defgeneric determine-effective-shader-class (class))

(defmethod finalize :after ((pass shader-pass))
  (when (framebuffer pass)
    (finalize (framebuffer pass))))

(define-handler (shader-pass register-subject-for-enter enter) (ev entity)
  (let ((pass (register-object-for-pass shader-pass entity)))
    (when pass (load pass))))

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
                  (gl:bind-texture :texture-2d (resource (texture port)))))
       (loop for (name value) in (uniforms pass)
             do (setf (uniform program name) value)))))

(defun prepare-pass-program (pass program)
  (funcall (compile 'prepare-pass-program (generate-prepare-pass-program))
           pass program))

(define-shader-pass per-object-pass ()
  ((assets :initform (make-hash-table :test 'eql) :accessor assets)))

(define-handler (per-object-pass update-shader-for-redefined-subject subject-class-redefined) (ev subject-class)
  (let ((assets (assets per-object-pass)))
    (flet ((refresh (class)
             (let ((previous (gethash class assets)))
               (remhash class assets)
               (register-object-for-pass per-object-pass class)
               (when (and previous (resource previous))
                 (restart-case
                     (progn
                       (load (gethash class assets))
                       (offload previous))
                   (continue ()
                     :report "Ignore the change and continue with the hold shader."
                     (setf (gethash class assets) previous)))))))
      (cond ((eql subject-class (class-of per-object-pass))
             ;; Pass changed, recompile everything
             (loop for class being the hash-keys of assets
                   do (refresh class)))
            ((and (typep subject-class 'shader-entity-class)
                  (not (typep subject-class 'shader-pass-class)))
             ;; Object changed, recompile it
             (refresh subject-class))))))

(defmethod shader-program-for-pass ((pass per-object-pass) (subject shader-entity))
  (gethash (class-of subject) (assets pass)))

(defmethod coerce-pass-shader ((pass per-object-pass) class type spec)
  (glsl-toolkit:merge-shader-sources
   (list spec (getf (effective-shaders pass) type))))

(defmethod determine-effective-shader-class ((name symbol))
  (determine-effective-shader-class (find-class name)))

(defmethod determine-effective-shader-class ((object shader-entity))
  (determine-effective-shader-class (class-of object)))

(defmethod determine-effective-shader-class ((class standard-class))
  NIL)

;; FIXME: Maybe consider determining effective class for each
;;        individual shader stage as they might each change
;;        at different levels and could thus be cached more
;;        effectively.
;; FIXME: Share SHADER assets between shader programs by caching
;;        them... somewhere somehow?
(defmethod determine-effective-shader-class ((class shader-entity-class))
  (if (direct-shaders class)
      class
      (let* ((effective-superclasses (list (find-class 'shader-entity))))
        ;; Loop through superclasses and push new, effective superclasses.
        (loop for superclass in (c2mop:class-direct-superclasses class)
              for effective-class = (determine-effective-shader-class superclass)
              do (when (and effective-class (not (find effective-class effective-superclasses)))
                   (push effective-class effective-superclasses)))
        ;; If we have one or two --one always being the shader-entity class--
        ;; then we just return the more specific of the two, as there's no class
        ;; combination happening that would produce new shaders.
        (if (<= (length effective-superclasses) 2)
            (first effective-superclasses)
            class))))

(defmethod register-object-for-pass ((pass per-object-pass) o))

(defmethod register-object-for-pass ((pass per-object-pass) (container container))
  (for:for ((object over container))
    (register-object-for-pass pass object)))

(defmethod register-object-for-pass ((pass per-object-pass) (class shader-entity-class))
  (let ((shaders ()))
    (let ((effective-class (determine-effective-shader-class class)))
      (unless (gethash effective-class (assets pass))
        (loop for (type spec) on (effective-shaders effective-class) by #'cddr
              for inputs = (coerce-pass-shader pass effective-class type spec)
              for shader = (make-asset 'shader inputs :type type)
              do (push shader shaders))
        (setf (gethash effective-class (assets pass))
              (make-asset 'shader-program shaders)))
      (setf (gethash class (assets pass)) (gethash effective-class (assets pass))))))

(defmethod register-object-for-pass ((pass per-object-pass) (subject shader-entity))
  (register-object-for-pass pass (class-of subject)))

(defmethod paint :around ((subject shader-entity) (pass per-object-pass))
  (let ((program (shader-program-for-pass pass subject)))
    (gl:use-program (resource program))
    (prepare-pass-program pass program)
    (call-next-method)))

(define-shader-pass multisampled-pass (bakable)
  ((multisample-fbo :initform NIL :accessor multisample-fbo)
   (samples :initarg :samples :accessor samples))
  (:default-initargs :samples 8))

(defmethod bake ((pass multisampled-pass))
  (setf (multisample-fbo pass)
        (make-asset 'framebuffer-bundle
                    `((:attachment :color-attachment0 :bits ,(samples pass) :target :texture-2d-multisample)
                      (:attachment :depth-stencil-attachment :bits ,(samples pass) :target :texture-2d-multisample))
                    :width (width *context*) :height (height *context*))))

(defmethod paint-with :around ((pass multisampled-pass) target)
  (let ((original-framebuffer (resource (framebuffer pass))))
    (gl:bind-framebuffer :framebuffer (resource (multisample-fbo pass)))
    (gl:clear :color-buffer :depth-buffer :stencil-buffer)
    (call-next-method)
    (gl:bind-framebuffer :draw-framebuffer original-framebuffer)
    (%gl:blit-framebuffer 0 0 (width target) (height target) 0 0 (width target) (height target)
                          (logior (cffi:foreign-bitfield-value '%gl::ClearBufferMask :color-buffer)
                                  (cffi:foreign-bitfield-value '%gl::ClearBufferMask :depth-buffer)
                                  (cffi:foreign-bitfield-value '%gl::ClearBufferMask :stencil-buffer))
                          (cffi:foreign-enum-value '%gl:enum :nearest))
    (gl:bind-framebuffer :framebuffer original-framebuffer)))

(define-handler (multisampled-pass resize) (ev width height)
  (when (multisample-fbo multisampled-pass)
    (resize (multisample-fbo multisampled-pass) width height)))

(define-shader-pass multisampled-per-object-pass (multisampled-pass per-object-pass)
  ())

(define-shader-pass single-shader-pass ()
  ((shader-program :initform (make-instance 'shader-program) :accessor shader-program)))

(define-handler (single-shader-pass update-shader-for-redefined-subject subject-class-redefined) (ev subject-class)
  (when (eql subject-class (class-of single-shader-pass))
    (let* ((program (shader-program single-shader-pass))
           (loaded (and program (resource program))))
      (when loaded (offload program))
      (setf (shader-program single-shader-pass) (make-class-shader-program single-shader-pass))
      (when loaded (load (shader-program single-shader-pass))))))

(defmethod load progn ((pass single-shader-pass))
  (setf (shader-program pass) (make-class-shader-program pass)))

(defmethod register-object-for-pass ((pass single-shader-pass) o))

(defmethod shader-program-for-pass ((pass single-shader-pass) o)
  (shader-program pass))

(defmethod paint-with :around ((pass single-shader-pass) thing)
  (let ((program (shader-program pass)))
    (gl:use-program (resource program))
    (prepare-pass-program pass program)
    (call-next-method)))

(define-shader-pass post-effect-pass (single-shader-pass)
  ((vertex-array :initform (asset 'trial 'fullscreen-square) :accessor vertex-array)))

(defmethod paint-with ((pass post-effect-pass) thing)
  (let ((vao (vertex-array pass)))
    (with-pushed-attribs
      (disable :depth-test)
      (gl:bind-vertex-array (resource vao))
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
