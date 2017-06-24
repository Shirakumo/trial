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

(defclass input (flow:in-port flow:1-port)
  ((uniform-name :initarg :uniform :initform NIL :accessor uniform-name)
   (texture :initform NIL :accessor texture)))

(defmethod initialize-instance :after ((input input) &key)
  (unless (uniform-name input)
    (setf (uniform-name input) (symbol->c-name (flow:name input)))))

(defclass output (flow:out-port flow:n-port)
  ((attachment :initarg :attachment :accessor attachment)
   (texture :initform NIL :accessor texture))
  (:default-initargs :attachment :color-attachment0))

(define-shader-subject shader-pass (flow:static-node)
  ((framebuffer :initform NIL :accessor framebuffer)
   (uniforms :initarg :uniforms :initform () :accessor uniforms))
  (:metaclass shader-pass-class))

(defgeneric register-object-for-pass (pass object))
(defgeneric shader-program-for-pass (pass object))

(defmethod finalize :after ((pass shader-pass))
  (when (framebuffer pass)
    (finalize (framebuffer pass))))

(defmethod paint ((pass shader-pass) target)
  (when (typep target 'main)
    (paint (scene target) pass)))

(define-handler (shader-pass register-subject-for-enter enter) (ev entity)
  (let ((pass (register-object-for-pass shader-pass entity)))
    (when pass (load pass))))

(defmacro define-shader-pass (name direct-superclasses direct-slots &rest options)
  (unless (find-if (lambda (c) (c2mop:subclassp (find-class c T env) 'shader-pass)) direct-superclasses)
    (setf direct-superclasses (append direct-superclasses (list 'shader-pass))))
  (unless (find :metaclass options :key #'car)
    (push '(:metaclass shader-pass-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

(defun prepare-pass-program (pass program)
  ;; FIXME: Query for max number of textures available and build
  ;;        this dynamically based on that number.
  (loop with texture-index = '(15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)
        with texture-name = '(:texture15 :texture14 :texture13 :texture12
                              :texture11 :texture10 :texture9  :texture8
                              :texture7  :texture6  :texture5  :texture4
                              :texture3  :texture2  :texture1  :texture0)
        for port in (flow:ports pass)
        do (when (typep port 'input)
             (setf (uniform program (uniform-name port)) (pop texture-index))
             (gl:active-texture (pop texture-name))
             (gl:bind-texture :texture-2d (resource (texture port)))))
  (loop for (name value) in (uniforms pass)
        do (setf (uniform program name) value)))

(define-shader-pass per-object-pass ()
  ((assets :initform (make-hash-table :test 'eql) :accessor assets)))

(define-handler (per-object-pass update-shader-for-redefined-subject subject-class-redefined) (ev subject-class)
  (let ((assets (assets per-object-pass)))
    (flet ((refresh (class)
             (let ((loaded (and (gethash class assets) (resource (gethash class assets)))))
               (when loaded (offload (gethash class assets)))
               (remhash class assets)
               (register-object-for-pass per-object-pass class)
               (when loaded (load (gethash class assets))))))
      (cond ((eql subject-class (class-of per-object-pass))
             ;; Pass changed, recompile everything
             (loop for class being the hash-keys of assets
                   do (refresh class)))
            ((and (typep subject-class 'shader-subject-class)
                  (not (typep subject-class 'shader-pass-class)))
             ;; Object changed, recompile it
             (refresh subject-class))))))

(defmethod load progn ((pass per-object-pass))
  (loop for v being the hash-values of (assets pass)
        do (load v)))

(defmethod shader-program-for-pass ((pass per-object-pass) (subject shader-subject))
  (gethash (class-of subject) (assets pass)))

(defmethod coerce-pass-shader ((pass per-object-pass) type spec)
  (glsl-toolkit:merge-shader-sources
   (list spec (class-shader type pass))))

(defmethod register-object-for-pass ((pass per-object-pass) o))

(defmethod register-object-for-pass ((pass per-object-pass) (class shader-subject-class))
  (let ((shaders ()))
    (unless (gethash class (assets pass))
      (loop for (type spec) on (effective-shaders class) by #'cddr
            for inputs = (coerce-pass-shader pass type spec)
            for shader = (make-asset 'shader inputs :type type)
            do (push shader shaders))
      (setf (gethash class (assets pass))
            (make-asset 'shader-program shaders)))))

(defmethod register-object-for-pass ((pass per-object-pass) (subject shader-subject))
  (register-object-for-pass pass (class-of subject)))

(defmethod paint :around ((subject shader-subject) (pass per-object-pass))
  (let ((program (shader-program-for-pass pass subject)))
    (gl:use-program (resource program))
    (prepare-pass-program pass program)
    (call-next-method)))

(define-shader-pass multisampled-pass ()
  ((multisample-fbo :initform NIL :accessor multisample-fbo)
   (samples :initarg :samples :accessor samples))
  (:default-initargs :samples 8))

(defmethod load progn ((pass multisampled-pass))
  (let ((fbo (make-asset 'framebuffer-bundle
                         `((:attachment :color-attachment0 :bits ,(samples pass) :target :texture-2d-multisample)
                           (:attachment :depth-attachment  :bits ,(samples pass) :target :texture-2d-multisample))
                         :width (width *context*) :height (height *context*))))
    (load fbo)
    (setf (multisample-fbo pass) fbo)))

(defmethod paint :around ((pass multisampled-pass) target)
  (let ((original-framebuffer (resource (framebuffer pass))))
    (gl:bind-framebuffer :framebuffer (resource (multisample-fbo pass)))
    (gl:clear :color-buffer :depth-buffer)
    (call-next-method)
    (gl:bind-framebuffer :draw-framebuffer original-framebuffer)
    (%gl:blit-framebuffer 0 0 (width target) (height target) 0 0 (width target) (height target)
                          (logior (cffi:foreign-bitfield-value '%gl::ClearBufferMask :color-buffer)
                                  (cffi:foreign-bitfield-value '%gl::ClearBufferMask :depth-buffer))
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
  (setf (shader-program pass) (load (make-class-shader-program pass))))

(defmethod register-object-for-pass ((pass single-shader-pass) o))

(defmethod shader-program-for-pass ((pass single-shader-pass) o)
  (shader-program pass))

(defmethod paint :around ((source scene) (pass single-shader-pass))
  (let ((program (shader-program pass)))
    (gl:use-program (resource program))
    (prepare-pass-program pass program)
    (call-next-method)))

(define-shader-pass post-effect-pass (single-shader-pass)
  ((vertex-array :initform (asset 'trial 'fullscreen-square) :accessor vertex-array)))

(defmethod load progn ((pass post-effect-pass))
  (load (vertex-array pass)))

(defmethod paint ((source scene) (pass post-effect-pass))
  (let ((vao (vertex-array pass)))
    (with-pushed-attribs
      (disable :depth-test)
      (gl:bind-vertex-array (resource vao))
      (%gl:draw-elements :triangles (size vao) :unsigned-int 0)
      (gl:bind-vertex-array 0))))

(define-class-shader post-effect-pass :vertex-shader
  "
layout (location = 0) in vec3 position;
layout (location = 1) in vec2 in_tex_coord;
out vec2 tex_coord;

void main(){
  gl_Position = vec4(position, 1.0f);
  tex_coord = in_tex_coord;
}")

(define-class-shader post-effect-pass :fragment-shader
  "
in vec2 tex_coord;")
