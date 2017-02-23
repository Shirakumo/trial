(in-package #:trial)
(in-readtable :qtools)

(define-asset shader primitive-vert (trial)
  :file "primitive.vert")

(define-asset shader primitive-frag (trial)
  :file "primitive.frag")

(define-asset shader-program primitive (trial)
  :shaders '((trial primitive-vert)
             (trial primitive-frag)))

(defmethod paint ((source main) (target main))
  (gl:viewport 0 0 (width target) (height target))
  (issue (scene target) 'tick)
  (process (scene target))
  (let* ((triangle-buffer (make-asset 'vertex-buffer-asset '((-0.5 -0.5  0.0
                                                              0.5 -0.5  0.0
                                                              0.0  0.5  0.0))))
         (triangle-array (make-asset 'vertex-array-asset `((,triangle-buffer :size 3))))
         (vertex-shader (make-asset 'shader-asset '("
#version 330 core
  
layout (location = 0) in vec3 position;

void main(){
  gl_Position = vec4(position, 1.0f);
}") :type :vertex-shader))
         (fragment-shader (make-asset 'shader-asset '("
#version 330 core

out vec4 color;

void main(){
  color = vec4(1.0, 0.2, 0.2, 1.0);
}") :type :fragment-shader))
         (shader-program (make-asset 'shader-program-asset (list vertex-shader fragment-shader))))
    (load-asset triangle-buffer)
    (load-asset triangle-array)
    (load-asset vertex-shader)
    (load-asset fragment-shader)
    (load-asset shader-program)
    (gl:use-program (resource shader-program))
    (gl:bind-vertex-array (resource triangle-array))
    (gl:draw-arrays :triangles 0 3)
    (gl:bind-vertex-array 0)
    (gl:use-program 0)
    (offload-asset triangle-buffer)
    (offload-asset triangle-array)
    (offload-asset vertex-shader)
    (offload-asset fragment-shader)
    (offload-asset shader-program)))
