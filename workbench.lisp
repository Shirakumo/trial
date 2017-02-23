(in-package #:trial)
(in-readtable :qtools)

(defmethod paint ((source main) (target main))
  (gl:viewport 0 0 (width target) (height target))
  (issue (scene target) 'tick)
  (process (scene target))
  (with-assets ((vertex-buffer 'vertex-buffer-asset '(+0.5  0.5 0.0  1.0 1.0
                                                      +0.5 -0.5 0.0  1.0 0.0
                                                      -0.5 -0.5 0.0  0.0 0.0
                                                      -0.5  0.5 0.0  0.0 1.0))
                (element-buffer 'vertex-buffer-asset '(3 1 0 3 2 1) :type :element-array-buffer :element-type :uint)
                (triangle-array 'vertex-array-asset `(((,vertex-buffer ,element-buffer) :size 3 :stride 20 :offset  0)
                                                      ((,vertex-buffer ,element-buffer) :size 2 :stride 20 :offset 12)))
                (cat-texture 'texture-asset '(#p"/home/linus/av.png"))
                (vertex-shader 'shader-asset '("
#version 330 core
  
layout (location = 0) in vec3 position;
layout (location = 1) in vec2 dtexcoord;

out vec2 texcoord;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  gl_Position = projection_matrix * view_matrix * model_matrix * vec4(position, 1.0f);
  texcoord = dtexcoord;
}") :type :vertex-shader)
                (fragment-shader 'shader-asset '("
#version 330 core

in vec2 texcoord;
out vec4 color;
uniform sampler2D teximage;

void main(){
  color = texture(teximage, texcoord);
}") :type :fragment-shader)
                (shader-program 'shader-program-asset (list vertex-shader fragment-shader)))
    (gl:use-program (resource shader-program))
    (gl:bind-vertex-array (resource triangle-array))
    (with-pushed-matrix
      (translate-by 0.5 0.5 0)
      (rotate +vz+ 15)
      (setf (uniform shader-program "model_matrix") (model-matrix))
      (setf (uniform shader-program "view_matrix") (view-matrix))
      (setf (uniform shader-program "projection_matrix") (projection-matrix))
      (%gl:draw-elements :triangles 6 :unsigned-int 0))
    (setf (uniform shader-program "model_matrix") (model-matrix))
    (setf (uniform shader-program "view_matrix") (view-matrix))
    (setf (uniform shader-program "projection_matrix") (projection-matrix))
    (%gl:draw-elements :triangles 6 :unsigned-int 0)
    (gl:bind-vertex-array 0)
    (gl:use-program 0)))
