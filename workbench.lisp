(in-package #:trial)
(in-readtable :qtools)

(define-pool workbench
  :base 'trial)

(define-asset (workbench av) texture-asset
    (#p"~/Documents/img/Gaymes/Touhou/5ac25bb39c05968f7ec82d4a6615f075.jpg"))

(define-shader-pass test-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader test-pass :fragment-shader
  "
in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;

void main(){
  color = texture(previousPass, texCoord);
}")

(define-shader-subject texcube (vertex-subject textured-subject located-entity rotated-entity)
  ()
  (:default-initargs
   :texture (asset 'workbench 'av)
   :vertex-array (asset 'geometry 'cube)))

(define-class-shader texcube :fragment-shader
  "
in vec2 texcoord;
out vec4 color;
uniform sampler2D texture_image;

void main(){
  vec2 texSize = textureSize(texture_image, 0);
  vec2 center = texSize/2;
  vec2 tc = texcoord * texSize;
  float radius = 1000;
  float angle = 0.8;
  tc -= center;
  float dist = length(tc);
  if (dist < radius) {
    float percent = (radius - dist) / radius;
    float theta = percent * percent * angle * 8.0;
    float s = sin(theta);
    float c = cos(theta);
    tc = vec2(dot(tc, vec2(c, -s)), dot(tc, vec2(s, c)));
  }
  tc += center;
  color = texture2D(texture_image, tc / texSize);
}")

(define-shader-subject colcube (vertex-subject colored-subject located-entity rotated-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'geometry 'cube)))

(define-handler (colcube tick) (ev)
  (incf (vx (rotation colcube)) 0.1)
  (incf (vy (rotation colcube)) 0.2))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (dotimes (i 100)
        (enter (make-instance 'texcube :location (vec3-random -10 10) :rotation (vec3-random 0 360)) scene))
      (enter (make-instance 'colcube :location (vec 1 0.5 0) :color (vec 0.8 0 0 1)) scene)
      (enter (make-instance 'target-camera :location (vec 0 -3 2)) scene)))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'per-object-pass))
          (pass2 (make-instance 'test-pass))
          (pass3 (make-instance 'gaussian-blur-pass :uniforms '(("blurRadius" 2.0))))
          (pass4 (make-instance 'gaussian-blur-pass :uniforms `(("blurRadius" 2.0) ("dir" ,(vec2 0.0 1.0))))))
      (connect-pass pass1 pass2 "previousPass" pipeline)
      (connect-pass pass2 pass3 "previousPass" pipeline)
      (connect-pass pass3 pass4 "previousPass" pipeline)))

  (maybe-reload-scene))
