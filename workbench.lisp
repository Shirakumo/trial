(in-package #:trial)
(in-readtable :qtools)

(define-pool workbench
  :base 'trial)

(define-asset (workbench av) texture-asset
    (#p"~/av.png"))

(define-asset (workbench cube) packed-vao-asset
    (#(0 1 2 2 3 0
       0 1 5 5 4 0
       2 3 7 7 6 2
       4 5 6 6 7 4)
     3 #(+0.5 +0.5 +0.5
         +0.5 -0.5 +0.5
         -0.5 -0.5 +0.5
         -0.5 +0.5 +0.5
         +0.5 +0.5 -0.5
         +0.5 -0.5 -0.5
         -0.5 -0.5 -0.5
         -0.5 +0.5 -0.5)
     2 #(1.0 1.0
         1.0 0.0
         0.0 0.0
         0.0 1.0
         1.0 1.0
         1.0 0.0
         0.0 0.0
         0.0 1.0)))

(define-shader-pass box-blur-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader box-blur-pass :fragment-shader
  "
in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;
const float blurSizeH = 1.0 / 300.0;
const float blurSizeV = 1.0 / 200.0;

void main(){
  vec4 sum = vec4(0.0);
  for (int x=-4; x<=4; x++){
    for (int y=-4; y<=4; y++){
      sum += texture(previousPass,
                     vec2(texCoord.x + x * blurSizeH, texCoord.y + y * blurSizeV)) / 81.0;
    }
  }
  color = sum;
}")

(define-shader-pass grayscale-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader grayscale-pass :fragment-shader
  "
in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;

void main(){
  color = texture(previousPass, texCoord);
  float avg = 0.2126*color.r + 0.7152*color.g + 0.0722*color.b;
  color = vec4(avg, avg, avg, 1.0);
}")

(define-shader-subject testcube (vertex-subject textured-subject)
  ()
  (:default-initargs
   :texture (asset 'workbench 'av)
   :vertex-array (asset 'workbench 'cube)))

(defmethod setup-scene ((main main))
  (let ((scene (scene main))
        (pipeline (make-instance 'pipeline :name 'pipeline)))
    (enter (make-instance 'testcube) scene)
    (register pipeline scene)
    (load scene)
    (let ((pass1 (make-instance 'per-object-pass))
          (pass2 (make-instance 'grayscale-pass))
          (pass3 (make-instance 'box-blur-pass)))
      (connect-pass pass1 pass2 "previousPass" pipeline)
      (connect-pass pass2 pass3 "previousPass" pipeline))
    (pack-pipeline pipeline main)
    ;; Manual for now
    (dolist (pass (passes pipeline))
      (for:for ((element over scene))
        (register-object-for-pass pass element)))
    (load pipeline)))

(define-handler (controller resize) (ev width height)
  (let ((pipeline (unit 'pipeline (scene *context*))))
    (when pipeline (resize pipeline width height))))

(defmethod paint ((source main) (target main))
  (let ((scene (scene source)))
    (gl:viewport 0 0 (width target) (height target))
    (issue scene 'tick)
    (process scene)
    (reset-matrix (projection-matrix))
    (reset-matrix (view-matrix))
    (perspective-projection 45 (/ (width target) (height target)) 0.1 100)
    (translate-by 0 0 -3 (view-matrix))
    (rotate +vx+ 0.03)
    (rotate +vy+ 0.05)
    (rotate +vz+ 0.07)
    (paint (unit 'pipeline scene) target)))
