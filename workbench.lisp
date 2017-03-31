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

(define-shader-subject testcube (vertex-subject textured-subject)
  ()
  (:default-initargs
   :texture (asset 'workbench 'av)
   :vertex-array (asset 'geometry 'cube)))

(defmethod paint :before ((testcube testcube) target)
  (rotate +vx+ 0.1)
  (rotate +vy+ 0.07)
  (rotate +vz+ 0.03))

(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    (enter (make-instance 'testcube) scene)
    (enter (make-instance 'target-camera :location (vec 0 2 2)) scene)))

(defmethod setup-pipeline ((main main))
  (let ((pipeline (pipeline main))
        (pass1 (make-instance 'per-object-pass))
        (pass2 (make-instance 'test-pass)))
    (connect-pass pass1 pass2 "previousPass" pipeline)))
