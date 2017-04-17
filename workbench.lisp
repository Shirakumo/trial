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
")

(define-shader-subject colcube (vertex-subject colored-subject located-entity rotated-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'geometry 'cube)))

(define-handler (colcube tick) (ev)
  )

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
          (pass3 (make-instance 'fxaa-pass)))
      (connect-pass pass1 pass2 "previousPass" pipeline)
      (connect-pass pass2 pass3 "previousPass" pipeline)))

  (maybe-reload-scene))
