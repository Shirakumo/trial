(in-package #:trial)
(in-readtable :qtools)

(define-pool workbench
  :base 'trial)

(define-asset (workbench av) texture-asset
    (#p"~/Documents/img/Gaymes/Touhou/5ac25bb39c05968f7ec82d4a6615f075.jpg"))

(define-shader-subject texcube (vertex-subject textured-subject located-entity rotated-entity)
  ()
  (:default-initargs
   :texture (asset 'workbench 'av)
   :vertex-array (asset 'geometry 'cube)))

(define-shader-subject colcube (vertex-subject colored-subject located-entity rotated-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'geometry 'cube)))

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
          (pass1 (make-instance 'multisampled-per-object-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))
