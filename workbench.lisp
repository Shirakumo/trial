(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)))

(define-pool workbench
  :base 'trial)

(define-asset (workbench grid) mesh
  (make-line-grid 10 100 100))

(define-asset (workbench chassis) mesh
  (make-cube (list 10 10 30) :y 5))

(define-asset (workbench tires) mesh
  (combine-shapes
    (make-cylinder 2 1 :x -5 :z -15)
    (make-cylinder 2 1 :x +5 :z -15)
    (make-cylinder 2 1 :x -5 :z +15)
    (make-cylinder 2 1 :x +5 :z +15)))

(defclass vehicle (listener located-entity axis-rotated-entity container-unit)
  ())

(defmethod initialize-instance :after ((vehicle vehicle) &key)
  (enter (make-instance 'vertex-entity :vertex-array (asset 'workbench 'chassis)) vehicle)
  (enter (make-instance 'vertex-entity :vertex-array (asset 'workbench 'tires)) vehicle))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'vertex-entity :vertex-array (asset 'workbench 'grid)) scene)
    (enter (make-instance 'vehicle) scene)
    (enter (make-instance 'vehicle :location (vec 50 0 0)) scene)
    (enter (make-instance 'editor-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
