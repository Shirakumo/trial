(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)))

(define-pool workbench
  :base 'trial)

(define-asset (workbench grid) mesh
    (make-line-grid 10 100 100))

(define-shader-subject grid (vertex-entity)
  ((vertex-array :initform (asset 'workbench 'grid))))

(define-asset (workbench rectangle) mesh
    (make-rectangle 800 600 :align :bottomleft))

(define-shader-entity filler (vertex-entity colored-entity)
  ((vertex-array :initform (asset 'workbench 'rectangle))
   (color :initform (vec 1 0 0 0.1))))

(defvar *clip-depth* 0)

(defmethod render :before ((workbench workbench) renderable)
  (setf *clip-depth* 0))

(define-shader-subject clipper (vertex-entity scaled-entity located-entity colored-entity)
  ((vertex-array :initform (asset 'workbench 'rectangle))
   (color :initform (vec 1 1 0 1))))

(defmethod paint :around ((clipper clipper) target)
  (gl:stencil-op :keep :incr :incr)
  (gl:stencil-func :lequal *clip-depth* #xFF)
  (gl:color-mask NIL NIL NIL NIL)
  (gl:depth-mask NIL)
  (call-next-method)
  (incf *clip-depth*)
  (gl:stencil-op :keep :keep :keep)
  (gl:stencil-func :lequal *clip-depth* #xFF)
  (gl:color-mask T T T T)
  (gl:depth-mask T))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (disable :cull-face)
    (enter (make-instance 'clipper :scaling (vec 0.5 0.5 1) :location (vec 100 100 0)) scene)
    (enter (make-instance 'filler) scene)
    (enter (make-instance 'clipper :scaling (vec 0.5 0.5 1) :location (vec 0 0 0)) scene)
    (enter (make-instance 'filler) scene)
    ;; (enter (make-instance 'clipper :scaling (vec (/ (expt 1.1 i)) (/ (expt 1.1 i)) 1)) scene)
    ;; (enter (make-instance 'filler) scene)
    (enter (make-instance '2d-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
