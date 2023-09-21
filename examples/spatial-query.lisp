(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity ubox (vertex-entity colored-entity transformed-entity)
  ((color :initform (vec 1 0 0 1))
   (vertex-array :initform (// 'trial 'unit-cube))))

(defmethod spaces:bsize ((box ubox))
  (scaling box))

(defmethod spaces:ensure-region ((object ubox) &optional region)
  (unless region
    (setf region (spaces:region 0 0 0 0 0 0)))
  (v<- region (location object))
  (nv- region (scaling object))
  (v<- (spaces:region-size region) (scaling object))
  (nv* (spaces:region-size region) 2)
  region)

(defclass spatial-structure (trial:array-container listener)
  ((q :initarg :q  :accessor q)
   (f :initarg :f :accessor f)))

(defmethod enter :after ((box ubox) (structure spatial-structure))
  (spaces:enter box (q structure)))

(defmethod leave :after ((box ubox) (structure spatial-structure))
  (spaces:leave box (q structure)))

(defmethod clear :after ((structure spatial-structure))
  (spaces:clear (q structure)))

(defmethod start-frame ((structure spatial-structure))
  (spaces:do-all (e (q structure))
    (vsetf (color e) 1 0 0 1))
  (spaces:do-overlapping (e (q structure) (f structure))
    (vsetf (color e) 0 1 0 1))
  (spaces:do-contained (e (q structure) (f structure))
    (vsetf (color e) 1 1 0 1)))

(define-handler (spatial-structure tick) ()
  (start-frame spatial-structure))

(define-example spatial-query
  (let ((structure (make-instance 'spatial-structure
                                  :q (ecase :kd
                                       (:kd (org.shirakumo.fraf.trial.space.kd-tree:make-kd-tree))
                                       (:grid (org.shirakumo.fraf.trial.space.grid3:make-grid 0.01 :bsize (vec3 1))))
                                  :f (make-instance 'ubox :scaling (vec3 0.5)))))
    (enter structure scene)
    (loop for i from 0 below 1000
          for box = (make-instance 'ubox :location (vrand (vec3 0) (vec3 2)) :scaling (vec3 0.01))
          do (enter box structure)))
  (enter (make-instance '3d-camera :location (vec 0 0 -2.5)) scene)
  (enter (make-instance 'render-pass) scene))
