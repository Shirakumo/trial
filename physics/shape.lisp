#|
This file is a part of trial
(c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defun make-verlet (&key radius location constraint velocity
                         friction ground-friction
                         pinned mass gravity)
  (make-instance 'verlet :radius (or radius 5)
                         :location (or location (vec3 0 0 0))
                         :constraint constraint
                         :velocity (or velocity (vec3 0 0 0))
                         :friction (or friction 0.91)
                         :ground-friction (or ground-friction 0.66)
                         :mass (or mass 1)
                         :gravity (or gravity (vec3 0 -1 0))
                         :pinned pinned))

(defun make-circle (radius &key location constraint velocity
                                friction ground-friction
                                pinned mass gravity)
  (let ((obj (make-instance 'verlet-entity))
        (verlet (make-verlet :radius radius
                             :location location
                             :constraint constraint
                             :velocity velocity
                             :friction friction
                             :ground-friction ground-friction
                             :mass mass
                             :gravity gravity
                             :pinned pinned)))
    (add-verlet obj verlet)
    obj))

(defun make-box (width &key height
                            (thickness 1) (location (vec3 0 0 0))
                            constraint velocity friction
                            ground-friction stiffness
                            pinned mass gravity)
  (let ((obj (make-instance 'verlet-entity))
        (height (/ (or height width) 2))
        (width (/ width 2)))
    (labels ((new-verlet (location)
               (make-verlet :radius (/ thickness 2)
                            :location location
                            :constraint constraint
                            :velocity velocity
                            :friction friction
                            :ground-friction ground-friction
                            :mass mass
                            :gravity gravity
                            :pinned pinned)))
      (add-verlet obj (new-verlet (v+ location (vec3 (- width) height 0))))
      (add-verlet obj (new-verlet (v+ location (vec3 width height 0))))
      (add-verlet obj (new-verlet (v+ location (vec3 width (- height) 0))))
      (add-verlet obj (new-verlet (v+ location (vec3 (- width) (- height) 0)))))
    (add-edges obj '(0 1  1 2  2 3  3 0  0 2  1 3))
    (when (or stiffness (/= thickness 1))
      (dotimes (i 6)
        (setf (stiffness (edge obj i)) stiffness)
        (setf (width (edge obj i)) thickness)))
    obj))
