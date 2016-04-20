#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass geometry (entity)
  ())

(defclass bounded-geometry (geometry)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height))
  (:default-initargs
   :width 10
   :height 10))

(defclass sized-geometry (geometry)
  ((size :initarg :size :accessor size))
  (:default-initargs
   :size 10))

(defclass segmented-geometry (geometry)
  ((segments :initarg :segments :accessor segments))
  (:default-initargs
   :segments 8))

(defclass triangle (bounded-geometry)
  ())

(defmethod paint ((triangle triangle) target)
  (let ((w (/ (width triangle) 2.0))
        (h (/ (height triangle) 2.0)))
    (with-primitives :triangles
      (gl:vertex (- w) (- h))
      (gl:vertex    w  (- h))
      (gl:vertex    w     h))))

(defclass square (sized-geometry)
  ())

(defmethod paint ((square square) target)
  (let ((s (/ (size square) 2.0)))
    (with-primitives :quads
      (gl:vertex (- s) (- s))
      (gl:vertex    s  (- s))
      (gl:vertex    s     s)
      (gl:vertex (- s)    s))))

(defclass rectangle (bounded-geometry)
  ())

(defmethod paint ((rectangle rectangle) target)
  (let ((w (/ (width rectangle) 2.0))
        (h (/ (height rectangle) 2.0)))
    (with-primitives :quads
      (gl:vertex (- w) (- h))
      (gl:vertex    w  (- h))
      (gl:vertex    w     h)
      (gl:vertex (- w)    h))))

(defclass sphere (sized-geometry segmented-geometry)
  ())

(defmethod paint ((sphere sphere) target)
  (let ((lat (segments sphere))
        (lng (segments sphere))
        (size (size sphere)))
    (loop for i from 0 below lat
          for lat0 = (* PI (+ -0.5 (/ (1- i) lat)))
          for lat1 = (* PI (+ -0.5 (/ i lat)))
          for z0 = (sin lat0)
          for zr0 = (cos lat0)
          for z1 = (sin lat1)
          for zr1 = (cos lat1)
          do (with-primitives :quad-strip
               (loop for j from 0 to lng
                     for l = (* 2 PI (/ (1- j) lng))
                     for x = (cos l)
                     for y = (sin l)
                     do (gl:normal (* x zr0 size) (* y zr0 size) z0)
                        (gl:vertex (* x zr0 size) (* y zr0 size) z0)
                        (gl:normal (* x zr1 size) (* y zr1 size) z1)
                        (gl:vertex (* x zr1 size) (* y zr1 size) z1))))))

(defclass cube (sized-geometry)
  ())

(defmethod paint ((cube cube) target)
  (let ((s (/ (size cube) 2.0)))
    (with-primitives :quads
      (gl:vertex    s     s (- s))
      (gl:vertex (- s)    s (- s))
      (gl:vertex (- s)    s     s)
      (gl:vertex    s     s     s)
      (gl:vertex    s  (- s)    s)
      (gl:vertex (- s) (- s)    s)
      (gl:vertex (- s) (- s) (- s))
      (gl:vertex    s  (- s) (- s))
      (gl:vertex    s     s     s)
      (gl:vertex (- s)    s     s)
      (gl:vertex (- s) (- s)    s)
      (gl:vertex  s    (- s)    s)
      (gl:vertex  s    (- s) (- s))
      (gl:vertex (- s) (- s) (- s))
      (gl:vertex (- s)    s  (- s))
      (gl:vertex    s     s  (- s))
      (gl:vertex (- s)    s     s)
      (gl:vertex (- s)    s  (- s))
      (gl:vertex (- s) (- s) (- s))
      (gl:vertex (- s) (- s)    s)
      (gl:vertex    s     s  (- s))
      (gl:vertex    s     s     s)
      (gl:vertex    s  (- s)    s)
      (gl:vertex    s  (- s) (- s)))))

(defclass cylinder (bounded-geometry segmented-geometry)
  ())

(defmethod paint ((cylinder cylinder) target)
  (let ((w (width cylinder))
        (h (/ (height cylinder) 2.0))
        (s (segments cylinder)))
    (with-primitives :triangle-fan
      (gl:vertex 0 (- h) 0)
      (loop for i from 0 to (* 2 PI) by (/ (* 2 PI) s)
            do (gl:vertex (* w (cos i)) (- h) (* w (sin i)))))
    (with-primitives :triangle-fan
      (gl:vertex 0 h 0)
      (loop for i from (* 2 PI) downto 0 by (/ (* 2 PI) s)
            do (gl:vertex (* w (cos i)) h (* w (sin i)))))
    (with-primitives :quad-strip
      (loop for i from 0 below s
            for theta = (/ (* 2 PI i) s)
            do (gl:vertex (* w (cos theta)) (- h) (* w (sin theta)))
               (gl:vertex (* w (cos theta))    h  (* w (sin theta)))))))
