#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-pool geometry
  :base 'trial)

(define-asset (geometry fullscreen-square) packed-vao-asset
    (#(0 1 2 2 3 0)
     3 #(+1.0 +1.0 +0.0
         +1.0 -1.0 +0.0
         -1.0 -1.0 +0.0
         -1.0 +1.0 +0.0)
     2 #(1.0 1.0
         1.0 0.0
         0.0 0.0
         0.0 1.0)))

;; Factor out into function that generates this
;; based on requested size, whether it should be
;; textured or shaded, etc.
(define-asset (geometry cube) packed-vao-asset
    (#( 0  1  2  2  3  0
       4  5  6  6  7  4
       8  9 10 10 11  8
       12 13 14 14 15 12
       16 17 18 18 19 16
       20 21 22 22 23 20)
     3 #(+0.5 +0.5 -0.5
         -0.5 +0.5 -0.5
         -0.5 +0.5 +0.5
         +0.5 +0.5 +0.5
         
         +0.5 -0.5 +0.5
         -0.5 -0.5 +0.5
         -0.5 -0.5 -0.5
         +0.5 -0.5 -0.5

         +0.5 +0.5 +0.5
         -0.5 +0.5 +0.5
         -0.5 -0.5 +0.5
         +0.5 -0.5 +0.5

         +0.5 -0.5 -0.5
         -0.5 -0.5 -0.5
         -0.5 +0.5 -0.5
         +0.5 +0.5 -0.5

         -0.5 +0.5 +0.5
         -0.5 +0.5 -0.5
         -0.5 -0.5 -0.5
         -0.5 -0.5 +0.5

         +0.5 +0.5 -0.5
         +0.5 +0.5 +0.5
         +0.5 -0.5 +0.5
         +0.5 -0.5 -0.5)
     2 #(1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0)))

(defclass geometry (entity)
  ())

(defclass bounded-geometry (geometry)
  ((width :initarg :width :initform 10 :accessor width)
   (height :initarg :height :initform 10 :accessor height)))

(define-saved-slots bounded-geometry width height)

(defclass sized-geometry (geometry)
  ((size :initarg :size :initform 10 :accessor size)))

(define-saved-slots sized-geometry size)

(defclass segmented-geometry (geometry)
  ((segments :initarg :segments :initform 8 :accessor segments)))

(define-saved-slots segmented-geometry segments)

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

(defclass disc (sized-geometry segmented-geometry)
  ())

(defmethod paint ((disc disc) target)
  (let ((s (size disc))
        (segs (segments disc)))
    (with-primitives :triangle-fan
      (gl:vertex 0 0)
      (loop for i from 0 to (* 2 PI) by (/ (* 2 PI) segs)
            do (gl:vertex (* s (cos i)) (* s (sin i)))))))

(defclass sphere (sized-geometry segmented-geometry)
  ())

(defmethod paint ((sphere sphere) target)
  (let ((lat (segments sphere))
        (lng (segments sphere))
        (size (size sphere)))
    (loop for i from lat downto 1
          for lat0 = (* PI (- (/ (1- i) lat) 0.5))
          for lat1 = (* PI (- (/ i lat) 0.5))
          for z0 = (sin lat0)
          for zr0 = (cos lat0)
          for z1 = (sin lat1)
          for zr1 = (cos lat1)
          do (with-primitives :quad-strip
               (loop for j from lng downto 0
                     for l = (* 2 PI (/ (1- j) lng))
                     for x = (cos l)
                     for y = (sin l)
                     do (gl:normal (* x zr0 size) (* y zr0 size) (* z0 size))
                        (gl:vertex (* x zr0 size) (* y zr0 size) (* z0 size))
                        (gl:normal (* x zr1 size) (* y zr1 size) (* z1 size))
                        (gl:vertex (* x zr1 size) (* y zr1 size) (* z1 size)))))))

(defclass cube (sized-geometry)
  ())

(defmethod paint ((cube cube) target)
  (let ((s (/ (size cube) 2.0)))
    (with-primitives :quads
      (gl:vertex    s     s  (- s))
      (gl:vertex (- s)    s  (- s))
      (gl:vertex (- s)    s     s)
      (gl:vertex    s     s     s)
      (gl:vertex    s  (- s)    s)
      (gl:vertex (- s) (- s)    s)
      (gl:vertex (- s) (- s) (- s))
      (gl:vertex    s  (- s) (- s))
      (gl:vertex    s     s     s)
      (gl:vertex (- s)    s     s)
      (gl:vertex (- s) (- s)    s)
      (gl:vertex    s  (- s)    s)
      (gl:vertex    s  (- s) (- s))
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

(defclass space-axes (sized-geometry segmented-geometry)
  ())

(defmethod paint ((entity space-axes) target)
  (let* ((s (size entity))
         (g (* (/ (segments entity) 2) s)))
    (gl:line-width 1.0)
    (gl:color 0.3 0.3 0.3)
    (with-primitives :lines
      (loop for i from (- g) to g by s
            do (gl:vertex (- g) 0.0 i)
               (gl:vertex g 0.0 i)
               (gl:vertex i 0.0 (- g))
               (gl:vertex i 0.0 g)))
    (gl:line-width 2.0)
    (with-primitives :lines
      (gl:color 1.0 0 0)
      (gl:vertex 0 0 0)
      (gl:vertex s 0 0)
      (gl:color 0 1.0 0)
      (gl:vertex 0 0 0)
      (gl:vertex 0 s 0)
      (gl:color 0 0 1.0)
      (gl:vertex 0 0 0)
      (gl:vertex 0 0 s)))
  (gl:color 1.0 1.0 1.0))
