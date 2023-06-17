#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: Allow specifying which attributes to include
;; FIXME: Generate normals
;; FIXME: Generate UVs
;; FIXME: Add shapes:
;;        - Box
;;        - Torus
;;        - Arrow
;; FIXME: Normalise interface (vectors instead of x/y/z args?)
;; NOTE: UV convention is U is pointing RIGHT, V is pointing UP
;; NOTE: Should generate triangle vertices in CCW order

(defun make-rectangle-mesh (w h &key (align :center) mesh pack (x 0) (y 0) (z 0) (u- 0) (v- 0) (u+ 1) (v+ 1))
  (let (l r u b)
    (ecase align
      (:center (setf l (- (/ w 2)) r (+ (/ w 2))
                     b (- (/ h 2)) u (+ (/ h 2))))
      (:topleft (setf l 0 r w
                      b (- h) u 0))
      (:topcenter (setf l (- (/ w 2)) r (+ (/ w 2))
                        b (- h) u 0))
      (:bottomleft (setf l 0 r w
                         b 0 u h)))
    (incf l x) (incf r x) (incf u y) (incf b y)
    (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'basic-vertex)) :pack pack)
      (vertex :position (vec l b z) :normal +vz3+ :uv (vec u- v-))
      (vertex :position (vec r b z) :normal +vz3+ :uv (vec u+ v-))
      (vertex :position (vec r u z) :normal +vz3+ :uv (vec u+ v+))
      (vertex :position (vec r u z) :normal +vz3+ :uv (vec u+ v+))
      (vertex :position (vec l u z) :normal +vz3+ :uv (vec u- v+))
      (vertex :position (vec l b z) :normal +vz3+ :uv (vec u- v-)))))

(defun make-triangle-mesh (w h &key (orientation :right) mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex)) :pack pack)
    (let ((l (- x (/ w 2)))
          (r (+ x (/ w 2)))
          (u (+ y (/ h 2)))
          (b (- y (/ h 2))))
      (ecase orientation
        (:up
         (vertex :position (vec l b z))
         (vertex :position (vec r b z))
         (vertex :position (vec x u z)))
        (:right
         (vertex :position (vec l u z))
         (vertex :position (vec l b z))
         (vertex :position (vec r y z)))))))

(defun make-cube-mesh (size &key mesh pack (x 0) (y 0) (z 0))
  (destructuring-bind (w h d) (enlist size size size)
    (let ((w (/ w 2)) (d (/ d 2)) (h (/ h 2)))
      (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'basic-vertex)) :pack pack)
        (vertex :position (vec (+ x w) (+ y h) (- z d)) :uv (vec 1.0 0.0) :normal (vec 0 1 0))
        (vertex :position (vec (- x w) (+ y h) (- z d)) :uv (vec 0.0 0.0) :normal (vec 0 1 0))
        (vertex :position (vec (- x w) (+ y h) (+ z d)) :uv (vec 0.0 1.0) :normal (vec 0 1 0))
        (vertex :position (vec (- x w) (+ y h) (+ z d)) :uv (vec 0.0 1.0) :normal (vec 0 1 0))
        (vertex :position (vec (+ x w) (+ y h) (+ z d)) :uv (vec 1.0 1.0) :normal (vec 0 1 0))
        (vertex :position (vec (+ x w) (+ y h) (- z d)) :uv (vec 1.0 0.0) :normal (vec 0 1 0))

        (vertex :position (vec (+ x w) (- y h) (+ z d)) :uv (vec 1.0 0.0) :normal (vec 0 -1 0))
        (vertex :position (vec (- x w) (- y h) (+ z d)) :uv (vec 0.0 0.0) :normal (vec 0 -1 0))
        (vertex :position (vec (- x w) (- y h) (- z d)) :uv (vec 0.0 1.0) :normal (vec 0 -1 0))
        (vertex :position (vec (- x w) (- y h) (- z d)) :uv (vec 0.0 1.0) :normal (vec 0 -1 0))
        (vertex :position (vec (+ x w) (- y h) (- z d)) :uv (vec 1.0 1.0) :normal (vec 0 -1 0))
        (vertex :position (vec (+ x w) (- y h) (+ z d)) :uv (vec 1.0 0.0) :normal (vec 0 -1 0))

        (vertex :position (vec (+ x w) (+ y h) (+ z d)) :uv (vec 1.0 0.0) :normal (vec 0 0 1))
        (vertex :position (vec (- x w) (+ y h) (+ z d)) :uv (vec 0.0 0.0) :normal (vec 0 0 1))
        (vertex :position (vec (- x w) (- y h) (+ z d)) :uv (vec 0.0 1.0) :normal (vec 0 0 1))
        (vertex :position (vec (- x w) (- y h) (+ z d)) :uv (vec 0.0 1.0) :normal (vec 0 0 1))
        (vertex :position (vec (+ x w) (- y h) (+ z d)) :uv (vec 1.0 1.0) :normal (vec 0 0 1))
        (vertex :position (vec (+ x w) (+ y h) (+ z d)) :uv (vec 1.0 0.0) :normal (vec 0 0 1))

        (vertex :position (vec (+ x w) (- y h) (- z d)) :uv (vec 1.0 0.0) :normal (vec 0 0 -1))
        (vertex :position (vec (- x w) (- y h) (- z d)) :uv (vec 0.0 0.0) :normal (vec 0 0 -1))
        (vertex :position (vec (- x w) (+ y h) (- z d)) :uv (vec 0.0 1.0) :normal (vec 0 0 -1))
        (vertex :position (vec (- x w) (+ y h) (- z d)) :uv (vec 0.0 1.0) :normal (vec 0 0 -1))
        (vertex :position (vec (+ x w) (+ y h) (- z d)) :uv (vec 1.0 1.0) :normal (vec 0 0 -1))
        (vertex :position (vec (+ x w) (- y h) (- z d)) :uv (vec 1.0 0.0) :normal (vec 0 0 -1))

        (vertex :position (vec (- x w) (+ y h) (+ z d)) :uv (vec 1.0 0.0) :normal (vec -1 0 0))
        (vertex :position (vec (- x w) (+ y h) (- z d)) :uv (vec 0.0 0.0) :normal (vec -1 0 0))
        (vertex :position (vec (- x w) (- y h) (- z d)) :uv (vec 0.0 1.0) :normal (vec -1 0 0))
        (vertex :position (vec (- x w) (- y h) (- z d)) :uv (vec 0.0 1.0) :normal (vec -1 0 0))
        (vertex :position (vec (- x w) (- y h) (+ z d)) :uv (vec 1.0 1.0) :normal (vec -1 0 0))
        (vertex :position (vec (- x w) (+ y h) (+ z d)) :uv (vec 1.0 0.0) :normal (vec -1 0 0))

        (vertex :position (vec (+ x w) (+ y h) (- z d)) :uv (vec 1.0 0.0) :normal (vec 1 0 0))
        (vertex :position (vec (+ x w) (+ y h) (+ z d)) :uv (vec 0.0 0.0) :normal (vec 1 0 0))
        (vertex :position (vec (+ x w) (- y h) (+ z d)) :uv (vec 0.0 1.0) :normal (vec 1 0 0))
        (vertex :position (vec (+ x w) (- y h) (+ z d)) :uv (vec 0.0 1.0) :normal (vec 1 0 0))
        (vertex :position (vec (+ x w) (- y h) (- z d)) :uv (vec 1.0 1.0) :normal (vec 1 0 0))
        (vertex :position (vec (+ x w) (+ y h) (- z d)) :uv (vec 1.0 0.0) :normal (vec 1 0 0))))))

(defun make-quad-grid-mesh (size x-count z-count &key mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'basic-vertex)) :pack pack)
    (loop for xi from 0 below x-count
          for xc from (* x-count size -0.5) by size
          do (loop for zi from 0 below z-count
                   for zc from (* z-count size -0.5) by size
                   do (let ((l (+ x xc)) (r (+ x xc size))
                            (u (+ z zc)) (b (+ z zc size)))
                        (vertex :position (vec l y b) :uv (vec (/ (+ 0 xi) x-count) (/ (+ 0 zi) z-count)) :normal (vec 0 1 0))
                        (vertex :position (vec r y b) :uv (vec (/ (+ 1 xi) x-count) (/ (+ 0 zi) z-count)) :normal (vec 0 1 0))
                        (vertex :position (vec r y u) :uv (vec (/ (+ 1 xi) x-count) (/ (+ 1 zi) z-count)) :normal (vec 0 1 0))
                        (vertex :position (vec r y u) :uv (vec (/ (+ 1 xi) x-count) (/ (+ 1 zi) z-count)) :normal (vec 0 1 0))
                        (vertex :position (vec l y u) :uv (vec (/ (+ 0 xi) x-count) (/ (+ 1 zi) z-count)) :normal (vec 0 1 0))
                        (vertex :position (vec l y b) :uv (vec (/ (+ 0 xi) x-count) (/ (+ 0 zi) z-count)) :normal (vec 0 1 0)))))))

(defun make-line-grid-mesh (size w h &key mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex :face-length 2)) :pack pack)
    (let ((w (/ w 2)) (h (/ h 2))
          (ws (/ w size)) (hs (/ h size)))
      (loop for _x from (- w) to w by ws
            do (vertex :position (vec (+ x _x) y (- z h)))
               (vertex :position (vec (+ x _x) y (+ z h))))
      (loop for _z from (- h) to h by hs
            do (vertex :position (vec (- x w) y (+ z _z)))
               (vertex :position (vec (+ x w) y (+ z _z)))))))

(defun make-sphere-mesh (size &key (segments 24) mesh pack (x 0) (y 0) (z 0))
  (let ((lat segments) (lng segments)
        (off (vec x y z)))
    (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'basic-vertex)) :pack pack)
      (loop for i from lat downto 1
            for lat0 = (* PI (- (/ (1- i) lat) 0.5))
            for lat1 = (* PI (- (/ i lat) 0.5))
            for z0 = (sin lat0)
            for zr0 = (cos lat0)
            for z1 = (sin lat1)
            for zr1 = (cos lat1)
            do (loop for j from lng downto 0
                     for l1 = (* 2 PI (/ (- j 1) lng)) for l2 = (* 2 PI (/ (- j 2) lng))
                     for x1 = (cos l1) for x2 = (cos l2)
                     for y1 = (sin l1) for y2 = (sin l2)
                     do (flet ((vertex (position uv)
                                 (vertex :position (v+ position off) :normal (vunit position) :uv (nv* (nv+ uv 1.0) 0.5))))
                          (vertex (vec (* x1 zr0 size) (* y1 zr0 size) (* z0 size)) (vec x1 y1))
                          (vertex (vec (* x1 zr1 size) (* y1 zr1 size) (* z1 size)) (vec x1 y1))
                          (vertex (vec (* x2 zr0 size) (* y2 zr0 size) (* z0 size)) (vec x2 y2))
                          
                          (vertex (vec (* x2 zr0 size) (* y2 zr0 size) (* z0 size)) (vec x2 y2))
                          (vertex (vec (* x1 zr1 size) (* y1 zr1 size) (* z1 size)) (vec x1 y1))
                          (vertex (vec (* x2 zr1 size) (* y2 zr1 size) (* z1 size)) (vec x2 y2))))))))

(defun make-disc-mesh (size &key (segments 32) mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex)) :pack pack)
    (loop with step = (/ (* 2 PI) segments)
          for i1 = (- step) then i2
          for i2 from 0 to (* 2 PI) by step
          do (vertex :position (vec x y z))
             (vertex :position (vec (+ x (* size (cos i1))) (+ y (* size (sin i1))) z))
             (vertex :position (vec (+ x (* size (cos i2))) (+ y (* size (sin i2))) z)))))

(defun make-cylinder-mesh (size height &key (segments 32) mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex)) :pack pack)
    (loop with step = (/ (* 2 PI) segments)
          for i1 = (- step) then i2
          for i2 from 0 to (* 2 PI) by step
          for e1b = (vec (+ x (* size (cos i1))) y (+ z (* size (sin i1))))
          for e2b = (vec (+ x (* size (cos i2))) y (+ z (* size (sin i2))))
          for e1t = (nv+ (vec 0 height 0) e1b)
          for e2t = (nv+ (vec 0 height 0) e2b)
          do ;; Bottom disc
             (vertex :position (vec x y z))
             (vertex :position e2b)
             (vertex :position e1b)
             ;; Top Disc
             (vertex :position (vec x (+ height y) z))
             (vertex :position e1t)
             (vertex :position e2t)
             ;; Wall
             (vertex :position e2b)
             (vertex :position e1t)
             (vertex :position e1b)
             (vertex :position e1t)
             (vertex :position e2b)
             (vertex :position e2t))))

(defun make-cone-mesh (size height &key (segments 32) mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex)) :pack pack)
    (loop with step = (/ (* 2 PI) segments)
          for i1 = (- step) then i2
          for i2 from 0 to (* 2 PI) by step
          do ;; Cone top
             (vertex :position (vec x (+ y height) z))
             (vertex :position (vec (+ x (* size (cos i1))) y (+ z (* size (sin i1)))))
             (vertex :position (vec (+ x (* size (cos i2))) y (+ z (* size (sin i2)))))
             ;; Bottom disc
             (vertex :position (vec x y z))
             (vertex :position (vec (+ x (* size (cos i2))) y (+ z (* size (sin i2)))))
             (vertex :position (vec (+ x (* size (cos i1))) y (+ z (* size (sin i1))))))))

(defun make-tube-mesh (size height inner-size &key (segments 32) mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex)) :pack pack)
    (loop with step = (/ (* 2 PI) segments)
          for i1 = (- step) then i2
          for i2 from 0 to (* 2 PI) by step
          for e1b = (vec (+ x (* size (cos i1))) y (+ z (* size (sin i1))))
          for e2b = (vec (+ x (* size (cos i2))) y (+ z (* size (sin i2))))
          for e1t = (nv+ (vec 0 height 0) e1b)
          for e2t = (nv+ (vec 0 height 0) e2b)
          
          for f1b = (vec (+ x (* inner-size (cos i1))) y (+ z (* inner-size (sin i1))))
          for f2b = (vec (+ x (* inner-size (cos i2))) y (+ z (* inner-size (sin i2))))
          for f1t = (nv+ (vec 0 height 0) f1b)
          for f2t = (nv+ (vec 0 height 0) f2b)
          do ;; Bottom ring
          (vertex :position f1b)
          (vertex :position e1b)
          (vertex :position e2b)
          (vertex :position e2b)
          (vertex :position f2b)
          (vertex :position f1b)
          ;; Top ring
          (vertex :position f2t)
          (vertex :position e2t)
          (vertex :position e1t)
          (vertex :position e1t)
          (vertex :position f1t)
          (vertex :position f2t)
          ;; ;; Outer wall
          (vertex :position e2b)
          (vertex :position e1b)
          (vertex :position e1t)
          (vertex :position e1t)
          (vertex :position e2t)
          (vertex :position e2b)
          ;; ;; Inner wall
          (vertex :position f2b)
          (vertex :position f1t)
          (vertex :position f1b)
          (vertex :position f1t)
          (vertex :position f2b)
          (vertex :position f2t))))

(defclass line-vertex (colored-vertex normal-vertex)
  ())

(defun make-lines (points &key mesh (default-color (vec 0 0 0 1)))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'line-vertex)))
    (loop for (a b) on points by #'cddr
          while b
          do (destructuring-bind (a ac) (enlist a default-color)
               (destructuring-bind (b bc) (enlist b default-color)
                 (vertex :location a :normal (v- a b) :color ac)
                 (vertex :location b :normal (v- a b) :color bc)
                 (vertex :location a :normal (v- b a) :color ac)
                 (vertex :location b :normal (v- a b) :color bc)
                 (vertex :location b :normal (v- b a) :color bc)
                 (vertex :location a :normal (v- b a) :color ac))))))

(define-asset (trial fullscreen-square) mesh
    (make-rectangle-mesh 2 2 :pack T))

(define-asset (trial empty-vertex-array) mesh
    (make-instance 'vertex-mesh))

(define-asset (trial unit-cube) mesh
    (make-cube-mesh 1.0 :pack T))

(define-asset (trial axes) mesh
    (make-lines (list (list (vec 0 0 0) (vec 1 0 0 1)) (list (vec 10 0 0) (vec 1 0 0 1))
                      (list (vec 0 0 0) (vec 0 1 0 1)) (list (vec 0 10 0) (vec 0 1 0 1))
                      (list (vec 0 0 0) (vec 0 0 1 1)) (list (vec 0 0 10) (vec 0 0 1 1)))))

(define-asset (trial 2d-axes) mesh
    (with-vertex-filling ((make-instance 'vertex-mesh :face-length 2))
      ;; KLUDGE: for whatever reason using most-positive/negative-single-float does not work.
      (vertex :location (vec 0 -1000000 0))
      (vertex :location (vec 0 +1000000 0))
      (vertex :location (vec -1000000 0 0))
      (vertex :location (vec +1000000 0 0))))
