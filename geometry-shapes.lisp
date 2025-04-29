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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-rectangle-mesh (w h &key (align :center) (normal +vz+) (x 0) (y 0) (z 0) (u- 0) (v- 0) (u+ 1) (v+ 1))
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
      (check-type normal vec3)
      (let ((quat (qtowards +vz+ normal))
            (x (float x 0f0))
            (y (float y 0f0))
            (z (float z 0f0))
            (vec (vec3)))
        (with-mesh-construction (v :attributes (location normal uv))
          (macrolet ((vv (x y u v)
                       `(progn
                          (vsetf vec ,x ,y 0f0)
                          (!q* vec quat vec)
                          (v (+ x (vx vec)) (+ y (vy vec)) (+ z (vz vec)) (vx normal) (vy normal) (vz normal) ,u ,v))))
            (vv l b u- v-)
            (vv r b u+ v-)
            (vv r u u+ v+)
            (vv r u u+ v+)
            (vv l u u- v+)
            (vv l b u- v-))
          (finalize-data)))))

  (defun make-triangle-mesh (w h &key (orientation :right) (x 0) (y 0) (z 0))
    (with-mesh-construction (v :attributes (location normal uv))
      (let ((l (- x (/ w 2)))
            (r (+ x (/ w 2)))
            (u (+ y (/ h 2)))
            (b (- y (/ h 2))))
        (ecase orientation
          (:up
           (v l b z 0 0 1 0 0)
           (v r b z 0 0 1 1 0)
           (v x u z 0 0 1 0.5 1))
          (:right
           (v l u z 0 0 1 0 1)
           (v l b z 0 0 1 0 0)
           (v r y z 0 0 1 1 0.5))))
      (finalize-data)))

  (defun make-cube-mesh (size &key (x 0) (y 0) (z 0))
    (destructuring-bind (w h d) (enlist size size size)
      (let ((w (/ w 2)) (d (/ d 2)) (h (/ h 2)))
        (with-mesh-construction (v :attributes (location normal uv))
          (v (+ x w) (+ y h) (- z d) 0 1 0 1.0 0.0)
          (v (- x w) (+ y h) (- z d) 0 1 0 0.0 0.0)
          (v (- x w) (+ y h) (+ z d) 0 1 0 0.0 1.0)
          (v (- x w) (+ y h) (+ z d) 0 1 0 0.0 1.0)
          (v (+ x w) (+ y h) (+ z d) 0 1 0 1.0 1.0)
          (v (+ x w) (+ y h) (- z d) 0 1 0 1.0 0.0)
          
          (v (+ x w) (- y h) (+ z d) 0 -1 0 1.0 0.0)
          (v (- x w) (- y h) (+ z d) 0 -1 0 0.0 0.0)
          (v (- x w) (- y h) (- z d) 0 -1 0 0.0 1.0)
          (v (- x w) (- y h) (- z d) 0 -1 0 0.0 1.0)
          (v (+ x w) (- y h) (- z d) 0 -1 0 1.0 1.0)
          (v (+ x w) (- y h) (+ z d) 0 -1 0 1.0 0.0)
          
          (v (+ x w) (+ y h) (+ z d) 0 0 1 1.0 0.0)
          (v (- x w) (+ y h) (+ z d) 0 0 1 0.0 0.0)
          (v (- x w) (- y h) (+ z d) 0 0 1 0.0 1.0)
          (v (- x w) (- y h) (+ z d) 0 0 1 0.0 1.0)
          (v (+ x w) (- y h) (+ z d) 0 0 1 1.0 1.0)
          (v (+ x w) (+ y h) (+ z d) 0 0 1 1.0 0.0)
          
          (v (+ x w) (- y h) (- z d) 0 0 -1 1.0 0.0)
          (v (- x w) (- y h) (- z d) 0 0 -1 0.0 0.0)
          (v (- x w) (+ y h) (- z d) 0 0 -1 0.0 1.0)
          (v (- x w) (+ y h) (- z d) 0 0 -1 0.0 1.0)
          (v (+ x w) (+ y h) (- z d) 0 0 -1 1.0 1.0)
          (v (+ x w) (- y h) (- z d) 0 0 -1 1.0 0.0)
          
          (v (- x w) (+ y h) (+ z d) -1 0 0 1.0 0.0)
          (v (- x w) (+ y h) (- z d) -1 0 0 0.0 0.0)
          (v (- x w) (- y h) (- z d) -1 0 0 0.0 1.0)
          (v (- x w) (- y h) (- z d) -1 0 0 0.0 1.0)
          (v (- x w) (- y h) (+ z d) -1 0 0 1.0 1.0)
          (v (- x w) (+ y h) (+ z d) -1 0 0 1.0 0.0)
          
          (v (+ x w) (+ y h) (- z d) 1 0 0 1.0 0.0)
          (v (+ x w) (+ y h) (+ z d) 1 0 0 0.0 0.0)
          (v (+ x w) (- y h) (+ z d) 1 0 0 0.0 1.0)
          (v (+ x w) (- y h) (+ z d) 1 0 0 0.0 1.0)
          (v (+ x w) (- y h) (- z d) 1 0 0 1.0 1.0)
          (v (+ x w) (+ y h) (- z d) 1 0 0 1.0 0.0)
          (finalize-data)))))
  
  (defun make-quad-grid-mesh (size x-count z-count &key (x 0) (y 0) (z 0))
    (with-mesh-construction (v :attributes (location normal uv) :deduplicate NIL)
      (loop for xi from 0 below x-count
            for xc from (* x-count size -0.5) by size
            do (loop for zi from 0 below z-count
                     for zc from (* z-count size -0.5) by size
                     do (let ((l (+ x xc)) (r (+ x xc size))
                              (u (+ z zc)) (b (+ z zc size)))
                          (v l y b 0 1 0 (/ (+ 0 xi) x-count) (/ (+ 0 zi) z-count))
                          (v r y b 0 1 0 (/ (+ 1 xi) x-count) (/ (+ 0 zi) z-count))
                          (v r y u 0 1 0 (/ (+ 1 xi) x-count) (/ (+ 1 zi) z-count))
                          (v r y u 0 1 0 (/ (+ 1 xi) x-count) (/ (+ 1 zi) z-count))
                          (v l y u 0 1 0 (/ (+ 0 xi) x-count) (/ (+ 1 zi) z-count))
                          (v l y b 0 1 0 (/ (+ 0 xi) x-count) (/ (+ 0 zi) z-count)))))
      (finalize-data)))

  (defun make-line-grid-mesh (size w h &key (x 0) (y 0) (z 0))
    (with-mesh-construction (v :attributes (location))
      (let ((w (/ w 2)) (h (/ h 2))
            (ws (/ w size)) (hs (/ h size)))
        (loop for _x from (- w) to w by ws
              do (v (+ x _x) y (- z h))
                 (v (+ x _x) y (+ z h)))
        (loop for _z from (- h) to h by hs
              do (v (- x w) y (+ z _z))
                 (v (+ x w) y (+ z _z))))
      (finalize-data :vertex-form :lines)))

  (defun make-sphere-mesh (size &key (segments 24) (x 0) (y 0) (z 0))
    (let ((lat segments) (lng segments)
          (off (vec x y z)))
      (destructuring-bind (w h d) (enlist size size size)
        (with-mesh-construction (v :attributes (location normal uv))
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
                                     (let ((p (v+ position off))
                                           (n (vunit position))
                                           (u (nv* (nv+ uv 1.0) 0.5)))
                                       (v (vx p) (vy p) (vz p) (vx n) (vy n) (vz n) (vx u) (vy u)))))
                              (vertex (vec (* x1 zr0 w) (* y1 zr0 h) (* z0 d)) (vec x1 y1))
                              (vertex (vec (* x1 zr1 w) (* y1 zr1 h) (* z1 d)) (vec x1 y1))
                              (vertex (vec (* x2 zr0 w) (* y2 zr0 h) (* z0 d)) (vec x2 y2))
                              
                              (vertex (vec (* x2 zr0 w) (* y2 zr0 h) (* z0 d)) (vec x2 y2))
                              (vertex (vec (* x1 zr1 w) (* y1 zr1 h) (* z1 d)) (vec x1 y1))
                              (vertex (vec (* x2 zr1 w) (* y2 zr1 h) (* z1 d)) (vec x2 y2)))))
          (finalize-data)))))

  (defun make-disc-mesh (size &key (segments 32) (x 0) (y 0) (z 0))
    (with-mesh-construction (v :attributes (location normal uv))
      (loop with step = (/ (* 2 PI) segments)
            for i1 = (- step) then i2
            for i2 from 0 to (* 2 PI) by step
            do (v x y z 0 0 1 0.5 0.5)
               (v (+ x (* size (cos i1))) (+ y (* size (sin i1))) z 0 0 1 (+ 0.5 (* 0.5 (cos i1))) (+ 0.5 (* 0.5 (sin i1))))
               (v (+ x (* size (cos i2))) (+ y (* size (sin i2))) z 0 0 1 (+ 0.5 (* 0.5 (cos i2))) (+ 0.5 (* 0.5 (sin i2)))))
      (finalize-data)))

  (defun make-cylinder-mesh (size height &key (segments 32) (x 0) (y 0) (z 0) (radius-top size) (radius-bottom size))
    (with-mesh-construction (v :attributes (location))
      (loop with step = (/ (* 2 PI) segments)
            for i1 = (- step) then i2
            for i2 from 0 to (* 2 PI) by step
            for e1b = (vec (+ x (* radius-bottom (cos i1))) y (+ z (* radius-bottom (sin i1))))
            for e2b = (vec (+ x (* radius-bottom (cos i2))) y (+ z (* radius-bottom (sin i2))))
            for e1t = (vec (+ x (* radius-top (cos i1))) (+ y height) (+ z (* radius-top (sin i1))))
            for e2t = (vec (+ x (* radius-top (cos i2))) (+ y height) (+ z (* radius-top (sin i2))))
            do ;; Bottom disc
            (v x y z)
            (v (vx e1b) (vy e1b) (vz e1b))
            (v (vx e2b) (vy e2b) (vz e2b))
            ;; Top Disc
            (v x (+ height y) z)
            (v (vx e2t) (vy e2t) (vz e2t))
            (v (vx e1t) (vy e1t) (vz e1t))
            ;; Wall
            (v (vx e2b) (vy e2b) (vz e2b))
            (v (vx e1b) (vy e1b) (vz e1b))
            (v (vx e1t) (vy e1t) (vz e1t))
            (v (vx e1t) (vy e1t) (vz e1t))
            (v (vx e2t) (vy e2t) (vz e2t))
            (v (vx e2b) (vy e2b) (vz e2b)))
      (finalize-data)))

  (defun make-cone-mesh (size height &key (segments 32) (x 0) (y 0) (z 0))
    (with-mesh-construction (v :attributes (location))
      (loop with step = (/ (* 2 PI) segments)
            for i1 = (- step) then i2
            for i2 from 0 to (* 2 PI) by step
            do ;; Cone top
            (v x (+ y height) z)
            (v (+ x (* size (cos i2))) y (+ z (* size (sin i2))))
            (v (+ x (* size (cos i1))) y (+ z (* size (sin i1))))
            ;; Bottom disc
            (v x y z)
            (v (+ x (* size (cos i1))) y (+ z (* size (sin i1))))
            (v (+ x (* size (cos i2))) y (+ z (* size (sin i2)))))
      (finalize-data)))

  (defun make-tube-mesh (size height inner-size &key (segments 32) (x 0) (y 0) (z 0))
    (with-mesh-construction (v :attributes (location))
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
            (v (vx f1b) (vy f1b) (vz f1b))
            (v (vx e1b) (vy e1b) (vz e1b))
            (v (vx e2b) (vy e2b) (vz e2b))
            (v (vx e2b) (vy e2b) (vz e2b))
            (v (vx f2b) (vy f2b) (vz f2b))
            (v (vx f1b) (vy f1b) (vz f1b))
            ;; Top ring
            (v (vx f2t) (vy f2t) (vz f2t))
            (v (vx e2t) (vy e2t) (vz e2t))
            (v (vx e1t) (vy e1t) (vz e1t))
            (v (vx e1t) (vy e1t) (vz e1t))
            (v (vx f1t) (vy f1t) (vz f1t))
            (v (vx f2t) (vy f2t) (vz f2t))
            ;; Outer wall
            (v (vx e2b) (vy e2b) (vz e2b))
            (v (vx e1b) (vy e1b) (vz e1b))
            (v (vx e1t) (vy e1t) (vz e1t))
            (v (vx e1t) (vy e1t) (vz e1t))
            (v (vx e2t) (vy e2t) (vz e2t))
            (v (vx e2b) (vy e2b) (vz e2b))
            ;; Inner wall
            (v (vx f2b) (vy f2b) (vz f2b))
            (v (vx f1t) (vy f1t) (vz f1t))
            (v (vx f1b) (vy f1b) (vz f1b))
            (v (vx f1t) (vy f1t) (vz f1t))
            (v (vx f2b) (vy f2b) (vz f2b))
            (v (vx f2t) (vy f2t) (vz f2t)))
      (finalize-data)))

  (defun make-lines (points &key (default-color (vec 0 0 0 1)))
    (with-mesh-construction (v :attributes (location normal color) :deduplicate NIL)
      (loop for (a b) on points by #'cddr
            while b
            do (destructuring-bind (a ac) (enlist a default-color)
                 (destructuring-bind (b bc) (enlist b default-color)
                   (let ((a-b (v- a b))
                         (b-a (v- b a)))
                     (v (vx a) (vy a) (vz a) (vx a-b) (vy a-b) (vz a-b) (vx ac) (vy ac) (vz ac))
                     (v (vx b) (vy b) (vz b) (vx a-b) (vy a-b) (vz a-b) (vx bc) (vy bc) (vz bc))
                     (v (vx a) (vy a) (vz a) (vx b-a) (vy b-a) (vz b-a) (vx ac) (vy ac) (vz ac))
                     (v (vx b) (vy b) (vz b) (vx a-b) (vy a-b) (vz a-b) (vx bc) (vy bc) (vz bc))
                     (v (vx b) (vy b) (vz b) (vx b-a) (vy b-a) (vz b-a) (vx bc) (vy bc) (vz bc))
                     (v (vx a) (vy a) (vz a) (vx b-a) (vy b-a) (vz b-a) (vx ac) (vy ac) (vz ac))))))
      (finalize-data :vertex-form :lines))))

(define-asset (trial fullscreen-square) mesh
    (make-rectangle-mesh 2 2))

(define-asset (trial empty-vertex-array) mesh
    (make-instance 'mesh-data :faces (make-array 0 :element-type '(unsigned-byte 16))))

(define-asset (trial point) mesh
    (with-mesh-construction (v :attributes (location normal))
      (v 0 0 0 0 1 0)
      (finalize-data :vertex-form :points)))

(define-asset (trial unit-line) mesh
    (with-mesh-construction (v :attributes (location normal))
      (v 0 0 0 0 1 0)
      (v 0 1 0 0 1 0)
      (finalize-data :vertex-form :lines)))

(define-asset (trial unit-cube) mesh
    (make-cube-mesh 1.0))

(define-asset (trial unit-sphere) mesh
    (make-sphere-mesh 1.0))

(define-asset (trial unit-square) mesh
    (make-rectangle-mesh 1 1))

(define-asset (trial unit-disc) mesh
    (make-disc-mesh 1.0))

(define-asset (trial unit-cylinder) mesh
    (make-cylinder-mesh 1.0 1.0))

(define-asset (trial unit-cone) mesh
    (make-cone-mesh 1.0 1.0))

(define-asset (trial unit-tube) mesh
    (make-tube-mesh 1.0 1.0 0.5))

(define-asset (trial unit-point) mesh
    (make-triangle-mesh 0.0 0.0))

(define-asset (trial triangle) mesh
    (with-mesh-construction (v :attributes (location normal uv color))
      (v -0.5 -0.5 0.0 0 0 -1 0.0 0.0 1 0 0 1)
      (v +0.5 -0.5 0.0 0 0 -1 0.1 0.0 0 1 0 1)
      (v +0.0 +0.5 0.0 0 0 -1 0.5 0.1 0 0 1 1)
      (finalize-data)))

(define-asset (trial grid) mesh
    (make-line-grid-mesh 10 10 10))

(define-asset (trial axes) mesh
    (make-lines (list (list (vec 0 0 0) (vec 1 0 0 1)) (list (vec 10 0 0) (vec 1 0 0 1))
                      (list (vec 0 0 0) (vec 0 1 0 1)) (list (vec 0 10 0) (vec 0 1 0 1))
                      (list (vec 0 0 0) (vec 0 0 1 1)) (list (vec 0 0 10) (vec 0 0 1 1)))))

(define-asset (trial 2d-axes) mesh
    (with-mesh-construction (v :attributes (location))
      ;; KLUDGE: for whatever reason using most-positive/negative-single-float does not work.
      (v 0 -1000000 0)
      (v 0 +1000000 0)
      (v -1000000 0 0)
      (v +1000000 0 0)
      (finalize-data :vertex-form :lines)))
