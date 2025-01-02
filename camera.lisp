(in-package #:org.shirakumo.fraf.trial)

(defclass camera (located-entity listener)
  ((name :initform :camera)
   (location :initform (vec 0 30 200))
   (near-plane :initarg :near-plane :initform 0.1f0 :accessor near-plane)
   (far-plane :initarg :far-plane :initform 10000.0f0 :accessor far-plane)
   (bsize :initform (vec2 0 0) :accessor bsize)))

(defgeneric project-view (camera))
(defgeneric setup-perspective (camera width height))
(defgeneric map-visible (function camera container))
(defgeneric in-view-p (object camera))
(defgeneric focal-point (camera))
(defgeneric screen-area (thing camera))

(defmethod activate ((camera camera))
  (setf (camera (scene +main+)) camera))

(defmethod width ((camera camera))
  (* 2 (vx (bsize camera))))

(defmethod height ((camera camera))
  (* 2 (vy (bsize camera))))

(defmethod handle ((ev tick) (camera camera))
  (project-view camera))

(defmethod handle ((ev resize) (camera camera))
  (vsetf (bsize camera) (* 0.5 (width ev)) (* 0.5 (height ev)))
  (setup-perspective camera (width ev) (height ev)))

(defmethod (setf near-plane) :around (val (camera camera))
  (when (/= val (near-plane camera))
    (call-next-method)
    (setup-perspective camera T T)))

(defmethod (setf far-plane) :around (val (camera camera))
  (when (/= val (far-plane camera))
    (call-next-method)
    (setup-perspective camera T T)))

(defmethod setup-perspective ((camera camera) (w (eql T)) (h (eql T)))
  (let ((bsize (bsize camera)))
    (setup-perspective camera (max 1 (* 2 (vx bsize))) (max 1 (* 2 (vy bsize))))))

(defmethod setup-perspective :before ((camera camera) (w real) (h real))
  (reset-matrix *projection-matrix*))

(defmethod project-view :before ((camera camera))
  (reset-matrix))

(defmethod map-visible (function (camera camera) (object scene-node))
  (when (in-view-p object camera)
    (funcall function object)))

(defmethod map-visible (function (camera camera) (container container))
  (do-scene-graph (object container)
    (when (in-view-p object camera)
      (funcall function object))))

(defmethod map-visible (function (camera null) (container container))
  (do-scene-graph (object container)
    (funcall function object)))

(defmacro do-visible ((entity camera container &optional return) &body body)
  `(block NIL
     (map-visible (lambda (,entity) ,@body) ,camera ,container)
     ,return))

(defmethod in-view-p (object (camera camera)) T)

(defmethod in-view-p (object (standard (eql T)))
  (in-view-p object (camera (scene +main+))))

(defclass 2d-camera (camera)
  ((near-plane :initform 0.0)
   (far-plane :initform 100.0)
   (location :initform (vec 0 0 200))))

(defmethod setup-perspective ((camera 2d-camera) (width real) (height real))
  (orthographic-projection 0 (max 1 width) 0 (max 1 height) (near-plane camera) (far-plane camera)))

(defmethod project-view ((camera 2d-camera))
  (reset-matrix *view-matrix*)
  (translate (v- (location camera)) *view-matrix*))

(defmethod screen-area ((null null) (camera 2d-camera))
  (let ((bsize (bsize camera)))
    (* (vx bsize) (vy bsize) 4)))

(defmethod screen-area ((entity sized-entity) (camera 2d-camera))
  (* (vx (bsize entity)) (vy (bsize entity))))

(defmethod in-view-p ((entity sized-entity) (camera 2d-camera))
  (let ((eloc (global-location entity))
        (esiz (global-bsize entity))
        (cloc (location camera))
        (csiz (bsize camera)))
    (and (<= (abs (- (vx eloc) (vx cloc))) (+ (vx esiz) (vx csiz)))
         (<= (abs (- (vy eloc) (vy cloc))) (+ (vy esiz) (vy csiz))))))

(defmethod in-view-p ((entity global-bounds-cached-entity) (camera 2d-camera))
  (let ((eloc (global-location entity))
        (esiz (global-bsize entity))
        (cloc (location camera))
        (csiz (bsize camera)))
    (and (<= (abs (- (vx eloc) (vx cloc))) (+ (vx esiz) (vx csiz)))
         (<= (abs (- (vy eloc) (vy cloc))) (+ (vy esiz) (vy csiz))))))

(defclass sidescroll-camera (2d-camera)
  ((zoom :initarg :zoom :initform 1.0 :accessor zoom)
   (target :initarg :target :initform (vec 0 0 200) :accessor target)))

(defmethod project-view ((camera sidescroll-camera))
  (let* ((z (zoom camera))
         (v (vxy_ (bsize camera))))
    (declare (dynamic-extent v))
    (reset-matrix *view-matrix*)
    (scale-by z z z *view-matrix*)
    (translate (nv- (nv/ v z) (location camera)) *view-matrix*)))

(defmethod focal-point ((camera sidescroll-camera))
  (global-location (target camera)))

(define-handler ((camera sidescroll-camera) tick) ()
  (project-view camera)
  (let* ((loc (location camera))
         (int (vcopy loc)))
    (when (target camera)
      (let ((tar (location (target camera))))
        (vsetf int (round (vx tar)) (round (vy tar)) (vz loc))))
    (let* ((dir (v- int loc))
           (len (max 1 (vlength dir)))
           (ease (clamp 0 (+ 0.2 (/ (expt len 1.4) 100)) 20)))
      (nv* dir (/ ease len))
      (nv+ loc dir))))

;; TODO: implement in-view-p respective to zoom

(defclass 3d-camera (camera)
  ((fov :initarg :fov :initform 75.0 :accessor fov)
   (frustum :initform (make-convex-mesh :vertices (f32-vec -1 -1 -1  +1 -1 -1  +1 +1 -1  -1 +1 -1
                                                           -1 -1 +1  +1 -1 +1  +1 +1 +1  -1 +1 +1)
                                        :faces (u16-vec 0 1 2  2 3 0  4 5 6  6 7 4))
            :accessor frustum)))

(defmethod (setf fov) :around (val (camera 3d-camera))
  (when (/= (fov camera) val)
    (call-next-method)
    (setup-perspective camera T T)))

(defmethod setup-perspective ((camera 3d-camera) (width real) (height real))
  (perspective-projection (fov camera) (/ (max 1 width) (max 1 height)) (near-plane camera) (far-plane camera))
  ;; Compute the view frustum edge points
  (let ((inv (minv *projection-matrix*))
        (tmp (vec4))
        (verts (convex-mesh-vertices (frustum camera))))
    (declare (dynamic-extent inv tmp))
    (with-fast-matref (m inv)
      (flet ((invert (v)
               (let ((w (/ (+ (* (vx v) (m 3 0)) (* (vy v) (m 3 1)) (* (vz v) (m 3 2)) (m 3 3)))))
                 (n*m inv (vsetf tmp (* (vx v) w) (* (vy v) w) (* (vz v) w) w)))))
        (loop for v across #(#.(vec -1 -1 -1) #.(vec +1 -1 -1) #.(vec +1 +1 -1) #.(vec -1 +1 -1)
                             #.(vec -1 -1 +1) #.(vec +1 -1 +1) #.(vec +1 +1 +1) #.(vec -1 +1 +1))
              for i from 0 by 3
              do (invert v)
                 (replace verts (varr tmp) :start1 i))))))

(defmethod focal-point ((camera 3d-camera))
  #.(vec 0 0 0))

(defmethod project-view ((camera 3d-camera))
  (look-at (location camera) (vec 0 0 0) +vy3+))

(defmethod project-view :after ((camera 3d-camera))
  (let ((inv (minv *view-matrix*))
        (frustum (frustum camera)))
    (declare (dynamic-extent inv))
    (!m* (primitive-transform frustum) inv (primitive-local-transform frustum))
    (invalidate-global-bounds-cache frustum)))

(defmethod screen-area ((null null) (camera 3d-camera))
  (let ((x (* 2 (near-plane camera) (tan (* 0.5 (fov camera)))))
        (bsize (bsize camera)))
    (/ (* x x) (/ (vx bsize) (vy bsize)))))

;; FIXME: Test this stuff
(defmethod screen-area ((entity sized-entity) (camera 3d-camera))
  (with-vec (x y z) (the *vec3 (bsize entity))
    (let ((p1 (vec (- x) (- y) (- z)))
          (p2 (vec (+ x) (- y) (- z))) ;   p7 -- p8
          (p3 (vec (- x) (+ y) (- z))) ;  /|    / |
          (p4 (vec (+ x) (+ y) (- z))) ; p3 -- p4 |
          (p5 (vec (- x) (- y) (+ z))) ; | p5 --|p6
          (p6 (vec (+ x) (- y) (+ z))) ; |/     |/
          (p7 (vec (- x) (+ y) (+ z))) ; p1 -- p2
          (p8 (vec (+ x) (+ y) (+ z))))
      (declare (dynamic-extent p1 p2 p3 p4 p5 p6 p7 p8))
      (let* ((matrix (meye 4))
             (*model-matrix* matrix))
        (declare (dynamic-extent matrix))
        (apply-transforms entity)
        (n*m *view-matrix* matrix)
        (n*m *projection-matrix* matrix)
        (n*m matrix p1) (n*m matrix p2) (n*m matrix p3) (n*m matrix p4)
        (n*m matrix p5) (n*m matrix p6) (n*m matrix p7) (n*m matrix p8)
        (let ((f1 (vlength (vc (v- p2 p1) (v- p3 p1))))
              (f2 (vlength (vc (v- p7 p3) (v- p4 p3))))
              (f3 (vlength (vc (v- p7 p5) (v- p6 p5))))
              (f4 (vlength (vc (v- p5 p1) (v- p2 p1))))
              (f5 (vlength (vc (v- p4 p2) (v- p6 p2))))
              (f6 (vlength (vc (v- p3 p1) (v- p5 p1)))))
          (* 0.5 (+ f1 f2 f3 f4 f5 f6)))))))

(defmethod screen-area ((entity global-bounds-cached-entity) (camera 3d-camera))
  (with-vec (x y z) (the *vec3 (global-bsize entity))
    (let ((p1 (vec (- x) (- y) (- z)))
          (p2 (vec (+ x) (- y) (- z))) ;   p7 -- p8
          (p3 (vec (- x) (+ y) (- z))) ;  /|    / |
          (p4 (vec (+ x) (+ y) (- z))) ; p3 -- p4 |
          (p5 (vec (- x) (- y) (+ z))) ; | p5 --|p6
          (p6 (vec (+ x) (- y) (+ z))) ; |/     |/
          (p7 (vec (- x) (+ y) (+ z))) ; p1 -- p2
          (p8 (vec (+ x) (+ y) (+ z))))
      (declare (dynamic-extent p1 p2 p3 p4 p5 p6 p7 p8))
      (let* ((matrix (mtranslation (global-location entity))))
        (declare (dynamic-extent matrix))
        (n*m *view-matrix* matrix)
        (n*m *projection-matrix* matrix)
        (n*m matrix p1) (n*m matrix p2) (n*m matrix p3) (n*m matrix p4)
        (n*m matrix p5) (n*m matrix p6) (n*m matrix p7) (n*m matrix p8)
        (let ((f1 (vlength (vc (v- p2 p1) (v- p3 p1))))
              (f2 (vlength (vc (v- p7 p3) (v- p4 p3))))
              (f3 (vlength (vc (v- p7 p5) (v- p6 p5))))
              (f4 (vlength (vc (v- p5 p1) (v- p2 p1))))
              (f5 (vlength (vc (v- p4 p2) (v- p6 p2))))
              (f6 (vlength (vc (v- p3 p1) (v- p5 p1)))))
          (* 0.5 (+ f1 f2 f3 f4 f5 f6)))))))

(defmethod in-view-p ((entity sized-entity) (camera 3d-camera))
  (let ((box (make-box :bsize (global-bsize entity) :location (global-location entity))))
    (in-view-p box camera)))

(defmethod in-view-p ((entity global-bounds-cached-entity) (camera 3d-camera))
  (let ((box (make-box :bsize (global-bsize entity) :location (global-location entity))))
    (declare (dynamic-extent box))
    (in-view-p box camera)))

(defmethod in-view-p ((entity rigid-shape) camera)
  (loop for primitive across (physics-primitives entity)
        thereis (in-view-p primitive camera)))

(defmethod in-view-p ((primitive primitive) (camera 3d-camera))
  (let* ((hit (make-hit)) (hits (make-array 1 :initial-element hit)))
    (declare (dynamic-extent hits hit))
    (< 0 (org.shirakumo.fraf.trial.gjk:detect-hits primitive (frustum camera) hits 0 1 NIL))))

(defmethod in-view-p ((primitive all-space) (camera camera)) T)

(defclass target-camera (3d-camera)
  ((target :initarg :target :initform (vec3 0) :accessor target)
   (up :initarg :up :initform +vy3+ :accessor up)))

(defmethod project-view ((camera target-camera))
  (let ((matrix (meye 4))
        (loc (vec3))
        (tar (v- (target camera) (location camera))))
    (declare (dynamic-extent matrix loc tar))
    (global-transform-matrix camera matrix)
    (mcol3 matrix 3 loc)
    (n*m matrix tar)
    (look-at loc tar (up camera))))

(defmethod focal-point ((camera target-camera))
  (global-location (target camera)))

(defclass pivot-camera (target-camera)
  ((rotation :initform (quat) :accessor rotation)
   (radius :initform 1.0 :initarg :radius :accessor radius)))

(defmethod initialize-instance :after ((camera pivot-camera) &key)
  (setf (rotation camera) (rotation camera)))

(defmethod (setf rotation) :after (r (camera pivot-camera))
  (vsetf (location camera) (radius camera) 0 0)
  (!q* (location camera) (rotation camera) (location camera)))

(define-handler (pivot-camera mouse-move) (old-pos pos)
  ;; FIXME: this isn't right yet.
  (when (or (retained :middle)
            (retained :left-control))
    (let ((dp (vyx (v- pos old-pos)))
          (rot (rotation pivot-camera)))
      (nq* rot (qfrom-angle +vx+ (* -0.01 (vx dp))))
      (nq* rot (qfrom-angle +vy+ (* -0.01 (vy dp))))
      (setf (rotation pivot-camera) rot))))

(defclass following-camera (target-camera)
  ((target :initform NIL)))

(defmethod project-view ((camera following-camera))
  (when (target camera)
    (look-at (v+ (location camera)
                 (location (target camera)))
             (location (target camera))
             (up camera))))

(defclass fps-camera (3d-camera)
  ((rotation :initarg :rotation :initform (vec 0 0 0) :accessor rotation)
   (x-acceleration :initarg :x-acceleration :initform 0.01 :accessor x-acceleration)
   (y-acceleration :initarg :y-acceleration :initform 0.01 :accessor y-acceleration)))

(defmethod project-view ((camera fps-camera))
  (reset-matrix (view-matrix))
  (rotate +vx+ (vx (rotation camera)) (view-matrix))
  (rotate +vy+ (vy (rotation camera)) (view-matrix))
  (with-vec (x y z) (the vec3 (location camera))
    (translate-by (- x) (- y) (- z) (view-matrix))))

(defun do-fps-movement (camera old-pos pos)
  (let ((delta (v- pos old-pos)))
    (setf (vx delta) (* (vx delta) (x-acceleration camera)))
    (setf (vy delta) (* (vy delta) (y-acceleration camera)))
    (nv+ (rotation camera) (vyx_ delta))
    (nvmod (rotation camera) (* 2 PI))))

(define-handler (fps-camera mouse-move) (old-pos pos)
  (do-fps-movement fps-camera old-pos pos))

(defmethod focal-point ((camera fps-camera))
  ;; KLUDGE: not really accurate. Should be somewhere in front of the camera.
  (location camera))

(defclass freeroam-camera (fps-camera)
  ((move-speed :initarg :move-speed :initform 1.0 :accessor move-speed)))

(define-handler (freeroam-camera tick :after) ()
  (let* ((loc (location freeroam-camera))
         (rot (rotation freeroam-camera))
         (speed (* (move-speed freeroam-camera)
                   (if (retained :left-shift) 5 1)
                   (if (retained :left-alt) 1/5 1))))
    (cond ((retained :a)
           (decf (vx loc) (* speed (cos (vy rot))))
           (decf (vz loc) (* speed (sin (vy rot)))))
          ((retained :d)
           (incf (vx loc) (* speed (cos (vy rot))))
           (incf (vz loc) (* speed (sin (vy rot))))))
    (cond ((retained :w)
           (incf (vx loc) (* speed (sin (vy rot))))
           (decf (vz loc) (* speed (cos (vy rot))))
           (decf (vy loc) (* speed (sin (vx rot)))))
          ((retained :s)
           (decf (vx loc) (* speed (sin (vy rot))))
           (incf (vz loc) (* speed (cos (vy rot))))
           (incf (vy loc) (* speed (sin (vx rot))))))
    (cond ((retained :space)
           (incf (vy loc) speed))
          ((retained :c)
           (decf (vy loc) speed)))))

(defclass editor-camera (freeroam-camera)
  ())

(define-handler (editor-camera mouse-move) (old-pos pos)
  (when (or (retained :middle)
            (retained :left-control))
    (do-fps-movement editor-camera old-pos pos)))
