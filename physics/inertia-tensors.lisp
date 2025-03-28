(in-package #:org.shirakumo.fraf.trial)

(defmacro define-tensor-fun (name args &body body)
  `(defun ,name (,@args ,@(unless (find '&optional args) '(&optional)) (tensor (mat3)))
     (declare (type mat3 tensor))
     (fill (marr3 tensor) 0.0)
     (with-fast-matref (m tensor)
       ,@body
       tensor)))

(defun compute-center-of-mass (particles &optional (com (vec 0 0 0)))
  (nv/ com
       (loop for particle across particles
             do (nv+* com (location particle) (mass particle))
             sum (mass particle))))

(define-tensor-fun mass-aggregate-tensor (particles)
  (macrolet ((s (a b)
               `(loop for particle across particles
                      sum (* (mass particle)
                             (- (,a (location particle)) (,a com))
                             (- (,b (location particle)) (,b com))))))
    (let* ((com (compute-center-of-mass particles))
           (ixy (- (s vx vy)))
           (ixz (- (s vx vz)))
           (iyz (- (s vy vz))))
      (setf (m 0 0) (s vx vx))
      (setf (m 0 1) ixy)
      (setf (m 0 2) ixz)
      (setf (m 1 0) ixy)
      (setf (m 1 1) (s vy vy))
      (setf (m 1 2) iyz)
      (setf (m 2 0) ixz)
      (setf (m 2 1) iyz)
      (setf (m 2 2) (s vz vz)))))

(define-tensor-fun voxel-tensor (mass voxel-grid)
  (macrolet ((s (a b)
               `(* pm
                   (loop for z from 0 below (array-dimension voxel-grid 0)
                         sum (loop for y from 0 below (array-dimension voxel-grid 1)
                                   sum (loop for x from 0 below (array-dimension voxel-grid 2)
                                             when (< 0 (aref voxel-grid z y x))
                                             sum (float (* ,a ,b) 0f0)))))))
    (let* ((pm (/ (float mass 0f0)
                  (loop for i from 0 below (array-total-size voxel-grid)
                        count (< 0 (row-major-aref voxel-grid i)))))
           (ixy (- (s x y)))
           (ixz (- (s x z)))
           (iyz (- (s y z))))
      (setf (m 0 0) (s x x))
      (setf (m 0 1) ixy)
      (setf (m 0 2) ixz)
      (setf (m 1 0) ixy)
      (setf (m 1 1) (s y y))
      (setf (m 1 2) iyz)
      (setf (m 2 0) ixz)
      (setf (m 2 1) iyz)
      (setf (m 2 2) (s z z)))))

(define-tensor-fun combine-tensor (tensor-a tensor-b distance)
  ;; TODO
  (implement!))

(define-tensor-fun box-tensor (mass bsize)
  (let ((x2 (* (vx bsize) (vx bsize)))
        (y2 (* (vy bsize) (vy bsize)))
        (z2 (* (vz bsize) (vz bsize))))
    (setf (m 0 0) (* 0.3 mass (+ y2 z2)))
    (setf (m 1 1) (* 0.3 mass (+ x2 z2)))
    (setf (m 2 2) (* 0.3 mass (+ x2 y2)))))

(define-tensor-fun sphere-tensor (mass radius)
  (let ((x (float (* 2/5 mass radius radius) 0f0)))
    (setf (m 0 0) x)
    (setf (m 1 1) x)
    (setf (m 2 2) x)))

(define-tensor-fun shell-tensor (mass radius)
  (let ((x (float (* 2/3 mass radius radius) 0f0)))
    (setf (m 0 0) x)
    (setf (m 1 1) x)
    (setf (m 2 2) x)))

(define-tensor-fun ellipsoid-tensor (mass radius)
  (let ((x (/ (* 3.0 mass) 5.0)))
    (setf (m 0 0) (* x (+ (vy radius) (vz radius))))
    (setf (m 1 1) (* x (+ (vx radius) (vz radius))))
    (setf (m 2 2) (* x (+ (vx radius) (vy radius))))))

(define-tensor-fun cone-tensor (mass radius height)
  (let* ((x (float (* 3/10 mass radius radius) 0f0))
         (y (float (* 0.5 (+ x (* 3/40 mass (* height height)))) 0f0)))
    (setf (m 0 0) y)
    (setf (m 1 1) y)
    (setf (m 2 2) x)))

(define-tensor-fun tube-tensor (mass radius height inner-radius)
  (let* ((r (+ (* radius radius) (* inner-radius inner-radius)))
         (x (float (* 1/2 mass r) 0f0))
         (y (float (* 1/12 mass (+ (* height height) (* 3 r))) 0f0)))
    (setf (m 0 0) y)
    (setf (m 1 1) y)
    (setf (m 2 2) x)))

(define-tensor-fun dome-tensor (mass width height depth)
  (let* ((a (float width 0f0))
         (b (float depth 0f0))
         (c (float height 0f0))
         (x (* 19/320 mass c c))
         (y (* 1/5 mass b b))
         (z (* 1/5 mass a a)))
    (setf (m 0 0) (+ y x))
    (setf (m 1 1) (+ z x))
    (setf (m 2 2) (+ y z))))

(define-tensor-fun cylinder-tensor (mass radius-bottom radius-top height)
  ;; FIXME: Correct the tensor computation for varying radii
  (let* ((rb (float radius-bottom 0f0))
         (rt (float radius-top 0f0))
         (r (max rb rt))
         (x (* 0.5 mass r r))
         (y (* 0.5 (+ x (* 1/6 mass (* height height))))))
    (setf (m 0 0) y)
    (setf (m 1 1) y)
    (setf (m 2 2) x)))

(define-tensor-fun pill-tensor (mass radius-bottom radius-top height)
  ;; FIXME: Correct the tensor computation for varying radii
  (let* ((rb (float radius-bottom 0f0))
         (rt (float radius-top 0f0))
         (r (max rb rt))
         (h (float height 0f0))
         (mc (* mass (/ h (* 4/3 r))))
         (md (- mass mc))
         (x (* 2/5 md 2 r r))
         (y (+ (* 3/4 md h r) (* 1/2 md h h) (* 1/4 mc r r) (* 1/12 mc h h))))
    (setf (m 0 0) (+ x y))
    (setf (m 1 1) (+ x y))
    (setf (m 2 2) (+ x (* 1/4 mc 2 r r)))))

(define-tensor-fun mesh-tensor (mass vertices faces)
  (voxel-tensor mass (org.shirakumo.fraf.manifolds:voxelize vertices faces :grid 0.1) tensor))

(define-tensor-fun diagonal-tensor (diagonal &optional (orientation (quat)))
  (declare (type quat orientation))
  (let ((r (qmat orientation)))
    (loop for e across diagonal
          for r from 0
          do (setf (m r r) e))
    ;; I = R * D * R^-1
    (!m* tensor tensor (mtranspose r))
    (!m* tensor r tensor)))
