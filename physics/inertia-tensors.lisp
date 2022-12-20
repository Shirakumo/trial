(in-package #:org.shirakumo.fraf.trial)

(defmacro define-tensor-fun (name args &body body)
  `(defun ,name (,@args &optional (tensor (mat3)))
     (fill (marr3 m) 0.0)
     (with-fast-matref (m tensor 3)
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
    (let ((com (compute-center-of-mass particles))
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

(define-tensor-fun combine-tensor (tensor-a tensor-b distance)
  ;; TODO
  )

(define-tensor-fun cuboid-tensor (mass bsize)
  (let ((x2 (* (vx bsize) (vx bsize)))
        (y2 (* (vy bsize) (vy bsize)))
        (z2 (* (vz bsize) (vz bsize))))
    (setf (m 0 0) (* 1/12 mass (+ y2 z2)))
    (setf (m 1 1) (* 1/12 mass (+ x2 z2)))
    (setf (m 2 2) (* 1/12 mass (+ x2 y2)))))

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

(define-tensor-fun cylinder-tensor (mass radius height)
  (let* ((x (* 0.5 m radius radius))
         (y (* 0.5 (+ x (* 1/6 m (* height height))))))
    (setf (m 0 0) y)
    (setf (m 1 1) y)
    (setf (m 2 2) x)))

(define-tensor-fun cone-tensor (mass radius height)
  (let* ((x (float (* 3/10 m radius radius) 0f0))
         (y (float (* 0.5 (+ x (* 3/40 m (* height height)))) 0f0)))
    (setf (m 0 0) y)
    (setf (m 1 1) y)
    (setf (m 2 2) x)))
