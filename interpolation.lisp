(in-package #:org.shirakumo.fraf.trial)

(defmacro define-curve (name args &body body)
  (flet ((expand-real ()
           `(lambda (target x)
              (declare (optimize speed (safety 0)))
              (declare (type single-float x))
              (declare (ignore target))
              (let* ((1-x (- 1.0 x)))
                (declare (ignorable 1-x))
                ,@body)))
         (expand-vec (result type &rest accs)
           `(locally
                (declare (type ,type ,@args))
              (lambda (target x)
                (declare (optimize speed (safety 0)))
                (declare (type single-float x))
                (declare (type ,type target))
                (let* ((1-x (- 1.0 x)))
                  (declare (ignorable 1-x))
                  ,@(loop for acc in accs
                          collect `(let ,(loop for arg in args
                                               collect `(,arg (,acc ,arg)))
                                     (setf (,acc target) ,@body)))
                  ,result)))))
    `(defun ,name ,args
       (etypecase p1
         (real (let ,(loop for arg in args
                           collect `(,arg (float ,arg 0f0)))
                 ,(expand-real)))
         ((simple-array single-float (*))
          (let ((len (length p1)))
            (declare (type (simple-array single-float (*)) ,@args))
            (lambda (target x)
              (declare (optimize speed (safety 0)))
              (declare (type single-float x))
              (declare (type (simple-array single-float (*)) target))
              (loop with 1-x = (- 1.0 x)
                    for i from 0 below len
                    do (setf (aref target i)
                             (let ,(loop for arg in args
                                         collect `(,arg (aref ,arg i)))
                               ,@body)))
              target)))
         (quat
          ,(if (rest args)
               `(let ((p2 (if (< (q. p1 p2) 0) (q- p2) p2)))
                  ,(expand-vec '(nqunit* target) 'quat 'qx 'qy 'qz 'qw))
               (expand-vec '(nqunit* target) 'quat 'qx 'qy 'qz 'qw)))
         (vec4 ,(expand-vec 'target 'vec4 'vx4 'vy4 'vz4 'vw4))
         (vec3 ,(expand-vec 'target 'vec3 'vx3 'vy3 'vz3))
         (vec2 ,(expand-vec 'target 'vec2 'vx2 'vy2))))))

(define-curve bezier (p1 c1 p2 c2)
  (+ (* p1 (* 1-x 1-x 1-x))
     (* c1 (* 3.0 x 1-x 1-x))
     (* c2 (* 3.0 x x 1-x))
     (* p2 (* x x x))))

(define-curve hermite (p1 s1 p2 s2)
  (let ((xx (* x x))
        (xxx (* x x x)))
    (+ (* p1 (+ (* 2.0 xxx) (* -3.0 xx) 1.0))
       (* p2 (+ (* -2.0 xxx) (* 3.0 xx)))
       (* s1 (+ xxx (* -2.0 xx) x))
       (* s2 (- xxx xx)))))

(define-curve linear (p1 p2)
  (+ (* p1 1-x)
     (* p2 x)))

(define-curve constant (p1)
  p1)

(defgeneric interpolate (a b x))
(defgeneric ninterpolate (dst a b x))

#+sbcl
(progn
  (sb-c:defknown interpolate (T T T) T (sb-c:any) :overwrite-fndb-silently T)
  (sb-c:defknown ninterpolate (T T T T) T (sb-c:any) :overwrite-fndb-silently T))

(defmacro define-inlined-method (name args &body expansion)
  `(progn
     #+sbcl
     (sb-c:deftransform ,name (,(loop for arg in args collect (first arg))
                               ,(loop for arg in args collect (second arg)))
       ',(first expansion))

     (defmethod ,name ,args
       ,@expansion)))

(declaim (inline lerp))
(defun lerp (a b x)
  (+ (* a (- 1.0 x)) (* b x)))

(define-inlined-method ninterpolate ((dst T) (a real) (b real) (x real))
  (declare (ignore dst))
  (let ((a (float a 0f0)))
    (+ a (* (- (float b 0f0) a) (float x 0f0)))))

(define-inlined-method interpolate ((a real) (b real) (x real))
  (let ((a (float a 0f0)))
    (+ a (* (- (float b 0f0) a) (float x 0f0)))))

(define-inlined-method ninterpolate ((dst vec2) (a vec2) (b vec2) (x real))
  (let ((x (float x 0f0)))
    (setf (vx2 dst) (lerp (vx2 a) (vx2 b) x))
    (setf (vy2 dst) (lerp (vy2 a) (vy2 b) x))
    dst))

(define-inlined-method interpolate ((a vec2) (b vec2) (x real))
  (ninterpolate (vec2 0 0) a b x))

(define-inlined-method ninterpolate ((dst vec3) (a vec3) (b vec3) (x real))
  (let ((x (float x 0f0)))
    (setf (vx3 dst) (lerp (vx3 a) (vx3 b) x))
    (setf (vy3 dst) (lerp (vy3 a) (vy3 b) x))
    (setf (vz3 dst) (lerp (vz3 a) (vz3 b) x))
    dst))

(define-inlined-method interpolate ((a vec3) (b vec3) (x real))
  (ninterpolate (vec3 0 0 0) a b x))

(define-inlined-method ninterpolate ((dst vec4) (a vec4) (b vec4) (x real))
  (let ((x (float x 0f0)))
    (setf (vx4 dst) (lerp (vx4 a) (vx4 b) x))
    (setf (vy4 dst) (lerp (vy4 a) (vy4 b) x))
    (setf (vz4 dst) (lerp (vz4 a) (vz4 b) x))
    (setf (vw4 dst) (lerp (vw4 a) (vw4 b) x))
    dst))

(define-inlined-method interpolate ((a vec4) (b vec4) (x real))
  (ninterpolate (vec4 0 0 0 0) a b x))

(define-inlined-method ninterpolate ((dst quat) (a quat) (b quat) (x real))
  (let ((x (float x 0f0))
        (lhs (quat))
        (rhs (quat)))
    (declare (dynamic-extent lhs rhs))
    (q<- lhs a)
    (q<- rhs b)
    (when (< (q. a b) 0)
      (nq- rhs))
    (nq* lhs (- 1.0 x))
    (nq* rhs x)
    (qsetf dst
           (+ (qx lhs) (qx rhs))
           (+ (qy lhs) (qy rhs))
           (+ (qz lhs) (qz rhs))
           (+ (qw lhs) (qw rhs)))
    (nqunit* dst)))

(define-inlined-method interpolate ((a quat) (b quat) (x real))
  (ninterpolate (quat) a b x))

(define-inlined-method ninterpolate ((dst transform) (a transform) (b transform) (x real))
  (let ((x (float x 0f0)))
    (ninterpolate (tlocation dst) (tlocation a) (tlocation b) x)
    (ninterpolate (tscaling dst) (tscaling a) (tscaling b) x)
    (ninterpolate (trotation dst) (trotation a) (trotation b) x)
    dst))

(define-inlined-method interpolate ((a transform) (b transform) (x real))
  (ninterpolate (transform) a b x))
