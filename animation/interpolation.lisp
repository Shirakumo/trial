#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.animation)

;; FIXME: implement modifying variant to avoid garbage production

(defmacro define-curve (name args &body body)
  `(defun ,name ,args
     (macrolet ((expand (type + * &optional (wrap 'progn))
                  `(locally
                       (declare (type ,type ,@',args))
                     (lambda (x)
                       (declare (optimize speed (safety 0)))
                       (declare (type single-float x))
                       (let* ((1-x (- 1.0 x)))
                         (,wrap ,,(first body)))))))
       (etypecase p1
         (real (let ,(loop for arg in args
                           collect `(,arg (float ,arg 0f0)))
                 (expand single-float + - *)))
         (quat
          (let ((p2 (if (< (q. p1 p2) 0) (q- p2) p2)))
            (expand quat nq+ q* nqunit)))
         (vec4 (expand vec4 nv+ v*))
         (vec3 (expand vec3 nv+ v*))
         (vec2 (expand vec2 nv+ v*))))))

(define-curve bezier (p1 c1 p2 c2)
  `(,+ (,* p1 (* 1-x 1-x 1-x))
       (,* c1 (* 3.0 x 1-x 1-x))
       (,* c2 (* 3.0 x x 1-x))
       (,* p2 (* x x x))))

(define-curve hermite (p1 s1 p2 s2)
  `(let ((xx (* x x))
         (xxx (* x x x)))
     (,+ (,* p1 (+ (* 2.0 xxx) (* -3.0 xx) 1.0))
         (,* p2 (+ (* -2.0 xxx) (* 3.0 xx)))
         (,* s1 (+ xxx (* -2.0 xx) x))
         (,* s2 (- xxx xx)))))

(define-curve linear (p1 p2)
  `(,+ (,* p1 1-x)
       (,* p2 x)))

(define-curve constant (p1)
  'p1)

(defgeneric interpolate (a b x))

#+sbcl
(sb-c:defknown interpolate (T T T) T)

(defmacro define-inlined-method (name args &body expansion)
  `(progn
     #+sbcl
     (sb-c:deftransform ,name (,(loop for arg in args collect (first arg))
                               ,(loop for arg in args collect (second arg)))
       ',(first expansion))

     (defmethod ,name ,args
       ,@expansion)))

(define-inlined-method interpolate ((a real) (b real) (x real))
  (let ((a (float a 0f0)))
    (+ a (* (- (float b 0f0) a) (float x 0f0)))))

(define-inlined-method interpolate ((a vec2) (b vec2) (x real))
  (let ((x (float x 0f0)))
    (vlerp a b x)))

(define-inlined-method interpolate ((a vec3) (b vec3) (x real))
  (let ((x (float x 0f0)))
    (vlerp a b x)))

(define-inlined-method interpolate ((a vec4) (b vec4) (x real))
  (let ((x (float x 0f0)))
    (vlerp a b x)))

(define-inlined-method interpolate ((a quat) (b quat) (x real))
  (let ((x (float x 0f0)))
    (vlerp a b x)))

(define-inlined-method interpolate ((a quat) (b quat) (x real))
  (let ((x (float x 0f0)))
    (nqunit (if (< (q. a b) 0)
                (qmix a (q- b) x)
                (qmix a b x)))))
