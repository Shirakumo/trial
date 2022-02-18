#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.animation)

;; FIXME: implement modifying variant to avoid garbage production

(defmacro define-curve (name args &body expansion)
  (destructuring-bind (body &optional quat) expansion
    `(defun ,name ,args
       (macrolet ((expand (type + - * &optional (wrap 'progn))
                    `(locally
                         (declare (type ,type ,@',args))
                       (lambda (x)
                         (declare (optimize speed (safety 0)))
                         (let* ((x (float x 0f0))
                                (1-x (- 1 x)))
                           (declare (ignorable x 1-x))
                           (,wrap ,,body))))))
         (etypecase p1
           (real (let ,(loop for arg in args
                             collect `(,arg (float ,arg 0f0)))
                   (expand single-float + - *)))
           (quat
            (let ,quat
              (expand quat q+ q- q* nqunit)))
           (vec4 (expand vec4 v+ v- v*))
           (vec3 (expand vec3 v+ v- v*))
           (vec2 (expand vec2 v+ v- v*)))))))

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
         (,* s2 (- xxx xx))))
  ((p2 (if (< (q. p1 p2) 0) (q- p2) p2))))

(define-curve linear (p1 p2)
  `(,+ (,* p1 x)
       (,* p2 1-x)))

(define-curve constant (p1)
  'p1)

(defgeneric interpolate (a b x))

(defmethod interpolate ((a single-float) (b single-float) x)
  (+ a (* (- b a) (the single-float x))))

(defmethod interpolate ((a vec2) (b vec2) x)
  (vlerp a b x))

(defmethod interpolate ((a vec3) (b vec3) x)
  (vlerp a b x))

(defmethod interpolate ((a vec4) (b vec4) x)
  (vlerp a b x))

(defmethod interpolate ((a quat) (b quat) x)
  (nqunit (if (< (q. a b) 0)
              (qmix a (q- b) x)
              (qmix a b x))))
