(in-package #:org.shirakumo.fraf.trial)

(define-global +static-frictions+ (make-hash-table :test 'equal))
(define-global +dynamic-frictions+ (make-hash-table :test 'equal))

(defun static-friction (a b)
  (let* ((cons (cons a b)))
    (declare (dynamic-extent cons))
    (or (gethash cons +static-frictions+)
        #+trial-release 0.0
        #-trial-release
        (restart-case (error "No static friction known for ~a" cons)
          (continue ()
            :report "Ignore the friction"
            (setf (static-friction a b) 0.0))
          (use-value (value)
            :report "Store a friction"
            (setf (static-friction a b) value))))))

(defun (setf static-friction) (friction a b)
  (setf (gethash (cons a b) +static-frictions+) (float friction 0f0))
  (setf (gethash (cons b a) +static-frictions+) (float friction 0f0)))

(defun dynamic-friction (a b)
  (let* ((cons (cons a b)))
    (declare (dynamic-extent cons))
    (or (gethash cons +dynamic-frictions+)
        #+trial-release 0.0
        #-trial-release
        (restart-case (error "No dynamic friction known for ~a" cons)
          (continue ()
            :report "Ignore the friction"
            (setf (dynamic-friction a b) 0.0))
          (use-value (value)
            :report "Store a friction"
            (setf (dynamic-friction a b) value))))))

(defun (setf dynamic-friction) (friction a b)
  (setf (gethash (cons a b) +dynamic-frictions+) (float friction 0f0))
  (setf (gethash (cons b a) +dynamic-frictions+) (float friction 0f0)))

(defun set-friction-values (&rest values)
  (loop for (a b static dynamic) on values by #'cddddr
        do (setf (static-friction a b) static)
           (setf (dynamic-friction a b) dynamic)))

(set-friction-values
 :wood             :wood          0.5  0.5
 :wood             :concrete      0.5  0.4
 :wood             :ice           0.2  0.1
 :glass            :ice           0.1  0.03
 :glass            :glass         0.95 0.4
 :metal            :metal         0.6  0.4
 :lubricated-metal :metal         0.1  0.05
 :rubber           :concrete      1.0  0.8
 :wet-rubber       :concrete      0.7  0.5
 :tire             :concrete      1.5  1.0
 :velcro           :velcro        6.0  4.0)
