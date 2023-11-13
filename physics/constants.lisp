(in-package #:org.shirakumo.fraf.trial)

(define-global +material-interaction-properties+ (make-hash-table :test 'equal))

(defstruct (material-interaction-properties
            (:constructor make-material-interaction-properties
                (a b &optional (static-friction 0.0) (dynamic-friction 0.0) (restitution 0.5) (friction-combine :average) (restitution-combine :average)))
            (:copier NIL)
            (:predicate NIL))
  (a NIL :type T)
  (b NIL :type T)
  (static-friction 0.0 :type single-float)
  (dynamic-friction 0.0 :type single-float)
  (restitution 0.5 :type single-float)
  (friction-combine :average :type keyword)
  (restitution-combine :average :type keyword))

(defmethod print-object ((properties material-interaction-properties) stream)
  (print-unreadable-object (properties stream :type T)
    (format stream "~a ~a ~s ~a ~s ~a ~s ~a ~s ~a ~s ~a"
            (material-interaction-properties-a properties)
            (material-interaction-properties-a properties)
            :static-friction (material-interaction-properties-static-friction properties)
            :dynamic-friction (material-interaction-properties-dynamic-friction properties)
            :restitution (material-interaction-properties-restitution properties)
            :friction-combine (material-interaction-properties-friction-combine properties)
            :restitution-combine (material-interaction-properties-restitution-combine properties))))

(define-accessor-delegate-methods a
  (material-interaction-properties-a material-interaction-properties))
(define-accessor-delegate-methods b
  (material-interaction-properties-b material-interaction-properties))
(define-accessor-delegate-methods static-friction
  (material-interaction-properties-static-friction material-interaction-properties))
(define-accessor-delegate-methods dynamic-friction
  (material-interaction-properties-dynamic-friction material-interaction-properties))
(define-accessor-delegate-methods restitution
  (material-interaction-properties-restitution material-interaction-properties))
(define-accessor-delegate-methods friction-combine
  (material-interaction-properties-friction-combine material-interaction-properties))
(define-accessor-delegate-methods restitution-combine
  (material-interaction-properties-restitution-combine material-interaction-properties))

(defvar *default-material-interaction-properties*
  (make-material-interaction-properties NIL NIL 0.0 0.0 0.5))

(declaim (ftype (function (material-interaction-properties material-interaction-properties)
                          (values single-float single-float single-float &optional))
                combine-material-interaction-properties))
(defun combine-material-interaction-properties (a b)
  (if (eq a b)
      (values (material-interaction-properties-static-friction a)
              (material-interaction-properties-dynamic-friction a)
              (material-interaction-properties-restitution a))
      (flet ((combine-function (a b)
               (cond ((or (eql :average a) (eql :average b))
                      (lambda (a b) (* 0.5 (+ a b))))
                     ((or (eql :minimum a) (eql :minimum b))
                      (lambda (a b) (min a b)))
                     ((or (eql :maximum a) (eql :maximum b))
                      (lambda (a b) (max a b)))
                     ((or (eql :multiply a) (eql :multiply b))
                      (lambda (a b) (* a b)))
                     (T
                      (error "Unknown combine types: ~a and ~a" a b)))))
        (values (funcall (combine-function (material-interaction-properties-friction-combine a)
                                           (material-interaction-properties-friction-combine b))
                         (material-interaction-properties-static-friction a)
                         (material-interaction-properties-static-friction b))
                (funcall (combine-function (material-interaction-properties-friction-combine a)
                                           (material-interaction-properties-friction-combine b))
                         (material-interaction-properties-dynamic-friction a)
                         (material-interaction-properties-dynamic-friction b))
                (funcall (combine-function (material-interaction-properties-restitution-combine a)
                                           (material-interaction-properties-restitution-combine b))
                         (material-interaction-properties-restitution a)
                         (material-interaction-properties-restitution b))))))

(declaim (ftype (function ((or symbol material-interaction-properties) &optional (or symbol material-interaction-properties))
                          (values single-float single-float single-float &optional))
                material-interaction-properties))
(defun material-interaction-properties (a &optional b)
  (declare (optimize speed))
  (flet ((normalize-properties (prop)
           (etypecase prop
             (null *default-material-interaction-properties*)
             (material-interaction-properties prop)
             (T (gethash prop +material-interaction-properties+ *default-material-interaction-properties*))))
         (finish (prop)
           (values (material-interaction-properties-static-friction prop)
                   (material-interaction-properties-dynamic-friction prop)
                   (material-interaction-properties-restitution prop))))
    (let* ((cons (cons a b))
           (prop (gethash cons +material-interaction-properties+)))
      (declare (dynamic-extent cons))
      (if prop
          (finish prop)
          (combine-material-interaction-properties (normalize-properties a) (normalize-properties b))))))

(defun (setf material-interaction-properties) (properties a &optional b)
  (let ((properties (etypecase properties
                      (material-interaction-properties properties)
                      (cons (apply #'make-material-interaction-properties a b properties)))))
    (cond (b
           (setf (gethash (cons a b) +material-interaction-properties+) properties)
           (setf (gethash (cons b a) +material-interaction-properties+) properties))
          (T
           (setf (gethash a +material-interaction-properties+) properties)))))

(defun list-material-interaction-properties ()
  (delete-duplicates (loop for v being the hash-values of +material-interaction-properties+
                           collect v)))

(defun set-material-interaction-properties (values)
  (loop for (ab . args) in values
        do (if (listp ab)
               (destructuring-bind (a b) ab
                 (setf (material-interaction-properties a b) (apply #'make-material-interaction-properties a b args)))
               (setf (material-interaction-properties ab) (apply #'make-material-interaction-properties ab NIL args)))))

(set-material-interaction-properties
 '(;;A-----------------|B----------------|static friction--|dynamic friciton-|restitution------|
   ((:wood             :wood)             0.5               0.5               0.3)
   ((:wood             :concrete)         0.5               0.4               0.5)
   ((:wood             :ice)              0.2               0.1               0.3)
   ((:ice              :ice)              0.1               0.03              0.4)
   ((:glass            :ice)              0.1               0.03              0.4)
   ((:glass            :glass)            0.95              0.4               0.4)
   ((:metal            :metal)            0.6               0.4               0.4)
   ((:lubricated-metal :metal)            0.1               0.05              0.4)
   ((:rubber           :concrete)         1.0               0.8               0.9)
   ((:wet-rubber       :concrete)         0.7               0.5               0.85)
   ((:tire             :concrete)         1.5               1.0               0.8)
   ((:velcro           :velcro)           6.0               4.0               0.0)))
