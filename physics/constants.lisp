(in-package #:org.shirakumo.fraf.trial)

(define-global +material-interaction-properties+ (make-hash-table :test 'equal))

(defstruct (material-interaction-properties
            (:constructor make-material-interaction-properties
                (a b &optional (static-friction 0.0) (dynamic-friction 0.0) (restitution 0.0)))
            (:copier NIL)
            (:predicate NIL))
  (a NIL :type T)
  (b NIL :type T)
  (static-friction 0.0 :type single-float)
  (dynamic-friction 0.0 :type single-float)
  (restitution 0.5 :type single-float))

(defmethod print-object ((properties material-interaction-properties) stream)
  (print-unreadable-object (properties stream :type T)
    (format stream "~a ~a ~s ~a ~s ~a ~s ~a"
            (material-interaction-properties-a properties)
            (material-interaction-properties-a properties)
            :static-friction (material-interaction-properties-static-friction properties)
            :dynamic-friction (material-interaction-properties-dynamic-friction properties)
            :restitution (material-interaction-properties-restitution properties))))

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

(defvar *default-material-interaction-properties*
  (make-material-interaction-properties NIL NIL 0.0 0.0 0.5))

(defun material-interaction-properties (a b)
  (if (not (and a b))
      *default-material-interaction-properties*
      (let* ((cons (cons a b)))
        (declare (dynamic-extent cons))
        (or (gethash cons +material-interaction-properties+)
            #+trial-release *default-material-interaction-properties*
            #-trial-release
            (restart-case (error "No material interaction properties known between ~a ~a" a b)
              (continue ()
                :report "Use the default"
                (setf (material-interaction-properties a b)
                      *default-material-interaction-properties*))
              (use-value (value)
                :report "Store a default"
                (check-type value material-interaction-properties)
                (setf (material-interaction-properties a b) value)))))))

(defun (setf material-interaction-properties) (properties a b)
  (setf (gethash (cons a b) +material-interaction-properties+) properties)
  (setf (gethash (cons b a) +material-interaction-properties+) properties))

(defun list-material-interaction-properties ()
  (delete-duplicates (loop for v being the hash-values of +material-interaction-properties+
                           collect v)))

(defun set-material-interaction-properties (&rest values)
  (loop for (a b f d r) on values by (lambda (l) (nthcdr 5 l))
        for props = (make-material-interaction-properties a b f d r)
        do (setf (material-interaction-properties a b) props)))

(set-material-interaction-properties
 ;;A--------------|B----------------|static friction--|dynamic friciton-|restitution------|
 :wood             :wood             0.5               0.5               0.6
 :wood             :concrete         0.5               0.4               0.6
 :wood             :ice              0.2               0.1               0.6
 :glass            :ice              0.1               0.03              0.6
 :glass            :glass            0.95              0.4               0.6
 :metal            :metal            0.6               0.4               0.6
 :lubricated-metal :metal            0.1               0.05              0.6
 :rubber           :concrete         1.0               0.8               0.6
 :wet-rubber       :concrete         0.7               0.5               0.6
 :tire             :concrete         1.5               1.0               0.6
 :velcro           :velcro           6.0               4.0               0.6)
