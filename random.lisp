(in-package #:org.shirakumo.fraf.trial)

(defgeneric draw-instance (thing &rest initargs))
(defgeneric sample-volume (volume &optional vec))

(defmethod draw-instance ((name symbol) &rest args)
  (apply #'make-instance (find-class name) args))

(defmethod draw-instance ((class class) &rest args)
  (apply #'make-instance class args))

(defmethod draw-instance ((classes sequence) &rest args)
  (apply #'make-instance (alexandria:random-elt classes) args))

(defmethod draw-instance ((entity entity) &rest args)
  (apply #'clone entity args))

(defmethod draw-instance ((function function) &rest args)
  (apply function args))

(defmacro define-weighted-draw (name &body items)
  `(defmethod draw-instance ((name (eql ',name)) &rest args)
     ;; TODO: this could be faster via a binsearch, but honestly it's probably not worth it
     ;;       these lists will hardly be hundreds of elements long.
     (let ((r (random ,(float (loop for (item weight) in items sum weight) 0f0)))
           (i (cond ,@(nreverse (loop for prev = 0.0 then (+ prev weight)
                                      for (item weight) in items
                                      collect `((< ,prev r) ',item))))))
       (apply #'draw-instance i args))))

;; Random is imported from random-state, default init to a squirrel hash based RNG.
(when (eq random:*generator* *random-state*)
  (setf random:*generator* (random:global-generator :squirrel)))
