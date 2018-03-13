#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defgeneric compute-resources (object resource-vector readying-vector traversal-cache))
(defgeneric bake (bakable))
(defgeneric baked-p (bakable))
(defgeneric resources-ready (readying))
(defgeneric transition (from to))
(defgeneric dependencies (resource))

(defun compute-resources-for (thing)
  (let ((resources (make-array 0 :adjustable T :fill-pointer T))
        (readying (make-array 0 :adjustable T :fill-pointer T)))
    (compute-resources thing resources readying (make-hash-table :test 'eq))
    (values resources readying)))

(defmethod dependencies ((resource resource))
  ())

(defmethod compute-resources :around (object resources readying (cache hash-table))
  (unless (gethash object cache)
    (setf (gethash object cache) T)
    (call-next-method)))

(defmethod compute-resources ((anything T) resources readying cache))

(defmethod compute-resources ((cons cons) resources readying cache)
  (compute-resources (car cons) resources readying cache)
  (compute-resources (cdr cons) resources readying cache))

(defmethod compute-resources ((vector vector) resources readying cache)
  (unless (typep vector 'string)
    (loop for object across vector
          do (compute-resources object resources readying cache))))

(defmethod compute-resources ((table hash-table) resources readying cache)
  (loop for value being the hash-values of table
        do (compute-resources value resources readying cache)))

(defmethod compute-resources ((object entity) resources readying cache)
  (loop for slot in (c2mop:class-slots (class-of object))
        for name = (c2mop:slot-definition-name slot)
        when (slot-boundp object name)
        do (compute-resources (slot-value object name) resources readying cache)))

(defmethod compute-resources ((resource resource) resources readying cache)
  (call-next-method)
  (vector-push-extend resource resources)
  (dolist (dep (dependencies resource))
    (compute-resources dep resources readying cache)))

(defclass bakable ()
  ((baked-p :initform NIL :accessor baked-p)))

(defmethod compute-resources :before ((bakable bakable) resources readying cache)
  (bake bakable))

(defmethod bake :around ((bakable bakable))
  (unless (baked-p bakable)
    (call-next-method))
  (setf (baked-p bakable) T))

(defclass readied ()
  ())

(defmethod compute-resources :before ((readied readied) resources readying cache)
  (vector-push-extend readied readying))

(defun stable-set-difference-eq (a b)
  (let ((table (make-hash-table :test 'eq :size (length b))))
    (loop for item across b do (setf (gethash item table) T))
    (remove-if (lambda (item) (gethash item table)) a)))

(defun topological-sort-by-dependencies (resources)
  (let ((status (make-hash-table :test 'eq))
        (sorted (make-array (length resources) :fill-pointer 0)))
    (labels ((visit (resource)
               (case (gethash resource status :unvisited)
                 (:temporary
                  (warn "Dependency loop detected on ~a." resource))
                 (:unvisited
                  (setf (gethash resource status) :temporary)
                  (dolist (dependency (dependencies resource))
                    ;; Avoid injecting dependencies that are not part of the
                    ;; resource loading list to avoid duplicate loading.
                    ;; FIXME: maybe use the cache from the traversal for quicker lookup.
                    (when (find dependency resources)
                      (visit dependency)))
                  (setf (gethash resource status) :done)
                  (vector-push resource sorted)))))
      (map NIL #'visit resources))
    sorted))

(defun %transition (to-load to-deallocate to-ready)
  (when to-load
    (let ((to-load (topological-sort-by-dependencies to-load)))
      (v:info :trial.loader "Loading ~a asset~:p." (length to-load))
      (v:debug :trial.loader "Loading:~%~a" to-load)
      (map NIL #'load to-load)))
  (when to-ready
    (map NIL #'resources-ready to-ready))
  (when to-deallocate
    (v:info :trial.loader "Deallocating ~a asset~:p." (length to-deallocate))
    (v:debug :trial.loader "Deallocating:~%~a" to-deallocate)
    (map NIL #'deallocate to-deallocate)))

(defmethod transition ((from entity) (to scene))
  (v:info :trial.loader "Transitioning ~a into ~a." from to)
  (multiple-value-bind (to-load to-ready) (compute-resources-for from)
    (%transition to-load NIL to-ready)
    to))

(defmethod transition ((from null) (to scene))
  (v:info :trial.loader "Transitioning to ~a" to)
  (multiple-value-bind (to-load to-ready) (compute-resources-for to)
    (%transition to-load NIL to-ready)
    to))

(defmethod transition ((from scene) (to null))
  (v:info :trial.loader "Transitioning from ~a" from)
  (let ((to-deallocate (compute-resources-for to)))
    (%transition NIL to-deallocate NIL)
    to))

(defmethod transition ((from scene) (to scene))
  (v:info :trial.loader "Transitioning from ~a to ~a" from to)
  (multiple-value-bind (to to-ready) (compute-resources-for to)
    (let* ((from (compute-resources-for from))
           (to-load (stable-set-difference-eq to from))
           (to-deallocate (stable-set-difference-eq from to)))
      (%transition to-load to-deallocate to-ready)
      to)))
