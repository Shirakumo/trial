#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defgeneric compute-resources (object traversal-cache))
(defgeneric bake (bakable))
(defgeneric baked-p (bakable))
(defgeneric transition (from to))
(defgeneric dependencies (resource))

(defmethod dependencies ((resource resource))
  ())

;; FIXME: Consider using a vector to compute the resources instead.
;;        A large vector could easily become more efficient than a
;;        list, especially considering we are mostly APPENDing things.
(defmethod compute-resources :around (object (cache null))
  (compute-resources object (make-hash-table :test 'eq)))

(defmethod compute-resources :around (object (cache hash-table))
  (unless (gethash object cache)
    (setf (gethash object cache) T)
    (call-next-method)))

(defmethod compute-resources ((anything T) cache)
  NIL)

(defmethod compute-resources ((cons cons) cache)
  (nconc (compute-resources (car cons) cache)
         (compute-resources (cdr cons) cache)))

(defmethod compute-resources ((vector vector) cache)
  (unless (typep vector 'string)
    (loop for object across vector
          nconc (compute-resources object cache))))

(defmethod compute-resources ((table hash-table) cache)
  (loop for value being the hash-values of table
        nconc (compute-resources value cache)))

(defmethod compute-resources ((object entity) cache)
  (loop for slot in (c2mop:class-slots (class-of object))
        for name = (c2mop:slot-definition-name slot)
        when (slot-boundp object name)
        nconc (compute-resources (slot-value object name) cache)))

(defmethod compute-resources ((resource resource) cache)
  (nconc (call-next-method)
         (dependencies resource)
         (list resource)))

(defclass bakable ()
  ((baked-p :initform NIL :accessor baked-p)))

(defmethod compute-resources :before ((bakable bakable) cache)
  (bake bakable))

(defmethod bake :around ((bakable bakable))
  (unless (baked-p bakable)
    (call-next-method))
  (setf (baked-p bakable) T))

(defmethod transition ((from null) (to scene))
  (v:info :trial.loader "Transitioning to ~a" to)
  (let ((to-load (topological-sort-by-dependencies (compute-resources to NIL))))
    (v:info :trial.loader "Loading ~a assets." (length to-load))
    (v:debug :trial.loader "Loading:~%~a" to-load)
    (mapc #'load to-load)
    to))

(defmethod transition ((from scene) (to null))
  (v:info :trial.loader "Transitioning from ~a" from)
  (let ((to-deallocate (compute-resources to NIL)))
    (v:info :trial.loader "Deallocating ~a assets." (length to-deallocate))
    (v:debug :trial.loader "Deallocating:~%~a" to-deallocate)
    (mapc #'deallocate to-deallocate)
    to))

(defun stable-set-difference-eq (a b)
  (let ((table (make-hash-table :test 'eq)))
    (dolist (item b) (setf (gethash item table) T))
    (remove-if (lambda (item) (gethash item table)) a)))

(defun topological-sort-by-dependencies (resources)
  (let ((status (make-hash-table :test 'eq))
        (sorted ()))
    (labels ((visit (resource)
               (case (gethash resource status :unvisited)
                 (:temporary
                  (warn "Dependency loop detected on ~a." resource))
                 (:unvisited
                  (setf (gethash resource status) :temporary)
                  (dolist (dependency (dependencies resource))
                    ;; Avoid injecting dependencies that are not part of the
                    ;; resource loading list to avoid duplicate loading.
                    (when (find dependency resources)
                      (visit dependency)))
                  (setf (gethash resource status) :done)
                  (push resource sorted)))))
      (mapc #'visit resources))
    (nreverse sorted)))

(defmethod transition ((from scene) (to scene))
  (v:info :trial.loader "Transitioning from ~a to ~a" from to)
  (let* ((from (compute-resources from NIL))
         (to (compute-resources to NIL))
         (to-load (topological-sort-by-dependencies (stable-set-difference-eq to from)))
         (to-deallocate (stable-set-difference-eq from to)))
    (v:info :trial.loader "Loading ~a asset~:p." (length to-load))
    (v:debug :trial.loader "Loading:~%~a" to-load)
    (mapc #'load to-load)
    (v:info :trial.loader "Deallocating ~a asset~:p." (length to-deallocate))
    (v:debug :trial.loader "Deallocating:~%~a" to-deallocate)
    (mapc #'deallocate to-deallocate)
    to))
