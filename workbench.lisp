(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0.3 0.3 0.3 0)))

(define-pool workbench
  :base 'trial)

(define-asset (workbench grid) mesh
    (make-line-grid 10 100 100))

(define-shader-subject grid (vertex-entity colored-entity)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'grid)))

(define-shader-subject simple-clock (lines) ()
  (:default-initargs :vertex-array (change-class (make-lines ()) 'vertex-array :data-usage :stream-draw)))

(define-handler (simple-clock tick) (ev tt)
  (let* ((h (mod (floor tt (* 60 60)) 24))
         (m (mod (floor tt 60) 60))
         (s (floor (mod tt 60)))
         (hrad (+ (* (/ h 24) -2 PI) (/ PI 2)))
         (mrad (+ (* (/ m 60) -2 PI) (/ PI 2)))
         (srad (+ (* (/ s 60) -2 PI) (/ PI 2)))
         (mesh (make-lines (list (vec 0 0 0) (vec (* (cos hrad) 70) (* (sin hrad) 70) 0)
                                 (vec 0 0 0) (vec (* (cos mrad) 90) (* (sin mrad) 90) 0)
                                 (list (vec 0 0 0) (vec 1 0 0 1)) (list (vec (* (cos srad) 90) (* (sin srad) 90) 0) (vec 1 0 0 1))))))
    (replace-vertex-data (vertex-array simple-clock) mesh :update T)))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'grid) scene)
    (enter (make-instance 'lines :vertex-array (asset 'trial 'axes)) scene)
    ;; Creating static lines
    (let ((circle (loop for i from 0 to (* 2 PI) by (/ PI 30)
                        collect (vec (* 100 (cos i)) (* 100 (sin i)) 0)
                        collect (vec (* 100 (cos (+ i (/ PI 30)))) (* 100 (sin (+ i (/ PI 30)))) 0))))
      (enter (make-instance 'lines :vertex-array (change-class (make-lines circle) 'vertex-array)
                                   :line-width 5.0)
             scene))
    ;; Creating lines that change
    (enter (make-instance 'simple-clock) scene)
    (enter (make-instance 'editor-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
