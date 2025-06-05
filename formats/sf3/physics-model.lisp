(in-package #:org.shirakumo.fraf.trial)

(defmethod load-physics-model ((target rigid-shape) source &rest args)
  (flet ((translate (shape)
           (let ((mat (mat4 (org.shirakumo.sf3:transform shape))))
             (etypecase shape
               (org.shirakumo.sf3:ellipsoid
                (if (= (org.shirakumo.sf3:width shape)
                       (org.shirakumo.sf3:height shape)
                       (org.shirakumo.sf3:depth shape))
                    (make-sphere :radius (org.shirakumo.sf3:width shape)
                                 :local-transform mat)
                    (make-ellipsoid :radius (vec3 (org.shirakumo.sf3:width shape)
                                                  (org.shirakumo.sf3:height shape)
                                                  (org.shirakumo.sf3:depth shape))
                                    :local-transform mat)))
               (org.shirakumo.sf3:box
                (make-box :bsize (vec3 (org.shirakumo.sf3:width shape)
                                       (org.shirakumo.sf3:height shape)
                                       (org.shirakumo.sf3:depth shape))
                          :local-transform mat))
               (org.shirakumo.sf3:cylinder
                (if (= 0 (org.shirakumo.sf3:top-radius shape))
                    (make-cone :radius (org.shirakumo.sf3:bottom-radius shape)
                               :height (org.shirakumo.sf3:height shape)
                               :local-transform mat)
                    (make-cylinder :radius-bottom (org.shirakumo.sf3:bottom-radius shape)
                                   :radius-top (org.shirakumo.sf3:top-radius shape)
                                   :height (org.shirakumo.sf3:height shape)
                                   :local-transform mat)))
               (org.shirakumo.sf3:pill
                (make-pill :radius-bottom (org.shirakumo.sf3:bottom-radius shape)
                           :radius-top (org.shirakumo.sf3:top-radius shape)
                           :height (org.shirakumo.sf3:height shape)
                           :local-transform mat))
               (org.shirakumo.sf3:mesh
                (multiple-value-bind (vertices faces)
                    (org.shirakumo.fraf.quickhull:convex-hull (org.shirakumo.sf3:vertices shape))
                  (make-convex-mesh :vertices vertices
                                    :faces faces
                                    :local-transform mat)))))))
    (let ((sf3 (apply #'org.shirakumo.sf3:read-sf3 source args)))
      (check-type sf3 org.shirakumo.sf3:physics-model)
      (setf (mass target) (org.shirakumo.sf3:mass sf3))
      (setf (inertia-tensor target) (mat3 (org.shirakumo.sf3:tensor sf3)))
      (setf (physics-primitives target)
            (map 'vector #'translate (org.shirakumo.sf3:shapes sf3)))
      target)))

(defmethod save-physics-model ((source rigid-shape) target &rest args)
  (flet ((translate (shape)
           (let ((transform (marr (local-transform shape))))
             (etypecase shape
               (sphere
                (org.shirakumo.sf3:make-ellipsoid
                 (radius shape) (radius shape) (radius shape)
                 :transform transform))
               (ellipsoid
                (org.shirakumo.sf3:make-ellipsoid
                 (vx (radius shape)) (vy (radius shape)) (vz (radius shape))
                 :transform transform))
               (box
                (org.shirakumo.sf3:make-ellipsoid
                 (vx (bsize shape)) (vy (bsize shape)) (vz (bsize shape))
                 :transform transform))
               (cone
                (org.shirakumo.sf3:make-cylinder
                 (radius shape) 0.0 (height shape)
                 :transform transform))
               (cylinder
                (org.shirakumo.sf3:make-cylinder
                 (radius-bottom shape) (radius-top shape) (height shape)
                 :transform transform))
               (pill
                (org.shirakumo.sf3:make-pill
                 (radius-bottom shape) (radius-top shape) (height shape)
                 :transform transform))
               (convex-mesh
                (org.shirakumo.sf3:make-mesh
                 (convex-mesh-vertices shape)
                 :transform transform))))))
    (apply #'org.shirakumo.sf3:write-sf3
           (apply #'org.shirakumo.sf3:make-physics-model
                  (mass source) (marr (inertia-tensor source))
                  (map 'list #'translate (physics-primitives source)))
           target args)))
