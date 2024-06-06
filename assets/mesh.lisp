(in-package #:org.shirakumo.fraf.trial)

(defclass mesh-loader (resource-generator)
  ())

(defmethod generate-resources ((generator mesh-loader) (data mesh-data) &key (resource (resource generator T)))
  (make-vertex-array data resource))

;; FIXME: dependency cycle issues. Where the fuck do I put this?
;; (defmethod generate-resources ((generator mesh-loader) (primitive primitive) &rest args)
;;   (apply #'generate-resources generator (coerce-object primitive 'convex-mesh) args))

;; (defmethod generate-resources ((generator mesh-loader) (primitive convex-mesh) &key (data-usage :static-draw) (resource (resource generator T)))
;;   (let ((vbo (make-instance 'vertex-buffer :buffer-data (convex-mesh-vertices primitive) :buffer-type :array-buffer
;;                                            :data-usage data-usage :elemen-type :float))
;;         (ebo (make-instance 'vertex-buffer :buffer-data (convex-mesh-faces primitive) :buffer-type :element-array-buffer
;;                                            :data-usage data-usage :element-type :unsigned-int)))
;;     (ensure-instance resource 'vertex-array
;;                      :bindings `((,vbo :offset 0 :size 3 :stride 4))
;;                      :index-buffer ebo
;;                      :vertex-form :triangles)))

(defclass mesh (multi-resource-asset mesh-loader)
  ())
