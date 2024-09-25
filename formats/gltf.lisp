(defpackage #:org.shirakumo.fraf.trial.gltf
  (:use #:cl+trial)
  (:shadow #:asset #:load-image)
  (:local-nicknames
   (#:gltf #:org.shirakumo.fraf.gltf)
   (#:sequences #:org.shirakumo.trivial-extensible-sequences)
   (#:v #:org.shirakumo.verbose))
  (:export
   #:translate-track-pointer
   #:translate-effect
   #:define-trigger-translation))
(in-package #:org.shirakumo.fraf.trial.gltf)

(defun gltf-name (thing)
  (trial:lispify-name (or (gltf:name thing) (gltf:idx thing))))

(defun gltf-node-transform (node)
  (let ((matrix (gltf:matrix node))
        (translation (gltf:translation node))
        (scale (gltf:scale node))
        (rotation (gltf:rotation node)))
    (let ((transform (if matrix
                         (tfrom-mat (mat4 matrix))
                         (transform))))
      (when translation
        (vsetf (tlocation transform)
               (aref translation 0)
               (aref translation 1)
               (aref translation 2)))
      (when scale
        (vsetf (tscaling transform)
               (aref scale 0)
               (aref scale 1)
               (aref scale 2)))
      (when rotation
        (qsetf (trotation transform)
               (aref rotation 0)
               (aref rotation 1)
               (aref rotation 2)
               (aref rotation 3)))
      transform)))

(defmethod gltf:construct-element-reader ((element-type (eql :scalar)) (component-type (eql :float)))
  (lambda (ptr)
    (values (cffi:mem-ref ptr :float)
            (cffi:incf-pointer ptr 4))))

(defmethod gltf:construct-element-reader ((element-type (eql :vec2)) (component-type (eql :float)))
  (lambda (ptr)
    (values (vec (cffi:mem-ref ptr :float)
                 (cffi:mem-ref (cffi:incf-pointer ptr 4) :float))
            (cffi:incf-pointer ptr 4))))

(defmethod gltf:construct-element-reader ((element-type (eql :vec3)) (component-type (eql :float)))
  (lambda (ptr)
    (values (vec (cffi:mem-ref ptr :float)
                 (cffi:mem-ref (cffi:incf-pointer ptr 4) :float)
                 (cffi:mem-ref (cffi:incf-pointer ptr 4) :float))
            (cffi:incf-pointer ptr 4))))

(defmethod gltf:construct-element-reader ((element-type (eql :vec4)) (component-type (eql :float)))
  (lambda (ptr)
    (values (quat (cffi:mem-ref ptr :float)
                  (cffi:mem-ref (cffi:incf-pointer ptr 4) :float)
                  (cffi:mem-ref (cffi:incf-pointer ptr 4) :float)
                  (cffi:mem-ref (cffi:incf-pointer ptr 4) :float))
            (cffi:incf-pointer ptr 4))))

(defmethod gltf:construct-element-reader ((element-type (eql :mat4)) (component-type (eql :float)))
  (lambda (ptr)
    (let ((elements (make-array 16 :element-type 'single-float)))
      (dotimes (i (length elements))
        (setf (aref elements i) (cffi:mem-aref ptr :float i)))
      (values (nmtranspose (mat4 elements))
              (cffi:inc-pointer ptr (* 4 16))))))

(defun load-joint-names (gltf)
  (map 'vector #'gltf-name (gltf:nodes gltf)))

(defun load-rest-pose (gltf)
  (let* ((nodes (gltf:nodes gltf))
         (pose (make-instance 'pose :size (length nodes))))
    (loop for i from 0 below (length nodes)
          for node = (aref nodes i)
          do (setf (elt pose i) (gltf-node-transform node))
             (setf (parent-joint pose i) (if (gltf:parent node)
                                             (gltf:idx (gltf:parent node))
                                             -1)))
    (check-consistent pose)
    pose))

(defun load-animation-track (track sampler)
  (setf (interpolation track) (ecase (gltf:interpolation sampler)
                                (:step :constant)
                                (:linear :linear)
                                (:cubicspline :hermite)))
  (setf (frames track) (cons (gltf:input sampler) (gltf:output sampler))))

(defgeneric translate-track-pointer (pointer track gltf))

(defmethod translate-track-pointer ((pointer string) track gltf)
  (change-class track 'trial::slot-value-track
                :slot-name (lispify-name pointer "KEYWORD")
                :name (name track)))

(defmethod translate-track-pointer ((pointer symbol) track gltf)
  (change-class track 'trial::slot-value-track
                :slot-name pointer
                :name (name track)))

(defgeneric translate-effect (name effect gltf))

(defmethod translate-effect ((name string) effect gltf)
  (translate-effect (lispify-name name) effect gltf))

(defmethod translate-effect (name effect gltf)
  (v:warn :trial.gltf "Unknown effect name: ~s, ignoring." name)
  NIL)

;; FIXME: How do we actually translate the pointer to the corresponding lisp-side object slot?
;;        it's unlikely to be what's pointed to by the json pointer, since objects are transformed
;;        to more fitting native representations that should be manipulated instead.

(defun load-clip (gltf animation)
  (let ((clip (make-instance 'clip :name (gltf-name animation))))
    (loop for channel across (gltf:channels animation)
          for target = (gltf:target channel)
          for sampler = (svref (gltf:samplers animation) (gltf:sampler channel))
          for track = (find-animation-track clip (gltf:idx (gltf:node target)) :if-does-not-exist :create)
          do (case (gltf:path target)
               (:translation
                (load-animation-track (location track) sampler))
               (:scale
                (load-animation-track (scaling track) sampler))
               (:rotation
                (load-animation-track (rotation track) sampler))
               (:weights
                (change-class track 'trial::weights-track :name (gltf-name (gltf:node target)))
                (load-animation-track track sampler))
               (:pointer
                (translate-track-pointer (gltf:pointer channel) track gltf)
                (load-animation-track track sampler))
               (T (v:warn :trial.gltf "Unknown animation channel target path: ~s on ~s, ignoring."
                          (gltf:path (gltf:target channel)) (gltf-name animation)))))
    ;; Extra handling for custom properties
    (let* ((extras (gltf:extensions animation))
           (trial (when extras (gethash "SHIRAKUMO_trial" extras)))
           (tracks (when trial (gethash "extraTracks" trial))))
      (when tracks
        (loop for field being the hash-keys of tracks using (hash-value data)
              for track = (find-animation-track clip field :if-does-not-exist :create)
              do (translate-track-pointer field track gltf)
                 (setf (interpolation track) :constant)
                 (setf (frames track) (cons (map 'vector (lambda (f) (float f 0f0)) (gethash "times" data))
                                            (map 'vector (lambda (f) (float f 0f0)) (gethash "values" data)))))))
    (trial::recompute-duration clip)
    (case (gltf:kind animation)
      (:blocking
       (setf (trial:blocking-p clip) T))
      (:physical
       (setf (trial:blocking-p clip) T)
       (change-class clip 'forward-kinematic-clip :velocity-scale (gltf:velocity-scale animation))))
    (if (gltf:next animation)
        (setf (next-clip clip) (trial:lispify-name (gltf:next animation)))
        (setf (loop-p clip) (gltf:loop-p animation)))
    (setf (blend-duration clip) (gltf:blend-duration animation))
    (setf (trial::effects clip)
          (coerce (loop for effect across (gltf:effects animation)
                        for object = (translate-effect (gltf:name effect) effect gltf)
                        when object collect (trial::make-animation-effect
                                             (float (gltf:start effect) 0f0)
                                             (float (or (gltf:end effect) (gltf:start effect)) 0f0)
                                             object))
                  'simple-vector))
    clip))

(defun load-clips (gltf &optional (table (make-hash-table :test 'equal)))
  (loop for animation across (gltf:animations gltf)
        for clip = (load-clip gltf animation)
        do (setf (gethash (name clip) table) clip))
  table)

(defun load-bind-pose (gltf)
  (let* ((rest-pose (load-rest-pose gltf))
         (world-bind-pose (make-array (length rest-pose))))
    (dotimes (i (length world-bind-pose))
      (setf (svref world-bind-pose i) (global-transform rest-pose i)))
    (loop for skin across (gltf:skins gltf)
          for joints = (gltf:joints skin)
          for acc = (gltf:inverse-bind-matrices skin)
          do (loop for i from 0 below (length joints)
                   for inv-bind-matrix = (elt acc i)
                   do (setf (aref world-bind-pose (gltf:idx (svref joints i)))
                            (tfrom-mat (minv inv-bind-matrix)))))
    (let ((bind-pose rest-pose))
      (loop for i from 0 below (length world-bind-pose)
            for current = (svref world-bind-pose i)
            for p = (parent-joint bind-pose i)
            do (setf (elt bind-pose i)
                     (if (<= 0 p)
                         (t+ (tinv (svref world-bind-pose p)) current)
                         current)))
      (check-consistent bind-pose)
      bind-pose)))

(defun load-skeleton (gltf)
  (make-instance 'skeleton :rest-pose (load-rest-pose gltf)
                           :bind-pose (load-bind-pose gltf)
                           :joint-names (load-joint-names gltf)))

(defun gltf-attribute-to-native-attribute (attribute)
  (case attribute
    (:position 'location)
    (:normal 'normal)
    (:tangent 'tangent)
    (:texcoord_0 'uv)
    (:texcoord_1 'uv-1)
    (:texcoord_2 'uv-2)
    (:texcoord_3 'uv-3)
    (:joints_0 'joints)
    (:joints_1 'joints-1)
    (:joints_2 'joints-2)
    (:joints_3 'joints-3)
    (:weights_0 'weights)
    (:weights_1 'weights-1)
    (:weights_2 'weights-2)
    (:weights_3 'weights-3)))

(defun load-vertex-attribute (mesh attribute accessor skin)
  (declare (optimize speed))
  (let ((data (vertex-data mesh))
        (stride (vertex-attribute-stride mesh))
        (offset (vertex-attribute-offset attribute mesh)))
    (declare (type (simple-array (single-float) (*)) data))
    (declare (type (unsigned-byte 32) offset stride))
    (when (< (length data) (length accessor))
      (setf data (adjust-array data (* (length accessor) stride) :element-type 'single-float))
      (setf (vertex-data mesh) data))
    (case (vertex-attribute-category attribute)
      (joints
       (flet ((map-joint (joint)
                (float (max 0 (gltf:idx (svref (gltf:joints skin) joint))) 0f0)))
         (loop for i of-type (unsigned-byte 32) from 0 below (length accessor)
               for el = (elt accessor i)
               do (setf (aref data (+ (* i stride) offset 0)) (map-joint (aref el 0)))
                  (setf (aref data (+ (* i stride) offset 1)) (map-joint (aref el 1)))
                  (setf (aref data (+ (* i stride) offset 2)) (map-joint (aref el 2)))
                  (setf (aref data (+ (* i stride) offset 3)) (map-joint (aref el 3))))))
      (uv
       (loop for i of-type (unsigned-byte 32) from 0 below (length accessor)
             for el of-type vec2 = (elt accessor i)
             do (setf (aref data (+ (* i stride) offset 0)) (vx2 el))
                (setf (aref data (+ (* i stride) offset 1)) (- 1.0 (vy2 el)))))
      (T
       (ecase (vertex-attribute-size attribute)
         (1
          (loop for i of-type (unsigned-byte 32) from 0 below (length accessor)
                for el = (elt accessor i)
                do (setf (aref data (+ (* i stride) offset)) (float el 0f0))))
         (2
          (loop for i of-type (unsigned-byte 32) from 0 below (length accessor)
                for el of-type vec2 = (elt accessor i)
                do (setf (aref data (+ (* i stride) offset 0)) (vx2 el))
                   (setf (aref data (+ (* i stride) offset 1)) (vy2 el))))
         (3
          (loop for i of-type (unsigned-byte 32) from 0 below (length accessor)
                for el of-type vec3 = (elt accessor i)
                do (setf (aref data (+ (* i stride) offset 0)) (vx3 el))
                   (setf (aref data (+ (* i stride) offset 1)) (vy3 el))
                   (setf (aref data (+ (* i stride) offset 2)) (vz3 el))))
         (4
          (loop for i of-type (unsigned-byte 32) from 0 below (length accessor)
                for el of-type quat = (elt accessor i)
                do (setf (aref data (+ (* i stride) offset 0)) (qx el))
                   (setf (aref data (+ (* i stride) offset 1)) (qy el))
                   (setf (aref data (+ (* i stride) offset 2)) (qz el))
                   (setf (aref data (+ (* i stride) offset 3)) (qw el)))))))))

(defmethod org.shirakumo.memory-regions:call-with-memory-region ((function function) (accessor gltf:accessor) &key (start 0))
  (let ((region (org.shirakumo.memory-regions:memory-region
                 (cffi:inc-pointer (gltf:start accessor) start)
                 (* (gltf:size accessor) (gltf:byte-stride accessor)))))
    (declare (dynamic-extent region))
    (funcall function region)))

(defun load-mesh-attributes (mesh attribute-map &optional skin)
  (let* ((attributes (loop for attribute being the hash-keys of attribute-map
                           for native = (gltf-attribute-to-native-attribute attribute)
                           if native
                           collect native
                           else do (v:warn :trial.gltf "Ignoring mesh attribute ~s: don't know a native equivalent!" attribute))))
    (setf (vertex-attributes mesh) attributes)
    (loop for attribute being the hash-keys of attribute-map using (hash-value accessor)
          for native = (gltf-attribute-to-native-attribute attribute)
          do (when (member native attributes)
               (load-vertex-attribute mesh native accessor skin)))
    mesh))

(defun load-primitive (primitive &key name skin model model-name weights)
  (let* ((mesh (if (or skin (< 0 (length (gltf:targets primitive))))
                   (make-instance 'animated-mesh
                                  :name name
                                  :vertex-form (gltf:mode primitive)
                                  :skinned-p (not (null skin)))
                   (make-instance 'static-mesh
                                  :name name
                                  :vertex-form (gltf:mode primitive)))))
    (load-mesh-attributes mesh (gltf:attributes primitive) skin)
    (when (and model (gltf:material primitive))
      (setf (material mesh) (find-material (gltf-name (gltf:material primitive)) model)))
    (when (gltf:indices primitive)
      (let* ((accessor (gltf:indices primitive))
             (indexes (make-array (length accessor) :element-type (ecase (gltf:component-type accessor)
                                                                    (:uint8  '(unsigned-byte 8))
                                                                    (:uint16 '(unsigned-byte 16))
                                                                    (:uint32 '(unsigned-byte 32))))))
        (org.shirakumo.memory-regions:replace indexes accessor)
        (setf (faces mesh) (coerce indexes '(simple-array (unsigned-byte 32) 1)))))
    (when (< 0 (length (gltf:targets primitive)))
      (setf (trial::morphs mesh) (map 'vector (lambda (spec) (load-mesh-attributes (make-instance 'mesh-data) spec))
                                      (gltf:targets primitive)))
      (setf (trial::model-name mesh) model-name)
      (setf (trial::initial-weights mesh) (or weights #())))
    mesh))

(defun load-mesh (mesh model &key skin model-name)
  (let ((base-name (gltf-name mesh))
        (primitives (gltf:primitives mesh)))
    (flet ((load-primitive (primitive name)
             (load-primitive primitive :skin skin :name name :model model :weights (gltf:weights mesh) :model-name model-name)))
      (case (length primitives)
        (0 ())
        (1 (list (load-primitive (aref primitives 0) base-name)))
        (T (loop for i from 0 below (length primitives)
                 for primitive = (aref primitives i)
                 collect (load-primitive primitive (cons base-name i))))))))

(defun load-meshes (gltf model)
  (let ((meshes (make-array 0 :adjustable T :fill-pointer T)))
    (loop for node across (gltf:nodes gltf)
          for skin = (gltf:skin node)
          do (when (gltf:mesh node)
               (loop for mesh in (load-mesh (gltf:mesh node) model :skin skin :model-name (gltf-name node))
                     do (vector-push-extend mesh meshes))))
    meshes))

(defun load-image (asset texinfo)
  (when texinfo
    (let* ((texture (gltf:texture texinfo))
           (sampler (gltf:sampler texture))
           (image (gltf:source texture))
           (name (trial:lispify-name
                  (or ;; NOTE: we cannot use the image's name since it can
                   ;; alias with other, distinct images
                   (gltf:uri image)
                   (gltf:name (gltf:buffer-view image))
                   (format NIL "image-~d" (gltf:idx image))))))
      (generate-resources 'image-loader (if (gltf:uri image)
                                            (gltf:path image)
                                            (memory-region (gltf:start (gltf:buffer-view image))
                                                           (gltf:byte-length (gltf:buffer-view image))))
                          :type (or (gltf:mime-type image) T)
                          :resource (resource asset name)
                          :mag-filter (if sampler (gltf:mag-filter sampler) :linear)
                          :min-filter (if sampler (gltf:min-filter sampler) :linear)
                          :wrapping (list (if sampler (gltf:wrap-s sampler) :clamp-to-edge)
                                          (if sampler (gltf:wrap-t sampler) :clamp-to-edge)
                                          (if sampler (gltf:wrap-t sampler) :clamp-to-edge))))))

(defun load-materials (gltf model asset)
  (flet ((to-vec (array)
           (ecase (length array)
             (2 (vec (aref array 0) (aref array 1)))
             (3 (vec (aref array 0) (aref array 1) (aref array 2)))
             (4 (vec (aref array 0) (aref array 1) (aref array 2) (aref array 3))))))
    (loop for material across (gltf:materials gltf)
          for pbr = (gltf:pbr material)
          for name = (gltf-name material)
          for mr = (when pbr (load-image asset (gltf:metallic-roughness pbr)))
          for omr = (load-image asset (gltf:occlusion-metalness-roughness-texture material))
          for rmo = (load-image asset (gltf:roughness-metallic-occlusion-texture material))
          do (when mr (setf (trial::swizzle mr) '(:b :g :r :a)))
             (when omr (setf (trial::swizzle omr) '(:g :b :r :a)))
             (when rmo (setf (trial::swizzle rmo) '(:g :r :b :a)))
             (let ((material (trial:ensure-instance
                              (trial:find-material name model NIL) 'trial:pbr-material
                              :double-sided (gltf:double-sided-p material)
                              :albedo-texture (when pbr (load-image asset (gltf:albedo pbr)))
                              :metal-rough-texture mr
                              :metal-rough-occlusion-texture (or omr rmo)
                              :occlusion-texture (load-image asset (gltf:occlusion-texture material))
                              :emission-texture (load-image asset (gltf:emissive-texture material))
                              :normal-texture (load-image asset (gltf:normal-texture material))
                              :albedo-factor (if pbr (to-vec (gltf:albedo-factor pbr)) (vec 1 1 1 1))
                              :metalness-factor (if pbr (float (gltf:metallic-factor pbr) 0f0) 0.0)
                              :roughness-factor (if pbr (float (gltf:roughness-factor pbr) 0f0) 1.0)
                              :emission-factor (to-vec (gltf:emissive-factor material))
                              :occlusion-factor (if (gltf:occlusion-texture material) 1.0 0.0)
                              :alpha-cutoff (float (gltf:alpha-cutoff material) 0f0)
                              :storage NIL)))
               (setf (trial:find-material name model) material)))))

(defun load-light (light)
  (flet ((make (type intensity &rest initargs)
           (apply #'make-instance type
                  :color (nv* (vec (aref (gltf:color light) 0)
                                   (aref (gltf:color light) 1)
                                   (aref (gltf:color light) 2))
                              intensity)
                  initargs)))
    (etypecase light
      (gltf:directional-light
       (make 'trial:directional-light
             (/ (sqrt (gltf:intensity light)) 100.0)
             :direction (vec 0 0 -1)))
      (gltf:point-light
       (make 'trial:point-light
             (/ (sqrt (gltf:intensity light)) 500.0)
             :linear-attenuation (or (gltf:range light) 0.0)
             :quadratic-attenuation 0.0))
      (gltf:spot-light
       (make 'trial:spot-light
             ;; FIXME: I have no funcking clue what I'm doing here
             (/ (sqrt (gltf:intensity light)) 500.0)
             :direction (vec 0 0 -1)
             :linear-attenuation (or (gltf:range light) 0.0)
             :inner-radius (rad->deg (gltf:inner-angle light))
             :outer-radius (rad->deg (gltf:outer-angle light)))))))

(defun load-camera (camera)
  (etypecase camera
    (gltf:orthographic-camera
     (make-instance 'trial:2d-camera :near-plane (float (gltf:znear camera) 0f0)
                                     :far-plane (float (gltf:zfar camera) 0f0)
                                     :location (vec 0 0 0)))
    (gltf:perspective-camera
     (make-instance 'trial:target-camera :fov (float (rad->deg (gltf:fov camera)) 0f0)
                                         :near-plane (float (gltf:znear camera) 0f0)
                                         :far-plane (float (gltf:zfar camera) 0f0)
                                         :location (vec 0 0 0)
                                         :target (vec 0 0 -1)))))

(defun load-shape (shape model &rest args)
  (flet ((ensure-mesh (mesh)
           (or (find-mesh (gltf-name mesh) model NIL)
               (first (load-mesh mesh model)))))
    (etypecase shape
      (gltf:sphere-shape
       (apply #'trial:make-sphere :radius (float (gltf:radius shape) 0f0)
              args))
      (gltf:box-shape
       (apply #'trial:make-box :bsize (vec (* 0.5 (aref (gltf:size shape) 0))
                                           (* 0.5 (aref (gltf:size shape) 1))
                                           (* 0.5 (aref (gltf:size shape) 2)))
              args))
      (gltf:capsule-shape
       (apply #'trial:make-pill :height (float (* 0.5 (gltf:height shape)) 0f0)
                                :radius (max (float (gltf:radius-top shape) 0f0)
                                             (float (gltf:radius-bottom shape) 0f0))
                                args))
      (gltf:cylinder-shape
       (apply (cond ((= 0 (gltf:radius-top shape))
                     #'trial:make-cone)
                    (T
                     #'trial:make-cylinder))
              :height (float (* 0.5 (gltf:height shape)) 0f0)
              :radius (max (float (gltf:radius-top shape) 0f0)
                           (float (gltf:radius-bottom shape) 0f0))
              args))
      (gltf:mesh-shape
       (let ((mesh (ensure-mesh (gltf:mesh shape))))
         (apply (cond ((not (gltf:convex-p shape))
                       #'trial:make-general-mesh)
                      ;; NOTE: If we have a mesh with few faces, skip the more elaborate hill climbing.
                      ((< (length (trial:faces mesh)) 8)
                       #'trial:make-convex-mesh)
                      (T
                       #'trial::make-optimized-convex-mesh))
                :vertices (trial:reordered-vertex-data mesh '(trial:location))
                :faces (trial::simplify (trial:faces mesh) '(unsigned-byte 16))
                args))))))

(defvar *physics-material-cache* (make-hash-table :test 'equal))
(defun physics-material-instance (material)
  (let ((name (list (gltf:static-friction material)
                    (gltf:dynamic-friction material)
                    (gltf:restitution material)
                    (gltf:friction-combine material)
                    (gltf:restitution-combine material))))
    (or (gethash name *physics-material-cache*)
        (setf (gethash name *physics-material-cache*)
              (trial:make-material-interaction-properties
               NIL NIL
               (gltf:static-friction material)
               (gltf:dynamic-friction material)
               (gltf:restitution material)
               (gltf:friction-combine material)
               (gltf:restitution-combine material))))))

(defun collision-filter-mask (filter)
  (flet ()
    (let ((mask (1- (ash 1 32))))
      (cond ((gltf:collide-with-systems filter)
             (setf mask 0)
             (loop for system across (gltf:collide-with-systems filter)
                   for idx = (trial::collision-system-idx system)
                   do (setf (ldb (byte 1 idx) mask) 1)))
            ((gltf:not-collide-with-systems filter)
             (loop for system across (gltf:collide-with-systems filter)
                   for idx = (trial::collision-system-idx system)
                   do (setf (ldb (byte 1 idx) mask) 0))))
      mask)))

(defun find-colliders (node model)
  (let ((primitives (make-array 0 :adjustable T :fill-pointer T))
        (material :wood)
        (mask 1))
    (labels ((process (collider tf)
               (when (gltf:physics-material collider)
                 (setf material (physics-material-instance (gltf:physics-material collider))))
               (when (gltf:collision-filter collider)
                 (setf mask (collision-filter-mask (gltf:collision-filter collider))))
               (let ((primitive (load-shape (gltf:shape collider) model
                                            :local-transform (tmat tf))))
                 (setf (trial:primitive-collision-mask primitive) mask)
                 (setf (trial:primitive-material primitive) material)
                 (vector-push-extend primitive primitives)))
             (recurse (node tf)
               (let ((tf (t+ tf (gltf-node-transform node))))
                 (when (and (gltf:collider node)
                            #++(or (gltf:virtual-p node)
                                (not (gltf:rigidbody node))
                                (not (gltf:mesh node))))
                   (process (gltf:collider node) tf))
                 (loop for child across (gltf:children node)
                       do (recurse child tf)))))
      (let ((tf (transform)))
        (when (gltf:collider node)
          (process (gltf:collider node) tf))
        (loop for child across (gltf:children node)
              do (recurse child tf)))
      (trial::simplify primitives))))

(defun load-environment-light (light)
  (let ((envmap (trial:implement!)))
    (list (make-instance 'trial:environment-light
                         :color (vec (gltf:intensity light) (gltf:intensity light) (gltf:intensity light))
                         :irradiance-map (trial:implement!)
                         :environment-map envmap)
          (make-instance 'trial:skybox :texture (resource envmap :environment-map)))))

(defun load-rigidbody (model child node)
  ;; FIXME: implement joints
  (etypecase child
    (basic-entity (change-class child 'trial:basic-physics-entity))
    (basic-animated-entity (change-class child 'trial:animated-physics-entity)))
  (setf (trial:mass child) (gltf:mass (gltf:rigidbody node)))
  ;; FIXME: implement center-of-mass
  (when (/= 1.0 (gltf:gravity-factor (gltf:rigidbody node)))
    (v:warn :trial.gltf "Ignoring non-standard gravity factor on ~a" node))
  (let* ((r (gltf:inertia-orientation (gltf:rigidbody node)))
         (r (quat (aref r 0) (aref r 1) (aref r 2) (aref r 3)))
         (d (gltf:inertia-diagonal (gltf:rigidbody node))))
    (setf (trial:inertia-tensor child) (trial:diagonal-tensor d r)))
  (map-into (varr (trial:velocity child)) (lambda (x) (float x 0f0)) (gltf:linear-velocity (gltf:rigidbody node)))
  (map-into (varr (trial:rotation child)) (lambda (x) (float x 0f0)) (gltf:angular-velocity (gltf:rigidbody node)))
  ;; Extra support for damping factor
  (when (gltf:extras (gltf:rigidbody node))
    (setf (trial:damping child) (gethash "damping" (gltf:extras (gltf:rigidbody node)) 0.95)))
  ;; FIXME: this will eagerly decompose colliders and so on even if the node is never used...
  (setf (trial:physics-primitives child) (find-colliders node model)))

(defvar *trigger-translator-functions* (make-hash-table :test 'equal))

(defmacro define-trigger-translation (gltf-class args &body body)
  `(setf (gethash ',gltf-class *trigger-translator-functions*)
         (lambda ,args
           ,@body)))

(defun load-trigger (model child node)
  (etypecase child
    (basic-node
     (change-class child 'trial:trigger-volume)))
  (let ((shape (load-shape (gltf:shape (gltf:trigger node)) model)))
    (setf (trial:primitive-collision-mask shape) (collision-filter-mask (gltf:collision-filter (gltf:trigger node))))
    (setf (trial:physics-primitives child) shape)
    (unless (gltf:shirakumo-trigger-data node)
      (error "Trigger has no extra data."))
    (with-simple-restart (continue "Ignore the trigger translation")
      (funcall (or (gethash (type-of (gltf:shirakumo-trigger-data node)) *trigger-translator-functions*)
                   (error "Unknown trigger volume type."))
               child (gltf:shirakumo-trigger-data node)))))

(defun %find-child (name node &optional errorp)
  (sequences:dosequence (child node (when errorp (error "No child named ~a found!" name)))
    (when (and (<= (length name) (length (name child)))
               (string= name (name child) :end2 (length name)))
      (return child))))

(define-trigger-translation gltf:shirakumo-trigger (trigger trigger-data)
  (change-class trigger 'trial::simple-trigger-volume
                :type-expression (read-from-string (gltf:filter trigger-data))
                :form (gltf:form trigger-data)))

(define-trigger-translation gltf:shirakumo-spawner (trigger trigger-data)
  (destructuring-bind (&optional class-or-count &rest args) (enlist (read-from-string (gltf:spawn trigger-data)))
    (let ((spawn-volume (aref (physics-primitives trigger) 0))
          (trig-volume (%find-child "trigger" trigger))
          (spawn-count (gltf:spawn-count trigger-data)))
      (cond (trig-volume
             ;; Copy physics primitive //and transform// over
             (setf (physics-primitives trigger) (physics-primitives trig-volume))
             (!t+ (tf trigger) (tf trigger) (tf trig-volume)))
            (T
             (setf (physics-primitives trigger) (make-sphere :radius 10000f0))))
      (etypecase class-or-count
        (integer
         (setf spawn-count class-or-count)
         (setf class-or-count (%find-child "class" trigger T)))
        (symbol))
      (clear trigger)
      (change-class trigger 'trial::spawner-trigger-volume
                    :type-expression (read-from-string (gltf:filter trigger-data))
                    :spawn-class class-or-count
                    :spawn-arguments args
                    :spawn-count (or spawn-count 1)
                    :spawn-volume spawn-volume
                    :auto-deactivate (gltf:auto-deactivate-p trigger-data)
                    :respawn-cooldown (gltf:respawn-cooldown trigger-data)))))

(define-trigger-translation gltf:shirakumo-killvolume (trigger trigger-data)
  (change-class trigger 'trial::kill-trigger-volume
                :type-expression (read-from-string (gltf:filter trigger-data))
                :class-name (read-from-string (gltf:kill trigger-data))))

(define-trigger-translation gltf:shirakumo-checkpoint (trigger trigger-data)
  (let ((array (gltf:spawn-point trigger-data)))
    (change-class trigger 'trial::checkpoint-trigger
                  :type-expression (read-from-string (gltf:filter trigger-data))
                  :spawn-point (vec (aref array 0) (aref array 1) (aref array 2)))))

(define-trigger-translation gltf:shirakumo-progression (trigger trigger-data)
  (change-class trigger 'trial::global-sequence-trigger
                :condition (gltf:condition trigger-data)
                :sequence-id (gltf:state trigger-data)
                :new-value (gltf:value trigger-data)
                :modulation (ecase (gltf:mode trigger-data)
                              (:inc #'+)
                              (:dec #'-)
                              (:set (lambda (prev new)
                                      (declare (ignore prev))
                                      new)))))

(defmethod load-model (input (type (eql :glb)) &rest args)
  (apply #'load-model input :gltf args))

(defmethod load-model (input (type (eql :gltf)) &key (generator (make-instance 'resource-generator))
                                                     (model (make-instance 'model)))
  (gltf:with-gltf (gltf input)
    (let ((meshes (meshes model))
          (clips (clips model))
          (scenes (scenes model)))
      (load-materials gltf model generator)
      (loop for mesh across (load-meshes gltf model)
            do (setf (gethash (name mesh) meshes) mesh)
               (trial::make-vertex-array mesh (resource generator (name mesh))))
      ;; Patch up
      (load-clips gltf clips)
      (when (loop for mesh being the hash-values of meshes
                  thereis (skinned-p mesh))
        (setf (skeleton model) (load-skeleton gltf))
        (let ((map (make-hash-table :test 'eql)))
          (trial::reorder (skeleton model) map)
          (loop for clip being the hash-values of clips
                do (trial::reorder clip map))
          (loop for mesh being the hash-values of meshes
                do (when (skinned-p mesh)
                     (trial::reorder mesh map)))))
      ;; Construct scene graphs
      (labels ((construct (node)
                 (cond ((loop for skin across (gltf:skins gltf)
                              thereis (loop for joint across (gltf:joints skin)
                                            thereis (eq joint node)))
                        ;; Eliminate nodes that are parts of a skin
                        NIL)
                       ((gltf:virtual-p node)
                        ;; Eliminate nodes that are marked as virtual
                        NIL)
                       ((gltf:mesh node)
                        (let ((mesh-name (gltf-name (gltf:mesh node))))
                          (make-instance (etypecase (or (gethash mesh-name meshes)
                                                        (gethash (cons mesh-name 0) meshes))
                                           (static-mesh 'basic-entity)
                                           ;; FIXME: instead of turning each skin into an animated entity
                                           ;;        we should share the pose between them and only make one
                                           ;;        animated entity the controller
                                           (animated-mesh 'basic-animated-entity))
                                         :lods (loop for i from -1
                                                     for threshold across (gltf:lod-screen-coverage node)
                                                     for lod = mesh-name then (gltf-name (gltf:mesh (aref (gltf:lods node) i)))
                                                     collect (make-instance 'lod :threshold threshold :mesh lod))
                                         :transform (gltf-node-transform node)
                                         :name (gltf-name node)
                                         :asset generator
                                         :mesh mesh-name)))
                       (T
                        (make-instance 'basic-node :transform (gltf-node-transform node)
                                                   :name (gltf-name node)))))
               (recurse (children container)
                 (loop for node across children
                       for child = (construct node)
                       when child
                       do (recurse (gltf:children node) child)
                          (when (gltf:light node)
                            (enter (load-light (gltf:light node)) child))
                          (when (gltf:camera node)
                            (enter (load-camera (gltf:camera node)) child))
                          (when (and (not (gltf:trigger node)) (gltf:rigidbody node))
                            (load-rigidbody model child node))
                          (when (gltf:trigger node)
                            (load-trigger model child node))
                          (enter child container))))
        (loop for node across (gltf:scenes gltf)
              for scene = (make-instance 'basic-node :name (gltf-name node))
              do (setf (gethash (name scene) scenes) scene)
                 (when (gltf:light node)
                   (dolist (object (load-environment-light (gltf:light node)))
                     (enter object scene)))
                 (when (gltf:envmap node)
                   (let ((envmap (make-instance 'environment-map :input (merge-pathnames (gltf:envmap node) input))))
                     (enter (make-instance 'environment-light :asset envmap :name :envlight) scene)
                     (enter (make-instance 'skybox :texture (resource envmap :environment-map) :name :skybox) scene)))
                 (recurse (gltf:nodes node) scene)))
      model)))

(defun add-convex-shape (gltf vertices faces)
  (let* ((primitive (gltf:make-mesh-primitive gltf vertices faces '(:position)))
         (mesh (gltf:make-indexed 'gltf:mesh gltf :primitives (vector primitive))))
    (gltf:make-indexed 'gltf:mesh-shape gltf :mesh mesh :kind "mesh" :convex-p T)))

(defun push-convex-shape (base-node shape)
  (let* ((collider (make-instance 'gltf:collider :collision-filter (gltf:collision-filter (gltf:collider base-node))
                                                 :physics-material (gltf:physics-material (gltf:collider base-node))
                                                 :shape shape
                                                 :gltf (gltf:gltf base-node)))
         (child (gltf:make-indexed 'gltf:node base-node :collider collider :virtual-p T)))
    (gltf:push-child child base-node)))

(defmethod optimize-model (file (type (eql :glb)) &rest args)
  (apply #'optimize-model file :gltf args))

(defun shape-optimizable-p (shape)
  (and (typep shape 'gltf:mesh-shape)
       (not (gltf:convex-p shape))))

(defmethod optimize-model (file (type (eql :gltf)) &rest args &key (output file) &allow-other-keys)
  (let ((decomposition-args (remf* args :output))
        (shape-table (make-hash-table :test 'eql))
        (work-done-p NIL))
    (trial:with-tempfile (tmp :type (pathname-type file))
      (gltf:with-gltf (gltf file)
        ;; Rewrite mesh shapes to multiple new shapes.
        ;; TODO: if original mesh has no other refs anywhere, remove it
        (loop for shape across (gltf:shapes gltf)
              do (when (and (shape-optimizable-p shape)
                            ;; Only bother decomposing it if it's actually referenced anywhere.
                            (loop for node across (gltf:nodes gltf)
                                  thereis (and (gltf:collider node) (eql shape (gltf:shape (gltf:collider node))))))
                   (let* ((primitives (gltf:primitives (gltf:mesh shape)))
                          (mesh (load-primitive (aref primitives 0)))
                          (verts (reordered-vertex-data mesh '(location)))
                          (hulls (handler-bind ((warning #'muffle-warning))
                                   (apply #'org.shirakumo.fraf.convex-covering:decompose
                                          verts (faces mesh) decomposition-args))))
                     (setf (gethash shape shape-table)
                           (loop for hull across hulls
                                 collect (add-convex-shape
                                          gltf
                                          (org.shirakumo.fraf.convex-covering:vertices hull)
                                          (trial::simplify (org.shirakumo.fraf.convex-covering:faces hull) '(unsigned-byte 16))))))))
        ;; Rewrite nodes with refs to mesh colliders to have child nodes for
        ;; all decomposed hulls.
        (loop for node across (gltf:nodes gltf)
              do (when (and (gltf:collider node) (shape-optimizable-p (gltf:shape (gltf:collider node))))
                   (loop for shape in (gethash (gltf:shape (gltf:collider node)) shape-table)
                         do (push-convex-shape node shape))
                   (setf (gltf:collider node) NIL)
                   ;; Clear the extension, too. Ideally this would be done by the library already.
                   (remhash "collider" (gethash "KHR_physics_rigid_bodies" (gltf:extensions node)))
                   (setf work-done-p T)))
        (when work-done-p
          (gltf:serialize gltf tmp)))
      (when work-done-p
        ;; FIXME: this does not work correctly if gltf serialises to multiple files.
        (org.shirakumo.filesystem-utils:rename-file* tmp output)))))
