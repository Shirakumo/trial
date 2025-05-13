(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct standard-environment-information
  (view-matrix :mat4)
  (inv-view-matrix :mat4)
  (projection-matrix :mat4)
  (inv-projection-matrix :mat4)
  (view-size :uvec2 :accessor view-size)
  (camera-position :vec3 :accessor location)
  (focal-point :vec3 :accessor focal-point)
  (near-plane :float :accessor near-plane)
  (far-plane :float :accessor far-plane)
  (tt :float :accessor tt)
  (dt :float :accessor dt)
  (fdt :float :accessor fdt)
  (gamma :float :initform 2.2 :accessor gamma)
  (fc NIL :accessor fc :initform -1))

(define-asset (trial standard-environment-information) uniform-block
    'standard-environment-information
  :binding NIL)

(define-shader-pass standard-environment-pass ()
  ((fc :initform 0 :accessor fc)
   (frame-start :accessor frame-start :initform 0.0))
  (:buffers (trial standard-environment-information)))

(define-handler (standard-environment-pass tick :before) (tt dt fc)
  (setf (fc standard-environment-pass) fc)
  (with-buffer-tx (buffer (// 'trial 'standard-environment-information) :update NIL)
    (setf (slot-value buffer 'tt) (float tt 0f0))
    (setf (slot-value buffer 'dt) (float dt 0f0))))

(defmethod render :before ((pass standard-environment-pass) target)
  (when (< (fc (buffer-data (// 'trial 'standard-environment-information))) (fc pass))
    (with-buffer-tx (buffer (// 'trial 'standard-environment-information))
      (setf (fc buffer) (fc pass))
      (let* ((frame-time (current-time))
             (old-time (shiftf (frame-start pass) frame-time))
             (fdt (- frame-time old-time))
             (camera (camera pass))
             (tmp-mat (mat4)) (tmp-vec (vec2)))
        (declare (dynamic-extent tmp-mat tmp-vec))
        (setf (slot-value buffer 'view-matrix) (view-matrix))
        (setf (slot-value buffer 'inv-view-matrix) (!minv tmp-mat (view-matrix)))
        (setf (slot-value buffer 'projection-matrix) (projection-matrix))
        (setf (slot-value buffer 'inv-projection-matrix) (!minv tmp-mat (projection-matrix)))
        (setf (slot-value buffer 'view-size) (vsetf tmp-vec (width pass) (height pass)))
        (setf (slot-value buffer 'camera-position) (global-location camera))
        (setf (slot-value buffer 'focal-point) (focal-point camera))
        (setf (slot-value buffer 'near-plane) (float (near-plane camera) 0f0))
        (setf (slot-value buffer 'far-plane) (float (far-plane camera) 0f0))
        (setf (slot-value buffer 'fdt) (float fdt 0f0))
        (setf (slot-value buffer 'gamma) (setting :display :gamma))))))

(define-shader-pass standard-render-pass (per-object-pass standard-environment-pass)
  ((color :port-type output :texspec (:internal-format :rgba32f) :attachment :color-attachment0 :reader color)
   (normal :port-type output :texspec (:internal-format :rgb16f) :attachment :color-attachment1 :reader normal)
   (depth :port-type output :texspec (:internal-format :depth-stencil) :attachment :depth-stencil-attachment :reader depth)
   (material-block :buffer T :reader material-block)
   (light-block :buffer T :reader light-block)
   (allocated-textures :initform (make-lru-cache 16 'eq) :accessor allocated-textures)
   (allocated-materials :accessor allocated-materials)
   (allocated-lights :accessor allocated-lights))
  (:shader-file (trial "renderer/standard-render-pass.glsl")))

(defmethod initialize-instance :after ((pass standard-render-pass) &key (max-lights 128) (max-materials 64))
  (setf (allocated-materials pass) (make-lru-cache max-materials))
  (setf (allocated-lights pass) (make-lru-cache max-lights))
  (setf (slot-value pass 'material-block) (make-instance 'uniform-buffer :binding NIL :struct (make-instance (material-block-type pass) :size max-materials)))
  (setf (slot-value pass 'light-block) (make-instance 'uniform-buffer :binding NIL :struct (make-instance 'standard-light-block :size max-lights))))

(defmethod shared-initialize :after ((pass standard-render-pass) slots &key)
  (let ((max-textures (max 16 (if *context* (gl:get-integer :max-texture-image-units) 0))))
    (dolist (port (flow:ports pass))
      (typecase port
        (texture-port
         (setf max-textures (min max-textures (unit-id port))))))
    (lru-cache-resize (allocated-textures pass) max-textures)))

(defmethod describe-object :after ((pass standard-render-pass) stream)
  (format stream "~&~%Lights:~%")
  (do-lru-cache (material id (allocated-lights pass))
    (format stream " ~a~%" material))
  (format stream "~&~%Materials:~%")
  (do-lru-cache (material id (allocated-materials pass))
    (format stream " ~a~%" material)))

(defmethod lights ((pass standard-render-pass))
  (let ((lights ()))
    (do-lru-cache (light id (allocated-lights pass) lights)
      (push light lights))))

(defmethod materials ((pass standard-render-pass))
  (let ((materials ()))
    (do-lru-cache (material id (allocated-materials pass) materials)
      (push material materials))))

(defmethod make-pass-shader-program ((pass standard-render-pass) (object renderable))
  (if (typep object 'standard-renderable)
      (call-next-method)
      (make-shader-program object)))

(defmethod make-pass-shader-program ((pass standard-render-pass) (class shader-entity-class))
  (if (c2mop:subclassp class (find-class 'standard-renderable))
      (call-next-method)
      (make-shader-program class)))

(defmethod clear :after ((pass standard-render-pass))
  (lru-cache-clear (allocated-textures pass))
  (lru-cache-clear (allocated-materials pass))
  (lru-cache-clear (allocated-lights pass)))

(defmethod object-renderable-p ((material material) (pass standard-render-pass)) NIL)

(defgeneric material-block-type (standard-render-pass))

(defmethod bind-textures ((pass standard-render-pass))
  (call-next-method)
  (do-lru-cache (texture id (allocated-textures pass))
    ;; KLUDGE: textures might get deallocated but still be "stuck" in
    ;;         our cache here. If they aren't allocated, we simply remove
    ;;         them from the cache with the hope it won't be referenced.
    (cond ((gl-name texture)
           (bind texture id))
          (T
           (lru-cache-pop texture (allocated-textures pass))))))

(defmethod enable ((texture texture) (pass standard-render-pass))
  ;; KLUDGE: We effectively disable the cache here BECAUSE the texture binds are
  ;;         shared between standard-renderables and non, and the latter can
  ;;         thrash our bindings without our noticing. I'm not sure what the best
  ;;         solution is here at the moment, but this less-performant hack at
  ;;         least makes things work for now.
  (let ((id (or (lru-cache-push texture (allocated-textures pass))
                (lru-cache-id texture (allocated-textures pass)))))
    (when id
      (bind texture id))
    id))

(defmethod disable ((texture texture) (pass standard-render-pass))
  (lru-cache-pop texture (allocated-textures pass)))

(defmethod local-id ((texture texture) (pass standard-render-pass))
  (lru-cache-id texture (allocated-textures pass)))

(defmethod enable ((light light) (pass standard-render-pass))
  (let ((id (lru-cache-push light (allocated-lights pass))))
    (when (and id (allocated-p (light-block pass)))
      (with-buffer-tx (struct (light-block pass))
        (<- (aref (slot-value struct 'lights) id) light)
        (setf (light-count struct) (max (light-count struct) (1+ id)))))))

(defmethod disable ((light light) (pass standard-render-pass))
  (let ((id (lru-cache-pop light (allocated-lights pass))))
    (when (and id (allocated-p (light-block pass)))
      (with-buffer-tx (struct (light-block pass))
        (setf (light-type (aref (lights struct) id)) 0)
        (loop for i downfrom (1- (light-count struct)) to 0
              do (when (active-p (aref (lights struct) i))
                   (setf (light-count struct) (1+ i))
                   (return))
              finally (setf (light-count struct) 0))))))

(defmethod local-id ((light light) (pass standard-render-pass))
  (lru-cache-id light (allocated-lights pass)))

(defmethod leave ((light light) (pass standard-render-pass))
  (disable light pass))

(defmethod notice-update ((light light) (pass shader-pass)))

(defmethod notice-update ((light light) (pipeline pipeline))
  (loop for pass across (passes pipeline)
        do (notice-update light pass)))

(defmethod notice-update ((light light) (main (eql T)))
  (notice-update light (scene +main+)))

(defmethod notice-update ((light light) (pass standard-render-pass))
  (let ((id (lru-cache-id light (allocated-lights pass))))
    (when id
      (with-buffer-tx (struct (light-block pass))
        (<- (aref (slot-value struct 'lights) id) light)))))

(defmethod list-lights ((pass standard-render-pass))
  (lru-cache-list (allocated-lights pass)))

(defmethod enable ((material material) (pass standard-render-pass))
  (let ((id (lru-cache-push material (allocated-materials pass))))
    (when id
      (with-buffer-tx (struct (material-block pass))
        (<- (aref (slot-value struct 'materials) id) material)))
    (loop for texture across (textures material)
          do (enable texture pass))
    (when (double-sided-p material)
      (disable-feature :cull-face))))

(defmethod disable ((material material) (pass standard-render-pass))
  (lru-cache-pop material (allocated-materials pass))
  ;; We can eagerly disable textures here even if they are used by another
  ;; material, as the next ENABLE call for that material will re-enable the
  ;; required textures again.
  (loop for texture across (textures material)
        do (disable texture pass)))

(defmethod local-id ((material material) (pass standard-render-pass))
  (lru-cache-id material (allocated-materials pass)))

(defmethod notice-update ((material material) (pass shader-pass)))

(defmethod notice-update ((material material) (pipeline pipeline))
  (loop for pass across (passes pipeline)
        do (notice-update material pass)))

(defmethod notice-update ((material material) (main (eql T)))
  (notice-update material (scene +main+)))

(defmethod notice-update ((material material) (pass standard-render-pass))
  (let ((id (lru-cache-id material (allocated-materials pass))))
    (when id
      (with-buffer-tx (struct (material-block pass))
        (<- (aref (slot-value struct 'materials) id) material)))))

(defmethod render-with ((pass standard-render-pass) (material material) program)
  (error "Unsupported material~%  ~s~%for pass~%  ~s"
         material pass))

(define-handler (standard-render-pass material-changed) (changed-material)
  (notice-update changed-material standard-render-pass))

(define-shader-entity standard-renderable (renderable)
  (vertex-array ;; Backwards compatibility stub
   (vertex-arrays :initarg :vertex-arrays :initform #() :accessor vertex-arrays))
  (:shader-file (trial "renderer/standard-renderable.glsl"))
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod shared-initialize :after ((renderable standard-renderable) slots &key vertex-array)
  (cond (vertex-array
         (setf (vertex-array renderable) vertex-array))
        ((slot-boundp renderable 'vertex-array)
         (setf (vertex-array renderable) (slot-value renderable 'vertex-array)))))

(defmethod stage :after ((renderable standard-renderable) (area staging-area))
  (loop for vao across (vertex-arrays renderable)
        do (stage vao area)))

(define-transfer standard-renderable vertex-arrays)

(defmethod render-with :before ((pass shader-pass) (renderable standard-renderable) program)
  (prepare-pass-program pass program)
  (setf (uniform program "model_matrix") (model-matrix))
  (let ((inv (mat4)))
    (declare (dynamic-extent inv))
    (setf (uniform program "inv_model_matrix") (!minv inv (model-matrix)))))

(defmethod render ((renderable standard-renderable) (program shader-program))
  (declare (optimize speed))
  (loop for vao across (the simple-vector (vertex-arrays renderable))
        do (render vao program)))

(defmethod vertex-array ((renderable standard-renderable))
  (when (< 0 (length (vertex-arrays renderable)))
    (aref (vertex-arrays renderable) 0)))

(defmethod (setf vertex-array) ((resource resource) (renderable standard-renderable))
  (setf (vertex-arrays renderable) (vector resource)))

(define-shader-entity standard-animated-renderable (standard-renderable animated-entity)
  ()
  (:shader-file (trial "renderer/standard-animated-renderable.glsl"))
  (:inhibit-shaders (animated-entity :vertex-shader)
                    (standard-renderable :vertex-shader)))

(defmethod bind-palette ((pass standard-render-pass) (renderable standard-animated-renderable))
  (enable (palette-texture renderable) pass))

(defmethod render-with ((pass shader-pass) (renderable standard-animated-renderable) (program shader-program))
  (declare (optimize speed))
  (loop with morphs = (the simple-vector (morphs renderable))
        with skinning = (if (skinned-p renderable) 2 0)
        for i from 0
        for vao across (the simple-vector (vertex-arrays renderable))
        for material across (the simple-vector (materials renderable))
        for (morph . morphtex) = (if (< i (length morphs)) (aref morphs i) ())
        do (when (object-renderable-p material pass)
             (cond (morph
                    (bind (morph-data morph) program)
                    (setf (uniform program "morph_targets") (bind morphtex :texture6))
                    (setf (uniform program "animation") (+ skinning 1)))
                   (T
                    (setf (uniform program "morph_targets") 99)
                    (setf (uniform program "animation") (+ skinning 0))))
             (with-pushed-features
               (when (double-sided-p material)
                 (disable-feature :cull-face))
               (render vao program)))))

(define-shader-entity single-material-renderable (standard-renderable)
  ((material :initarg :material :accessor material)))

(defmethod stage :after ((renderable single-material-renderable) (area staging-area))
  (when (material renderable)
    (stage (material renderable) area)))

(define-transfer single-material-renderable material)

(defmethod render-with ((pass standard-render-pass) (object single-material-renderable) program)
  (when (and (material object) (object-renderable-p (material object) pass))
    (with-pushed-features
      (render-with pass (material object) program)
      (render (vertex-array object) program))))

(defmethod render :around ((renderable single-material-renderable) (program shader-program))
  (with-pushed-features
    (when (double-sided-p (material renderable))
      (disable-feature :cull-face))
    (call-next-method)))

(defmethod deregister :after ((renderable single-material-renderable) (pass standard-render-pass))
  (when (material renderable)
    (disable (material renderable) pass)))

(define-shader-entity per-array-material-renderable (standard-renderable)
  ((materials :initarg :materials :initform #() :accessor materials)))

(define-transfer per-array-material-renderable materials)

(defmethod stage :after ((renderable per-array-material-renderable) (area staging-area))
  (loop for material across (materials renderable)
        do (stage material area)))

(defmethod deregister :after ((renderable per-array-material-renderable) (pass standard-render-pass))
  (loop for material across (materials renderable)
        do (disable material pass)))

(defmethod render-with ((pass standard-render-pass) (renderable per-array-material-renderable) program)
  ;; KLUDGE: we can't do this in RENDER as we don't have access to the PASS variable, which we
  ;;         need to set the per-vao material. This will break user expectations, as the RENDER
  ;;         primary on the renderable is not invoked. Not sure how to fix this issue.
  (loop for vao across (vertex-arrays renderable)
        for material across (materials renderable)
        do (when (object-renderable-p material pass)
             (with-pushed-features
               (render-with pass material program)
               (render vao program)))))

(defmethod (setf mesh) :after ((meshes cons) (renderable per-array-material-renderable))
  (let ((arrays (make-array (length meshes))))
    (map-into arrays (lambda (m) (or (material m) (material 'none))) meshes)
    (setf (materials renderable) arrays)))

(define-shader-pass light-cache-render-pass (standard-render-pass)
  ((light-cache :initform (org.shirakumo.fraf.trial.space.kd-tree:make-kd-tree) :reader light-cache)
   (light-cache-dirty-p :initform T :accessor light-cache-dirty-p)
   (light-cache-location :initform (vec 0 0 0) :reader light-cache-location)
   (light-cache-distance-threshold :initform 10.0 :accessor light-cache-distance-threshold)
   (global-lights :initform (make-array 0 :adjustable T :fill-pointer T) :accessor global-lights)))

(defmethod object-renderable-p ((light light) (pass light-cache-render-pass)) T)

(defmethod clear :after ((pass light-cache-render-pass))
  (3ds:clear (light-cache pass))
  (setf (fill-pointer (global-lights pass)) 0))

(defmethod enter ((light located-light) (pass light-cache-render-pass))
  (3ds:enter light (light-cache pass))
  (setf (light-cache-dirty-p pass) T))

(defmethod enter ((light light) (pass light-cache-render-pass))
  (array-utils:vector-push-extend-new light (global-lights pass))
  (setf (light-cache-dirty-p pass) T))

(defmethod leave ((light located-light) (pass light-cache-render-pass))
  (3ds:leave light (light-cache pass))
  (lru-cache-pop light (allocated-lights pass))
  (setf (light-cache-dirty-p pass) T))

(defmethod leave ((light light) (pass light-cache-render-pass))
  (array-utils:vector-pop-element* (global-lights pass) light)
  (lru-cache-pop light (allocated-lights pass))
  (setf (light-cache-dirty-p pass) T))

(defmethod enable :after ((light located-light) (pass light-cache-render-pass))
  (3ds:enter light (light-cache pass))
  (setf (light-cache-dirty-p pass) T))

(defmethod disable :after ((light located-light) (pass light-cache-render-pass))
  (3ds:leave light (light-cache pass))
  (setf (light-cache-dirty-p pass) T))

(defmethod enable :after ((light light) (pass light-cache-render-pass))
  (unless (typep light 'located-light)
    (array-utils:vector-push-extend-new light (global-lights pass))))

(defmethod disable :after ((light light) (pass light-cache-render-pass))
  (unless (typep light 'located-light)
    (array-utils:vector-pop-element* (global-lights pass) light)))

(defmethod notice-update :after ((light located-light) (pass light-cache-render-pass))
  (3ds:update light (light-cache pass)))

(define-handler ((pass light-cache-render-pass) register) (changed-node)
  (when (typep changed-node 'light)
    (enter changed-node pass)))

(define-handler ((pass light-cache-render-pass) deregister) (changed-node)
  (when (typep changed-node 'light)
    (leave changed-node pass)))

(define-handler ((pass light-cache-render-pass) tick :before) ()
  (when (and (camera pass)
             (<= (light-cache-distance-threshold pass)
                 (vsqrdistance (focal-point (camera pass)) (light-cache-location pass))))
    (setf (light-cache-dirty-p pass) T)))

(defmethod revalidate-light-cache ((pass light-cache-render-pass))
  (let ((location (v<- (light-cache-location pass) (focal-point (camera pass))))
        (size (1- (lru-cache-size (allocated-lights pass)))))
    (with-buffer-tx (struct (light-block pass))
      (multiple-value-bind (nearest count) (org.shirakumo.fraf.trial.space.kd-tree:kd-tree-k-nearest
                                            size location (light-cache pass) :test #'active-p)
        (dotimes (i count)
          (when (active-p (aref nearest i))
            (enable (aref nearest i) pass))))
      (loop for light across (global-lights pass)
            do (when (active-p light)
                 (enable light pass)))))
  (setf (light-cache-dirty-p pass) NIL))

(defmethod render :before ((pass light-cache-render-pass) target)
  (when (light-cache-dirty-p pass)
    (revalidate-light-cache pass)))

;; FIXME: how do we know when lights moved or de/activated so we can update?

(define-shader-entity basic-entity (multi-mesh-entity per-array-material-renderable distance-lod-entity basic-node)
  ())

(define-shader-entity basic-physics-entity (rigidbody basic-entity)
  ())

(define-shader-entity basic-animated-entity (multi-mesh-entity standard-animated-renderable per-array-material-renderable distance-lod-entity basic-node)
  ())

(defmethod render-with ((pass standard-render-pass) (renderable basic-animated-entity) program)
  ;; KLUDGE: In order to access the morphs we once again duplicate functionality encoded
  ;;         in the multi-mesh-entity method for render-with.
  (loop with skinning = (if (skinned-p renderable) 2 0)
        with morphs = (the simple-vector (morphs renderable))
        for i from 0
        for vao across (the simple-vector (vertex-arrays renderable))
        for material across (the simple-vector (materials renderable))
        for (morph . morphtex) = (if (< i (length morphs)) (aref morphs i) ())
        do (when (object-renderable-p material pass)
             (with-pushed-features
               (render-with pass material program)
               (cond (morph
                      (bind (morph-data morph) program)
                      (setf (uniform program "morph_targets") (enable morphtex pass))
                      (setf (uniform program "animation") (+ skinning 1)))
                     (T
                      (setf (uniform program "morph_targets") 99)
                      (setf (uniform program "animation") (+ skinning 0))))
               (render vao program)))))

(define-shader-entity animated-physics-entity (rigidbody basic-animated-entity)
  ())
