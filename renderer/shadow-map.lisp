(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct shadow-map-info
  (sample-count :int :initform 4 :accessor sample-count)
  (sample-spread :float :initform 0.0002 :accessor sample-spread)
  (far-plane :float :initform 1000.0 :accessor far-plane)
  (projection-matrix :mat4))

(define-gl-struct shadow-map-block
  (size NIL :initarg :size :initform 18 :reader size)
  (dirty-p NIL :initform NIL :accessor dirty-p)
  (shadow-info (:array (:struct shadow-map-info) size) :accessor shadow-info))

(defgeneric required-shadow-maps (light))
(defmethod required-shadow-maps ((light light)) 0)
(defmethod required-shadow-maps ((light point-light)) 6)
(defmethod required-shadow-maps ((light directional-light)) 1)

;; TODO: implement frustum clipping to ensure the view of the light is as tight as possible

(defmethod <- progn ((struct shadow-map-info) (light light))
  (setf (sample-count struct) 4)
  (setf (sample-spread struct) 0.0002))

(defmethod <- progn ((struct shadow-map-info) (light directional-light))
  (let ((point (focal-point (camera (scene +main+))))
        (orientation (quat)) (a (mat4)) (b (mat4)))
    (declare (dynamic-extent orientation a b))
    (global-orientation light orientation)
    (setf (slot-value struct 'projection-matrix)
          (n*m (nmortho a -50.0 +50.0 -50.0 +50.0 1.0 200.0)
               (nmlookat b (nv+ (nv* (q* orientation (direction light)) -100) point) point +vy3+)))))

(defmethod <- progn ((struct shadow-map-info) (light spot-light))
  (let ((location (vec3))
        (orientation (quat))
        (a (mat4)) (b (mat4)))
    (declare (dynamic-extent location orientation a b))
    (global-location light location)
    (global-orientation light orientation)
    (setf (slot-value struct 'projection-matrix)
          (n*m (nmperspective a (* 2.0 (outer-radius light)) 1.0 1.0 1000.0)
               (nmlookat b location (v+ location (q* orientation (direction light))) +vy3+)))))

(defmethod <- progn ((struct shadow-map-info) (light point-light))
  (setf (far-plane struct) 1000.0))

(defmethod <- progn ((struct shadow-map-block) (light light))
  (<- (aref (shadow-info struct) (shadow-map light)) light))

(defmethod <- progn ((struct shadow-map-block) (light point-light))
  (let ((proj (mperspective 90.0 1.0 1.0 1000.0))
        (structs (shadow-info struct)))
    (setf (slot-value (aref structs (+ 0 (shadow-map light))) 'projection-matrix)
          (n*m proj (mlookat (location light) (nv+ (vec +1 0 0) (location light)) (vec 0 -1 0))))
    (setf (slot-value (aref structs (+ 1 (shadow-map light))) 'projection-matrix)
          (n*m proj (mlookat (location light) (nv+ (vec -1 0 0) (location light)) (vec 0 -1 0))))
    (setf (slot-value (aref structs (+ 2 (shadow-map light))) 'projection-matrix)
          (n*m proj (mlookat (location light) (nv+ (vec 0 +1 0) (location light)) (vec 0 0 +1))))
    (setf (slot-value (aref structs (+ 3 (shadow-map light))) 'projection-matrix)
          (n*m proj (mlookat (location light) (nv+ (vec 0 -1 0) (location light)) (vec 0 0 -1))))
    (setf (slot-value (aref structs (+ 4 (shadow-map light))) 'projection-matrix)
          (n*m proj (mlookat (location light) (nv+ (vec 0 0 +1) (location light)) (vec 0 -1 0))))
    (setf (slot-value (aref structs (+ 5 (shadow-map light))) 'projection-matrix)
          (n*m proj (mlookat (location light) (nv+ (vec 0 0 -1) (location light)) (vec 0 -1 0))))))

(define-shader-pass shadow-render-pass (single-shader-pass standard-environment-pass)
  ((shadow-map-block :buffer T :initarg :shadow-map-block :reader shadow-map-block)
   (shadow-map :port-type output :texspec (:target :texture-2d-array
                                           :internal-format :depth-component
                                           :min-filter :nearest :mag-filter :nearest
                                           :wrapping :clamp-to-border
                                           :border-color #.(vec4 1))
               :attachment :depth-attachment :accessor shadow-map))
  (:shader-file (trial "renderer/standard-shadow-map.glsl")))

(defmethod object-renderable-p ((material material) (pass shadow-render-pass))
  (not (transparent-p material)))

(define-shader-pass standard-shadows-pass (standard-render-pass)
  ((shadow-map :port-type fixed-input :reader shadow-map)
   (shadow-map-lights :accessor shadow-map-lights)
   (shadow-map-block :buffer T :reader shadow-map-block)
   (shadow-render-pass :reader shadow-render-pass))
  (:shader-file (trial "renderer/standard-shadows-pass.glsl")))

(defmethod initialize-instance :after ((pass standard-shadows-pass) &key (max-shadow-casters 18) (shadow-map-resolution (setting* 2048 :display :shadow-map-resolution)))
  (setf (shadow-map-lights pass) (make-array max-shadow-casters :initial-element NIL))
  (setf (slot-value pass 'shadow-map-block) (make-instance 'uniform-buffer :binding NIL :struct (make-instance 'shadow-map-block :size max-shadow-casters)))
  (let* ((render (make-instance 'shadow-render-pass :shadow-map-block (shadow-map-block pass)))
         (texture (make-port-texture (port render 'shadow-map) shadow-map-resolution shadow-map-resolution max-shadow-casters)))
    (setf (slot-value render 'shadow-map) texture)
    (setf (slot-value pass 'shadow-map) texture)
    (setf (framebuffer render) (make-pass-framebuffer render))
    (setf (slot-value pass 'shadow-render-pass) render)))

(defmethod stage :after ((pass standard-shadows-pass) (area staging-area))
  (stage (shadow-map pass) area)
  (stage (shadow-map-block pass) area)
  (stage (shadow-render-pass pass) area))

(defmethod clear :after ((pass standard-shadows-pass))
  (loop for i from 0 below (length (shadow-map-lights pass))
        for light = (aref (shadow-map-lights pass) i)
        do (when (and light (cast-shadows-p light))
             (setf (shadow-map light) #xFFFF)
             (setf (aref (shadow-map-lights pass) i) NIL))))

(defun allocate-shadow-maps (light pass)
  (unless (shadow-map light)
    (let* ((req (required-shadow-maps light))
           (lights (shadow-map-lights pass))
           ;; Yeah this is O(n), but honestly we're never gonna have a ton of shadow maps.
           (index (loop with start = 0
                        for end from 1 to (length lights)
                        do (cond ((aref lights (1- end))
                                  (setf start end))
                                 ((<= req (- end start))
                                  (return start))))))
      (cond (index
             (loop for i from index below (+ index req)
                   do (setf (aref lights i) light))
             (setf (shadow-map light) index)
             (if (allocated-p (shadow-map-block pass))
                 (with-buffer-tx (struct (shadow-map-block pass))
                   (<- (aref (shadow-info struct) (shadow-map light)) light))
                 (setf (dirty-p (buffer-data (shadow-map-block pass))) T)))
            (T
             (v:warn :trial.renderer.shadow-map "Failed to allocate shadow maps for ~s: no more space" light)
             (setf (shadow-map light) NIL))))))

(defun deallocate-shadow-maps (light pass)
  (when (shadow-map light)
    (loop repeat (required-shadow-maps light)
          for i from (shadow-map light)
          do (setf (aref (shadow-map-lights pass) i) NIL))
    (setf (shadow-map light) NIL)))

(defmethod enable :before ((light light) (pass standard-shadows-pass))
  (when (cast-shadows-p light)
    (allocate-shadow-maps light pass)))

(defmethod disable :after ((light light) (pass standard-shadows-pass))
  (when (cast-shadows-p light)
    (deallocate-shadow-maps light pass)))

(defmethod notice-update :before ((light light) (pass standard-shadows-pass))
  (cond ((and (cast-shadows-p light) (active-p light)
              (setting* T :display :shadows))
         (allocate-shadow-maps light pass)
         (when (shadow-map light)
           (if (allocated-p (shadow-map-block pass))
               (with-buffer-tx (struct (shadow-map-block pass))
                 (<- struct light))
               (setf (dirty-p (buffer-data (shadow-map-block pass))) T))))
        (T
         (deallocate-shadow-maps light pass))))

(defmethod cast-shadows-p ((entity renderable)) NIL)
(defmethod cast-shadows-p ((entity standard-renderable)) T)

(defmethod revalidate-light-cache :after ((pass standard-shadows-pass))
  (setf (dirty-p (buffer-data (shadow-map-block pass))) T))

(defmethod render-frame :before ((pass standard-shadows-pass) frame)
  (when (setting* T :display :shadows)
    (let* ((render (shadow-render-pass pass))
           (program (shader-program render))
           (map (gl-name (shadow-map pass)))
           (lights (shadow-map-lights pass)))
      (activate (framebuffer render))
      (activate program)
      (when (dirty-p (buffer-data (shadow-map-block pass)))
        (with-buffer-tx (struct (shadow-map-block pass))
          (loop for light across lights
                do (when light (<- struct light)))
          (setf (dirty-p struct) NIL)))
      (with-depth-mask T
        (dotimes (id (length lights))
          (when (and (aref lights id) (in-view-p (aref lights id) T))
            (setf (uniform program "shadow_map_id") id)
            (%gl:framebuffer-texture-layer :framebuffer :depth-attachment map 0 id)
            (gl:clear :depth-buffer)
            (loop for (object) across frame
                  do (when (cast-shadows-p object)
                       ;; TODO: we can also use in-view-p to eliminate objects
                       ;;       outside the shadow map purview.
                       (with-pushed-matrix ()
                         (apply-transforms object)
                         (render-with render object program)))))))
      (activate (framebuffer pass)))))


(untrace render render-with)

(print (compute-applicable-methods #'render (list (type-prototype 'basic-animated-entity)
                                                  (type-prototype 'shader-program))))
