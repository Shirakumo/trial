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
        (orientation (quat)))
    (declare (dynamic-extent orientation))
    (global-orientation light orientation)
    (setf (slot-value struct 'projection-matrix)
          (n*m (mortho -50.0 +50.0 -50.0 +50.0 1.0 200.0)
               (mlookat (nv+ (v* (q* orientation (direction light)) -100) point) point +vy3+)))))

(defmethod <- progn ((struct shadow-map-info) (light spot-light))
  (let ((location (vec3))
        (orientation (quat)))
    (declare (dynamic-extent location orientation))
    (global-location light location)
    (global-orientation light orientation)
    (setf (slot-value struct 'projection-matrix)
          (n*m (mperspective (* 2.0 (outer-radius light)) 1.0 1.0 1000.0)
               (mlookat location (v+ location (q* orientation (direction light))) +vy3+)))))

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

(define-shader-pass standard-shadows-pass (standard-render-pass)
  ((shadow-map :port-type fixed-input :reader shadow-map)
   (shadow-map-lights :accessor shadow-map-lights)
   (shadow-map-block :buffer T :reader shadow-map-block)
   (shadow-map-program :reader shadow-map-program)
   (shadow-map-framebuffer :reader shadow-map-framebuffer))
  (:shader-file (trial "renderer/standard-shadows-pass.glsl")))

(defmethod initialize-instance :after ((pass standard-shadows-pass) &key (max-shadow-casters 18) (shadow-map-resolution (or (setting :display :shadow-map-resolution) 2048)))
  (setf (shadow-map-lights pass) (make-array max-shadow-casters :initial-element NIL))
  (setf (slot-value pass 'shadow-map-block) (make-instance 'uniform-buffer :binding NIL :struct (make-instance 'shadow-map-block :size max-shadow-casters)))
  (let* ((texture (make-instance 'texture :width shadow-map-resolution :height shadow-map-resolution
                                          :depth max-shadow-casters
                                          :target :texture-2d-array
                                          :internal-format :depth-component
                                          :min-filter :nearest
                                          :mag-filter :nearest
                                          :wrapping :clamp-to-border
                                          :border-color #.(vec 1 1 1 1)))
         (framebuffer (make-instance 'framebuffer :attachments `((:depth-attachment ,texture :layer 0))
                                                  :clear-bits '(:depth-buffer)
                                                  :width shadow-map-resolution :height shadow-map-resolution)))
    (setf (slot-value pass 'shadow-map) texture)
    (setf (slot-value pass 'shadow-map-framebuffer) framebuffer))
  (let* ((*default-pathname-defaults* (pool-path 'trial "renderer/standard-shadow-map.glsl"))
         (shaders (loop with buffer = (glsl-toolkit:serialize (gl-source (shadow-map-block pass)))
                        for (type source) on (glsl-toolkit:preprocess *default-pathname-defaults* :include-resolution #'resolve-shader-include) by #'cddr
                        collect (make-instance 'shader :type type :source (format NIL "~a~%~a" buffer (glsl-toolkit:serialize source)))))
         (program (make-instance 'shader-program :shaders shaders :buffers (list (shadow-map-block pass)))))
    (setf (slot-value pass 'shadow-map-program) program)))

(defmethod stage :after ((pass standard-shadows-pass) (area staging-area))
  (stage (shadow-map pass) area)
  (stage (shadow-map-block pass) area)
  (stage (shadow-map-program pass) area)
  (stage (shadow-map-framebuffer pass) area))

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
  (cond ((and (cast-shadows-p light) (active-p light))
         (allocate-shadow-maps light pass)
         (when (shadow-map light)
           (if (allocated-p (shadow-map-block pass))
               (with-buffer-tx (struct (shadow-map-block pass))
                 (<- struct light))
               (setf (dirty-p (buffer-data (shadow-map-block pass))) T))))
        (T
         (deallocate-shadow-maps light pass))))

(defmethod render-frame :before ((pass standard-shadows-pass) frame)
  (let ((program (shadow-map-program pass))
        (map (gl-name (shadow-map pass)))
        (lights (shadow-map-lights pass)))
    (activate (shadow-map-framebuffer pass))
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
                do (when (typep object 'standard-renderable)
                     ;; TODO: we can also use in-view-p to eliminate objects
                     ;;       outside the shadow map purview.
                     (with-pushed-matrix ()
                       (apply-transforms object)
                       (setf (uniform program "animated") (if (typep object 'animated-entity) 1 0))
                       (render object program)))))))
    (activate (framebuffer pass))))
