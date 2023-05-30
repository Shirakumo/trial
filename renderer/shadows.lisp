#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct shadow-map-block
  (size NIL :initarg :size :initform 8 :reader size)
  (dirty-p NIL :initform NIL :accessor dirty-p)
  (shadow-sample-count :int :initform 4 :accessor shadow-sample-count)
  (shadow-sample-spread :float :initform 0.0002 :accessor shadow-sample-spread)
  (light-space-matrices (:array :mat4 size)))

(defmethod transfer-to progn ((struct shadow-map-block) (light directional-light))
  (let ((cloc (location (camera (scene +main+)))))
    (setf (elt (slot-value struct 'light-space-matrices) (shadow-map light))
          (n*m (mortho -100.0 +100.0 -100.0 +100.0 1.0 1000.0)
               (mlookat cloc (v+ cloc (direction light)) +vy3+)))))

(defmethod transfer-to progn ((struct shadow-map-block) (light spot-light))
  (setf (elt (slot-value struct 'light-space-matrices) (shadow-map light))
        (n*m (mperspective (outer-radius light) 1.0 1.0 100.0)
             (mlookat (location light) (v+ (location light) (direction light)) +vy3+))))

(defmethod transfer-to progn ((struct shadow-map-block) (light point-light))
  ;; FIXME: point lights need to reserve 6 textures, not one
  (let ((proj (mperspective 90.0 1.0 1.0 100.0)))
    (setf (elt (slot-value struct 'light-space-matrices) (+ 0 (shadow-map light)))
          (n*m proj (mlookat (location light) (nv+ (vec +1 0 0) (location light)) +vy3+)))
    (setf (elt (slot-value struct 'light-space-matrices) (+ 1 (shadow-map light)))
          (n*m proj (mlookat (location light) (nv+ (vec -1 0 0) (location light)) +vy3+)))
    (setf (elt (slot-value struct 'light-space-matrices) (+ 2 (shadow-map light)))
          (n*m proj (mlookat (location light) (nv+ (vec 0 +1 0) (location light)) +vy3+)))
    (setf (elt (slot-value struct 'light-space-matrices) (+ 3 (shadow-map light)))
          (n*m proj (mlookat (location light) (nv+ (vec 0 -1 0) (location light)) +vy3+)))
    (setf (elt (slot-value struct 'light-space-matrices) (+ 4 (shadow-map light)))
          (n*m proj (mlookat (location light) (nv+ (vec 0 0 +1) (location light)) +vy3+)))
    (setf (elt (slot-value struct 'light-space-matrices) (+ 5 (shadow-map light)))
          (n*m proj (mlookat (location light) (nv+ (vec 0 0 -1) (location light)) +vy3+)))))

(define-shader-pass standard-shadows-pass (standard-render-pass)
  ((allocated-shadow-casters :accessor allocated-shadow-casters)
   (shadow-map :port-type fixed-input :reader shadow-map)
   (shadow-map-block :reader shadow-map-block)
   (shadow-map-program :reader shadow-map-program)
   (shadow-map-framebuffer :reader shadow-map-framebuffer))
  (:shader-file (trial "standard-shadows-pass.glsl")))

(defmethod initialize-instance :after ((pass standard-shadows-pass) &key (max-shadow-casters 8) (shadow-map-resolution 2048))
  (setf (allocated-shadow-casters pass) (make-lru-cache max-shadow-casters))
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
  (let* ((*default-pathname-defaults* (pool-path 'trial "standard-shadow-map.glsl"))
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
  (lru-cache-clear (allocated-shadow-casters pass)))

(defmethod enter :after ((light light) (pass standard-shadows-pass))
  (when (cast-shadows-p light)
    (lru-cache-push light (allocated-shadow-casters pass))
    ;; FIXME: needd to unset shadow-map id in evicted light.
    (setf (shadow-map light) (lru-cache-id light (allocated-shadow-casters pass)))
    (if (allocated-p (shadow-map-block pass))
        (with-buffer-tx (struct (shadow-map-block pass))
          (transfer-to struct light))
        (setf (dirty-p (struct (shadow-map-block pass))) T))))

(defmethod leave :after ((light light) (pass standard-shadows-pass))
  (when (cast-shadows-p light)
    (lru-cache-pop light (allocated-shadow-casters pass))
    (setf (shadow-map light) NIL)))

(defmethod notice-update :after ((light light) (pass standard-shadows-pass))
  (cond ((cast-shadows-p light)
         (lru-cache-push light (allocated-shadow-casters pass))
         (setf (shadow-map light) (lru-cache-id light (allocated-shadow-casters pass)))
         (if (allocated-p (shadow-map-block pass))
             (with-buffer-tx (struct (shadow-map-block pass))
               (transfer-to struct light))
             (setf (dirty-p (struct (shadow-map-block pass))) T)))
        (T
         (lru-cache-pop light (allocated-shadow-casters pass))
         (setf (shadow-map light) NIL))))

(defmethod compute-shader (type (pass standard-shadows-pass) object)
  (if (or (typep object 'standard-renderable)
          (subtypep object 'standard-renderable))
      (let ((next (call-next-method)))
        (when next
          (list* (gl-source (shadow-map-block pass))
                 next)))
      (call-next-method)))

(defmethod buffers ((pass standard-shadows-pass))
  (list* (shadow-map-block pass) (call-next-method)))

(defmethod render-frame :before ((pass standard-shadows-pass) frame)
  (let ((program (shadow-map-program pass))
        (map (gl-name (shadow-map pass))))
    (activate (shadow-map-framebuffer pass))
    (activate program)
    (when (dirty-p (struct (shadow-map-block pass)))
      (with-buffer-tx (struct (shadow-map-block pass))
        (setf (shadow-sample-count struct) 4)
        (setf (shadow-sample-spread struct) 0.0002)
        (do-lru-cache (light id (allocated-shadow-casters pass))
          (transfer-to struct light))
        (setf (dirty-p struct) NIL)))
    (do-lru-cache (light id (allocated-shadow-casters pass))
      (setf (uniform program "shadow_map_id") id)
      (%gl:framebuffer-texture-layer :framebuffer :depth-attachment map 0 id)
      (loop for (object) across frame
            do (render object program)))
    (activate (framebuffer pass))))
