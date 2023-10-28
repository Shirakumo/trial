(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity debug-draw-text (standalone-shader-entity)
  ((text-vao :accessor text-vao)
   (text :accessor text)))

(defmethod initialize-instance :after ((draw debug-draw-text) &key)
  (setf (text draw) (make-array 256 :fill-pointer 0 :adjustable T :element-type 'single-float))
  (let ((vbo (make-instance 'vertex-buffer :buffer-data (text draw))))
    (setf (text-vao draw) (make-instance 'vertex-array :vertex-form :triangles :bindings
                                         `((,vbo :size 3 :offset 0 :stride 20 :index 0)
                                           (,vbo :size 2 :offset 12 :stride 20 :index 2))))))

(defmethod stage :after ((draw debug-draw-text) (area staging-area))
  (stage (text-vao draw) area)
  (stage (// 'trial 'ascii) area))

(defmethod render ((draw debug-draw-text) (program shader-program))
  (setf (uniform program "view_matrix") (view-matrix))
  (setf (uniform program "projection_matrix") (projection-matrix))
  (setf (uniform program "texture_image") 0)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (gl-name (// 'trial 'ascii)))
  (render-array (text-vao draw) :vertex-count (truncate (length (text draw)) 5)))

(define-class-shader (debug-draw-text :vertex-shader)
  "layout (location = 0) in vec3 position;
layout (location = 2) in vec2 i_uv;
out vec2 v_uv;

uniform sampler2D texture_image;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  mat4 modelView = view_matrix;
  modelView[0][0] = 1.0;
  modelView[0][1] = 0.0;
  modelView[0][2] = 0.0;
  modelView[1][0] = 0.0;
  modelView[1][1] = 1.0;
  modelView[1][2] = 0.0;
  modelView[2][0] = 0.0;
  modelView[2][1] = 0.0;
  modelView[2][2] = 1.0;

  gl_Position = projection_matrix * modelView * vec4(position, 1.0f);
  gl_Position.z = -1.0;
  v_uv = i_uv / textureSize(texture_image, 0).rg;
}")

(define-class-shader (debug-draw-text :fragment-shader)
  "in vec2 v_uv;
out vec4 color;
uniform sampler2D texture_image;

void main(){
  float fg_bg = texture(texture_image, v_uv, 0).r;
  color = mix(vec4(0,0,0,1), vec4(1,1,1,0.3), fg_bg);
}")

(defgeneric debug-draw (thing &key))

(define-shader-entity debug-draw (renderable)
  ((name :initform 'debug-draw)
   (points-vao :accessor points-vao)
   (points :accessor points)
   (lines-vao :accessor lines-vao)
   (lines :accessor lines)
   (text-render :accessor text-render)))

(defmethod initialize-instance :after ((draw debug-draw) &key)
  (setf (points draw) (make-array 64 :fill-pointer 0 :adjustable T :element-type 'single-float))
  (let ((vbo (make-instance 'vertex-buffer :buffer-data (points draw))))
    (setf (points-vao draw) (make-instance 'vertex-array :vertex-form :points :bindings
                                           `((,vbo :offset  0 :stride 24)
                                             (,vbo :offset 12 :stride 24)))))
  (setf (lines draw) (make-array 128 :fill-pointer 0 :adjustable T :element-type 'single-float))
  (let ((vbo (make-instance 'vertex-buffer :buffer-data (lines draw))))
    (setf (lines-vao draw) (make-instance 'vertex-array :vertex-form :lines :bindings
                                          `((,vbo :offset  0 :stride 24)
                                            (,vbo :offset 12 :stride 24)))))
  (setf (text-render draw) (make-instance 'debug-draw-text)))

(defmethod text-vao ((draw debug-draw))
  (text-vao (text-render draw)))

(defmethod text ((draw debug-draw))
  (text (text-render draw)))

(defmethod stage ((draw debug-draw) (area staging-area))
  (stage (points-vao draw) area)
  (stage (lines-vao draw) area)
  (stage (text-render draw) area))

(defmethod render ((draw debug-draw) (program shader-program))
  (setf (uniform program "view_matrix") (view-matrix))
  (setf (uniform program "projection_matrix") (projection-matrix))
  (render-array (points-vao draw) :vertex-count (truncate (length (points draw)) 6))
  (render-array (lines-vao draw) :vertex-count (truncate (length (lines draw)) 6))
  (render (text-render draw) T))

(defmacro define-debug-draw-function ((name type) args &body body)
  `(defun ,name (,@args (debug-draw (node 'debug-draw T)) (update T))
     (flet ((,name ()
              (unless debug-draw
                (setf debug-draw (enter-and-load (make-instance 'debug-draw) (scene +main+) +main+)))
              (let ((data (,type debug-draw)))
                (flet ((v (v)
                         (vector-push-extend (vx v) data)
                         (vector-push-extend (vy v) data)
                         (vector-push-extend (vz v) data)))
                  (declare (ignorable #'v))
                  ,@body))
              (when update
                (resize-buffer (caar (bindings (,(ecase type (points 'points-vao) (lines 'lines-vao) (text 'text-vao)) debug-draw))) T))))
       (if (current-p *context*)
           (,name)
           (with-eval-in-render-loop (T)
             (,name))))))

(defmethod debug-draw ((point vec2) &rest args &key &allow-other-keys)
  (apply #'debug-point (vxy_ point) args))

(defmethod debug-draw ((point vec3) &rest args &key &allow-other-keys)
  (apply #'debug-point point args))

(define-debug-draw-function (debug-point points) (point &key (color #.(vec 1 0 0)))
  (v point)
  (v color))

(define-debug-draw-function (debug-line lines) (a b &key (color #.(vec 1 0 0)) (color-a color) (color-b color))
  (v a)
  (v color-a)
  (v b)
  (v color-b))

(defmethod debug-draw ((string string) &rest args &key &allow-other-keys)
  (let ((point (getf args :point)))
    (remf args :point)
    (apply #'debug-text (or point (vec 0 0 0)) string args)))

(define-debug-draw-function (debug-text text) (point text &key (scale 0.2))
  (let ((i (print-ascii-text text data :start (fill-pointer data)
                                       :x (vx point)
                                       :y (vy point)
                                       :z (if (typep point 'vec2) 0.0 (vz point))
                                       :scale scale)))
    (setf (fill-pointer data) i)))

(defmethod debug-draw ((entity vertex-entity) &rest args &key &allow-other-keys)
  (apply #'debug-vertex-array (vertex-array entity) args))

(defmethod debug-draw :around ((entity transformed-entity) &rest args &key &allow-other-keys)
  (unless (getf args :transform)
    (setf (getf args :transform) (tmat (tf entity))))
  (apply #'call-next-method entity args))

(defmethod debug-draw ((data mesh-data) &rest args &key &allow-other-keys)
  (apply #'debug-triangles (reordered-vertex-data data '(location)) (index-data data) args))

(defmethod debug-draw ((primitive general-mesh) &rest args &key &allow-other-keys)
  (unless (getf args :transform)
    (setf (getf args :transform) (primitive-transform primitive)))
  (apply #'debug-triangles (general-mesh-vertices primitive) (general-mesh-faces primitive) args))

(defmethod debug-draw ((primitive primitive) &rest args &key &allow-other-keys)
  (unless (getf args :transform)
    (setf (getf args :transform) (primitive-transform primitive)))
  (let ((primitive (coerce-object primitive 'convex-mesh)))
    (apply #'debug-triangles (general-mesh-vertices primitive) (general-mesh-faces primitive) args)))

(define-debug-draw-function (debug-triangles lines) (vertices faces &key (color #.(vec 1 0 0)) (transform (model-matrix)))
  (let (prev)
    (flet ((lines (vec)
             (v vec)
             (v color)))
      (loop for e across faces
            for c = 0 then (mod (1+ c) 3)
            for i = (* e 3)
            do (let ((vec (vec (aref vertices (+ 0 i)) (aref vertices (+ 1 i)) (aref vertices (+ 2 i)) 1.0)))
                 (declare (dynamic-extent vec))
                 (n*m transform vec)
                 (case c
                   (0 (lines vec)
                    (setf prev vec))
                   (1 (lines vec)
                    (lines vec))
                   (T (lines vec)
                    (lines prev)
                    (lines vec))))))))

(define-debug-draw-function (debug-vertex-array lines) (vao &key (color #.(vec 1 0 0)) (transform (model-matrix)))
  (let ((count 0) prev pprev)
    (labels ((lines (vec)
               (v vec)
               (v color))
             (line-strip (vec)
               (case count
                 (0 (lines vec)
                  (setf count 1))
                 (1 (lines vec)
                  (lines vec))))
             (line-loop (vec)
               (error "Not implemented"))
             (triangles (vec)
               (case count
                 (0 (lines vec)
                  (setf prev vec)
                  (setf count 1))
                 (1 (lines vec)
                  (lines vec)
                  (setf count 2))
                 (T (lines vec)
                  (lines prev)
                  (lines vec)
                  (setf count 0))))
             (triangle-strip (vec)
               (case count
                 (0 (lines vec)
                  (setf pprev vec)
                  (setf count 1))
                 (1 (lines vec)
                  (setf prev vec)
                  (setf count 2))
                 (T (lines vec)
                  (lines prev)
                  (lines vec)
                  (lines pprev)
                  (shiftf pprev prev vec))))
             (triangle-fan (vec)
               (case count
                 (0 (lines prev)
                  (setf pprev vec)
                  (setf count 1))
                 (1 (lines vec)
                  (setf prev vec)
                  (setf count 2))
                 (T (lines vec)
                  (lines prev)
                  (lines vec)
                  (lines pprev)
                  (setf prev vec)))))
      (let ((vertex (ecase (vertex-form vao)
                      (:lines #'lines)
                      (:line-strip #'line-strip)
                      (:line-loop #'line-loop)
                      (:triangles #'triangles)
                      (:triangle-strip #'triangle-strip)
                      (:triangle-fan #'triangle-fan)))
            ebo vbo)
        (loop for binding in (bindings vao)
              do (case (buffer-type (unlist binding))
                   (:element-array-buffer
                    (setf ebo (buffer-data (unlist binding))))
                   (:array-buffer
                    (when (= 0 (getf (rest binding) :index 0))
                      (setf vbo binding)))))
        (destructuring-bind (buffer &key (size 3) (stride 0) (offset 0) &allow-other-keys) vbo
          (let ((data (buffer-data buffer)))
            (cond (ebo
                   (loop for e across ebo
                         for i = (+ (floor offset 4) (* (floor stride 4) e))
                         do (ecase size
                              (3
                               (let ((vec (vec (aref data (+ 0 i)) (aref data (+ 1 i)) (aref data (+ 2 i)) 1.0)))
                                 (declare (dynamic-extent vec))
                                 (n*m transform vec)
                                 (funcall vertex vec)))
                              (2
                               (let ((vec (vec (aref data (+ 0 i)) (aref data (+ 1 i)) 0.0 1.0)))
                                 (declare (dynamic-extent vec))
                                 (n*m transform vec)
                                 (funcall vertex vec))))))
                  (T
                   (loop for i from (floor offset 4) by (floor stride 4) below (length data)
                         do (ecase size
                              (3
                               (let ((vec (vec (aref data (+ 0 i)) (aref data (+ 1 i)) (aref data (+ 2 i)) 1.0)))
                                 (declare (dynamic-extent vec))
                                 (n*m transform vec)
                                 (funcall vertex vec)))
                              (2
                               (let ((vec (vec (aref data (+ 0 i)) (aref data (+ 1 i)) 0.0 1.0)))
                                 (declare (dynamic-extent vec))
                                 (n*m transform vec)
                                 (funcall vertex vec)))))))))))))

(defun debug-clear (&key (debug-draw (node 'debug-draw T)) (update T))
  (when debug-draw
    (setf (fill-pointer (points debug-draw)) 0)
    (setf (fill-pointer (lines debug-draw)) 0)
    (setf (fill-pointer (text debug-draw)) 0)
    (when update
      (resize-buffer (caar (bindings (points-vao debug-draw))) T)
      (resize-buffer (caar (bindings (lines-vao debug-draw))) T)
      (resize-buffer (caar (bindings (text-vao debug-draw))) T))))

(define-class-shader (debug-draw :vertex-shader)
  "layout (location = 0) in vec3 position;
layout (location = 1) in vec3 i_color;
out vec3 v_color;

uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  gl_Position = projection_matrix * view_matrix * vec4(position, 1.0f);
  v_color = i_color;
}")

(define-class-shader (debug-draw :fragment-shader)
  "in vec3 v_color;
out vec4 color;

void main(){
  color = vec4(v_color, 1);
}")
