(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity tile-layer (located-entity sized-entity renderable)
  ((vertex-array :initform (// 'trial 'fullscreen-square) :accessor vertex-array)
   (tilemap :initform NIL :accessor tilemap)
   (tileset :initform NIL :initarg :tileset :accessor tileset)
   (tile-data :initform NIL :initarg :tile-data :accessor tile-data)
   (visibility :initform 1.0 :accessor visibility)
   (tile-size :initform (vec 16 16) :accessor tile-size)
   (size :initarg :size :initform (vec 1 1) :accessor size)
   (isometric-p :initform NIL :initarg :isometric-p :accessor isometric-p))
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod initialize-instance :after ((layer tile-layer) &key tilemap (map-name 1) tile-data tile-size)
  (when tile-size (setf (tile-size layer) tile-size))
  (cond ((typep tilemap 'tilemap)
         (setf (tilemap layer) tilemap))
        (tile-data
         (setf (tilemap layer) (resource tile-data map-name)))
        (T
         (let* ((size (size layer))
                (data (etypecase tilemap
                        (null (make-array (floor (* (vx size) (vy size) 2))
                                          :element-type '(unsigned-byte 8)))
                        ((vector (unsigned-byte 8)) tilemap)
                        (pathname (alexandria:read-file-into-byte-vector tilemap))
                        (stream (alexandria:read-stream-content-into-byte-vector tilemap)))))
           (setf (bsize layer) (nv* (vxy_ (v* (size layer) (tile-size layer))) 0.5))
           (setf (tilemap layer) (make-instance 'texture :target :texture-2d
                                                         :width (floor (vx size))
                                                         :height (floor (vy size))
                                                         :pixel-data data
                                                         :pixel-type :unsigned-byte
                                                         :pixel-format :rg-integer
                                                         :internal-format :rg8ui
                                                         :min-filter :nearest
                                                         :mag-filter :nearest))))))

(defmethod observe-load-state ((layer tile-layer) (data tile-data) (state (eql :loaded)) (area staging-area))
  (let ((tileset (tileset (tilemap layer))))
    (setf (tileset layer) tileset)
    (setf (tile-size layer) (tile-size tileset))
    (setf (isometric-p layer) (isometric-p (tilemap layer)))
    (setf (vx (size layer)) (width (tilemap layer)))
    (setf (vy (size layer)) (height (tilemap layer)))
    (setf (bsize layer) (nv* (vxy_ (v* (size layer) (tile-size tileset))) 0.5))))

(defmethod stage ((layer tile-layer) (area staging-area))
  (when (tile-data layer)
    (register-load-observer area layer (tile-data layer)))
  (stage (vertex-array layer) area)
  (when (tilemap layer) (stage (tilemap layer) area))
  (when (tileset layer) (stage (tileset layer) area)))

(defmethod (setf tile-size) ((number real) (layer tile-layer))
  (vsetf (tile-size layer) number number))

(defmethod pixel-data ((layer tile-layer))
  (pixel-data (tilemap layer)))

(defmethod (setf pixel-data) ((data vector) (layer tile-layer))
  (replace (pixel-data (tilemap layer)) data)
  (%update-tile-layer layer))

(defmethod resize ((layer tile-layer) w h)
  (let ((size (vec2 (floor w (vx (tile-size layer))) (floor h (vy (tile-size layer))))))
    (unless (v= size (size layer))
      (setf (size layer) size))))

(defmethod (setf size) :before (value (layer tile-layer))
  (let* ((nw (floor (vx2 value)))
         (nh (floor (vy2 value)))
         (ow (floor (vx2 (size layer))))
         (oh (floor (vy2 (size layer))))
         (tilemap (pixel-data layer))
         (new-tilemap (make-array (* 4 nw nh) :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
    ;; Allocate resized and copy data over. Slow!
    (ignore-errors
     (dotimes (y (min nh oh))
       (dotimes (x (min nw ow))
         (let ((npos (* 2 (+ x (* y nw))))
               (opos (* 2 (+ x (* y ow)))))
           (dotimes (c 2)
             (setf (aref new-tilemap (+ npos c)) (aref tilemap (+ opos c))))))))
    ;; Resize the tilemap. Internal mechanisms should take care of re-mapping the pixel data.
    (when (gl-name (tilemap layer))
      (setf (pixel-data (tilemap layer)) new-tilemap)
      (resize (tilemap layer) nw nh))))

(defmethod (setf size) :after (value (layer tile-layer))
  (setf (bsize layer) (nv* (vxy_ (v* value (tile-size layer))) 0.5)))

(defmacro %with-layer-xy ((layer location) &body body)
  `(let ((x (floor (+ (- (vx ,location) (vx (location ,layer))) (vx (bsize ,layer))) (vx (tile-size layer))))
         (y (floor (+ (- (vy ,location) (vy (location ,layer))) (vy (bsize ,layer))) (vy (tile-size layer)))))
     (when (and (< -1.0 x (vx (size ,layer)))
                (< -1.0 y (vy (size ,layer))))
       ,@body)))

(defmethod tile ((location vec2) (layer tile-layer))
  (%with-layer-xy (layer location)
    (let ((pos (* 2 (+ x (* y (truncate (vx (size layer))))))))
      (list (aref (pixel-data layer) pos) (aref (pixel-data layer) (1+ pos))))))

(defmethod (setf tile) (value (location vec2) (layer tile-layer))
  (let ((dat (pixel-data layer))
        (texture (tilemap layer)))
    (%with-layer-xy (layer location)
      (let ((idx (* 2 (+ x (* y (truncate (vx (size layer))))))))
        (setf (aref dat (+ 0 idx)) (car value))
        (setf (aref dat (+ 1 idx)) (cdr value))))
    (%update-tile-layer layer)
    #++ ;; TODO: Optimize
    (update-buffer-data texture dat :data-start pos :x x :y y :width 1 :height 1))
  value)

(defmethod tile ((location vec3) (layer tile-layer))
  (tile (vxy location) layer))

(defmethod (setf tile) (value (location vec3) (layer tile-layer))
  (setf (tile (vxy location) layer) value))

(defun %update-tile-layer (layer)
  (let ((width (truncate (vx (size layer))))
        (height (truncate (vy (size layer)))))
    (update-buffer-data (tilemap layer) (pixel-data layer) :width width :height height)))

(defmethod clear ((layer tile-layer))
  (let ((dat (pixel-data layer)))
    (dotimes (i (truncate (* 2 (vx (size layer)) (vy (size layer)))))
      (setf (aref dat i) 0))
    (%update-tile-layer layer)))

(defmethod bind-textures :after ((layer tile-layer))
  (bind (tilemap layer) :texture0)
  (bind (tileset layer) :texture1))

(defmethod render ((layer tile-layer) (program shader-program))
  (when (< 0.0 (visibility layer))
    (setf (uniform program "visibility") (visibility layer))
    (setf (uniform program "map_size") (size layer))
    (setf (uniform program "tile_size") (tile-size layer))
    (setf (uniform program "projection_matrix") *projection-matrix*)
    (setf (uniform program "view_matrix") *view-matrix*)
    (setf (uniform program "model_matrix") *model-matrix*)
    (setf (uniform program "isometric") (if (isometric-p layer) 1 0))
    (setf (uniform program "tilemap") 0)
    (setf (uniform program "tileset") 1)
    (render (vertex-array layer) program)))

(define-class-shader (tile-layer :vertex-shader)
  "layout (location = TRIAL_V_LOCATION) in vec3 vertex;
layout (location = TRIAL_V_UV) in vec2 vertex_uv;
uniform mat4 view_matrix;
uniform mat4 model_matrix;
uniform mat4 projection_matrix;
uniform vec2 map_size;
uniform vec2 tile_size = vec2(16);
uniform usampler2D tilemap;
uniform int isometric = 0;
out vec2 pix_uv;
out vec2 world_pos;

vec2 rotate(vec2 uv) {
  return vec2(uv.x + uv.y, uv.y - uv.x);
}

void main() {
  vec2 vert = (vertex.xy * map_size * tile_size * 0.5);

  if(0 < isometric) {
    vert.x *= 0.5;
    vert = rotate(vert) * 0.5; // TODO: Why does it double the size without this halving?
    vert.x *= 2.0;
  }

  vec4 temp = model_matrix * vec4(vert, 0, 1);

  gl_Position = projection_matrix * view_matrix * temp;
  world_pos = temp.xy;
  pix_uv = vertex_uv * map_size;
}")

(define-class-shader (tile-layer :fragment-shader)
  "
uniform usampler2D tilemap;
uniform sampler2D tileset;
uniform float visibility = 1.0;
uniform vec2 map_size;
uniform vec2 tile_size = vec2(16);
uniform int isometric = 0;
in vec2 pix_uv;
in vec2 world_pos;
out vec4 color;

vec4 fetch_texel_for(vec2 uv, ivec2 map_xy) {
  // Look up tileset index from tilemap and pixel from tileset.
  uvec2 tile = texelFetch(tilemap, map_xy, 0).rg;
  ivec2 pixel_xy = ivec2((uv - floor(uv)) * tile_size);
  ivec2 tile_xy = ivec2(tile) * ivec2(tile_size) + pixel_xy;

  return texelFetch(tileset, tile_xy, 0);
}

bool is_inside_map(ivec2 map_xy) {
  return 0 <= map_xy.x && 0 <= map_xy.y && map_xy.x < map_size.x && map_xy.y < map_size.y;
}

bool is_pixel_edge(vec2 uv) {
  ivec2 tile_xy = ivec2(uv * tile_size);
  return (
    tile_xy.x <= 0 ||
    tile_xy.y <= 0 ||
    tile_size.x - 1 <= tile_xy.x ||
    tile_size.y - 1 <= tile_xy.y
  );
}

vec2 shift_to_diamond(vec2 uv, ivec2 map_xy) {
  vec2 ruv = uv * 0.5;
  ruv = vec2(ruv.x + ruv.y, ruv.y - ruv.x); // Rotate
  ruv.x += 0.5;

  if((map_xy.y + map_xy.x) % 2 == 0)
    ruv += 0.5;

  return ruv;
}

void main() {
  // Calculate tilemap index and pixel offset within tile.
  ivec2 map_xy = ivec2(pix_uv);

  if(0 < isometric) { // Isometric view.

    vec2 tile_uv = pix_uv - floor(pix_uv);
    vec2 edge_uv = tile_uv;

    // Distances to the nearest edge of the tile for each axis.
    if(0.5 < edge_uv.x)
      edge_uv.x = 1.0 - edge_uv.x;
    if(0.5 < edge_uv.y)
      edge_uv.y = 1.0 - edge_uv.y;

    color = vec4(0);

    if(is_inside_map(map_xy)) { // Get current tile pixel color.
      vec2 uv = shift_to_diamond(pix_uv, map_xy);
      color = fetch_texel_for(uv, map_xy);
    }

    if(color.a == 0 && is_pixel_edge(tile_uv)) {
      // Get the edge pixel from the neighbour so that it doesn't cut.
      // Note: The x-axis advances to bottom right and y-axis to top right.

      if(tile_uv.x < tile_uv.y) { // Top half.
        if(edge_uv.x < edge_uv.y)
          map_xy.x -= 1;
        else
          map_xy.y += 1;
      }
      else if(edge_uv.x < edge_uv.y) { // Bottom half.
        map_xy.x += 1;
      }
      else {
        map_xy.y -= 1;
      }

      if(is_inside_map(map_xy)) {
        vec2 uv = shift_to_diamond(pix_uv, map_xy);
        color = fetch_texel_for(uv, map_xy);
      }
    }
  }
  else { // Square tilemap.
    color = fetch_texel_for(pix_uv, map_xy);
  }

  color.a *= visibility;
}")
