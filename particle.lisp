#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct (particle (:layout-standard :vertex-buffer))
  (lifetime :vec2 :accessor lifetime))

(define-shader-entity particle-emitter (listener)
  ((live-particles :initform 0 :accessor live-particles)
   (vertex-array :accessor vertex-array)
   (particle-buffer :initarg :particle-buffer :accessor particle-buffer)))

(defmethod initialize-instance :after ((emitter particle-emitter) &key particle-mesh particle-buffer)
  (setf (vertex-array emitter)
        (add-vertex-bindings
         particle-buffer
         (change-class particle-mesh 'vertex-array))))

(defmethod paint ((emitter particle-emitter) pass)
  (let ((vao (vertex-array emitter)))
    (gl:bind-vertex-array (gl-name vao))
    (%gl:draw-elements-instanced (vertex-form vao) (size vao) :unsigned-int 0 (live-particles emitter))))

(defgeneric initial-particle-state (emitter tick particle))
(defgeneric update-particle-state (emitter tick input output))
(defgeneric new-particle-count (emitter tick)) ; => N

(defmethod handle ((ev tick) (emitter particle-emitter))
  (let ((vbo (particle-buffer emitter))
        (write-offset 0))
    (let ((data (struct-vector vbo)))
      (declare (type simple-vector data))
      (loop for read-offset from 0 below (live-particles emitter)
            for particle = (aref data read-offset)
            do (when (< (vx2 (lifetime particle)) (vy2 (lifetime particle)))
                 (when (update-particle-state emitter ev particle (aref data write-offset))
                   (incf write-offset))))
      (loop repeat (new-particle-count emitter ev)
            while (< write-offset (length data))
            do (initial-particle-state emitter ev (aref data write-offset))
               (incf write-offset))
      (setf (live-particles emitter) write-offset)
      (update-buffer-data vbo T))))

(define-gl-struct (simple-particle (:include particle)
                                   (:layout-standard :vertex-buffer))
  (location :vec3 :accessor location)
  (velocity :vec3 :accessor velocity))

(define-shader-entity simple-particle-emitter (particle-emitter)
  ())

(defmethod initial-particle-state :before ((emitter simple-particle-emitter) tick particle)
  (setf (location particle) (vec 0 0 0)))

(defmethod update-particle-state ((emitter simple-particle-emitter) tick particle output)
  (setf (location output) (v+ (location particle) (velocity particle)))
  (let ((life (lifetime particle)))
    (incf (vx2 life) (dt tick))
    (setf (lifetime output) life)
    (< (vx2 life) (vy2 life))))

(defmethod paint :before ((emitter simple-particle-emitter) (pass shader-pass))
  (let ((program (shader-program-for-pass pass emitter)))
    (setf (uniform program "view_matrix") (view-matrix))
    (setf (uniform program "projection_matrix") (projection-matrix))
    (setf (uniform program "model_matrix") (model-matrix))))

(define-class-shader (simple-particle-emitter :vertex-shader)
  "layout (location = 0) in vec3 vtx_location;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  vec3 position = vtx_location + location;
  gl_Position = projection_matrix * view_matrix * model_matrix * vec4(position, 1.0f);
}")
