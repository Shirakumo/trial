#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct particle
  (lifetime :vec2 :accessor lifetime))

(define-gl-struct (simple-particle (:include particle)
                                   (:layout-standard :vertex-buffer))
  (location :vec3 :accessor location)
  (velocity :vec3 :accessor velocity))

(define-shader-subject particle-emitter ()
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

(define-handler (particle-emitter tick) (ev)
  (let ((vbo (particle-buffer particle-emitter))
        (write-offset 0))
    (let ((data (struct-vector vbo)))
      (declare (type simple-vector data))
      (loop for read-offset from 0 below (live-particles particle-emitter)
            for particle = (aref data read-offset)
            do (when (< (vx2 (lifetime particle)) (vy2 (lifetime particle)))
                 (when (update-particle-state particle-emitter ev particle (aref data write-offset))
                   (incf write-offset))))
      (loop repeat (new-particle-count particle-emitter ev)
            while (< write-offset (length data))
            do (initial-particle-state particle-emitter ev (aref data write-offset))
               (incf write-offset))
      (setf (live-particles particle-emitter) write-offset)
      (update-buffer-data vbo T))))
