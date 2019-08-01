#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; TODO: Make the attributes extensible
(defun make-particle-storage (particle-geometry &key (max-particles 1024) (vertex-attributes T))
  (let* ((vao (change-class particle-geometry 'vertex-array :vertex-attributes vertex-attributes))
         (idx (loop for i from 0
                    for binding in (bindings vao)
                    when (listp binding)
                    maximize (getf (rest binding) :index i)))
         (data (make-array (* (+ 3 3 1) max-particles) :element-type 'single-float
                                                       :initial-element 0.0f0))
         (vbo (make-instance 'vertex-buffer :buffer-data data :data-usage :stream-draw)))
    (push (list vbo :index (+ 1 idx) :offset (* 0 4) :size 3 :stride (* 7 4) :instancing 1) (bindings vao))
    (push (list vbo :index (+ 2 idx) :offset (* 3 4) :size 3 :stride (* 7 4) :instancing 1) (bindings vao))
    (push (list vbo :index (+ 3 idx) :offset (* 6 4) :size 1 :stride (* 7 4) :instancing 1) (bindings vao))
    ;; location: vec3, velocity: vec3, lifetime: float
    (values vao (+ 1 idx))))

(define-shader-subject particle-emitter (bakable)
  ((live-particles :initform 0 :accessor live-particles)
   (vertex-array :initarg :vertex-array :accessor vertex-array)
   (vertex-buffer  :initform NIL :accessor vertex-buffer)))

(defmethod bake ((emitter particle-emitter))
  (let ((array (etypecase (vertex-array emitter)
                 (mesh (input (vertex-array emitter)))
                 (vertex-array (vertex-array emitter)))))
    (setf (vertex-buffer emitter)
          (loop for binding in (bindings array)
                do (when (and (listp binding)
                              (eql 1 (getf (rest binding) :instancing)))
                     (return (first binding)))
                finally (error "No instanced binding found in ~a" array)))))

(defmethod paint ((emitter particle-emitter) pass)
  (let ((vao (vertex-array emitter)))
    (gl:bind-vertex-array (gl-name vao))
    (%gl:draw-elements-instanced (vertex-form vao) (size vao) :unsigned-int 0 (live-particles emitter))))

(defgeneric initial-particle-state (emitter tick location velocity)) ; => lifetime
(defgeneric update-particle-state (emitter tick location velocity lifetime)) ; => lifetime
(defgeneric new-particle-count (emitter tick)) ; => N

(define-handler (particle-emitter tick) (ev)
  (let* ((vbo (vertex-buffer particle-emitter))
         (data (buffer-data vbo))
         (location (vec 0 0 0))
         (velocity (vec 0 0 0))
         (lifetime 0.0f0)
         (write-offset 0))
    (labels ((read-particle (offset)
               (vsetf location (aref data (+ 0 offset)) (aref data (+ 1 offset)) (aref data (+ 2 offset)))
               (vsetf velocity (aref data (+ 3 offset)) (aref data (+ 4 offset)) (aref data (+ 5 offset))))
             (write-particle (offset)
               (setf (aref data (+ 0 offset)) (vx3 location))
               (setf (aref data (+ 1 offset)) (vy3 location))
               (setf (aref data (+ 2 offset)) (vz3 location))
               (setf (aref data (+ 3 offset)) (vx3 velocity))
               (setf (aref data (+ 4 offset)) (vy3 velocity))
               (setf (aref data (+ 5 offset)) (vz3 velocity))
               (setf (aref data (+ 6 offset)) (coerce lifetime 'single-float))))
      (loop for read-offset from 0 below (* 7 (live-particles particle-emitter)) by 7
            do (setf lifetime (aref data (+ 6 read-offset)))
               (when (< 0 lifetime)
                 (read-particle read-offset)
                 (setf lifetime (update-particle-state particle-emitter ev location velocity lifetime))
                 (write-particle write-offset)
                 (incf write-offset 7)))
      (loop repeat (new-particle-count particle-emitter ev)
            while (< write-offset (length data))
            do (setf lifetime (initial-particle-state particle-emitter ev location velocity))
               (write-particle write-offset)
               (incf write-offset 7))
      (setf (live-particles particle-emitter) (/ write-offset 7))
      (with-pointer-to-vector-data (ptr data)
        (update-buffer-data/ptr vbo ptr write-offset)))))
