#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass geometry ()
  ((meshes :initform (make-hash-table :test 'equal) :accessor meshes)))

(defgeneric read-geometry (file format &key &allow-other-keys))

(defmethod read-geometry (file (format (eql T)) &rest args)
  (apply #'read-geometry file (intern (string-upcase (pathname-type file)) :keyword) args))

(defgeneric write-geometry (geometry file format &key &allow-other-keys))

(defmethod write-geometry (geometry file (format (eql T)) &rest args)
  (apply #'write-geometry geometry file (intern (string-upcase (pathname-type file)) :keyword) args))

(defclass sphere-mesh ()
  ((size :initarg :size :accessor size)))

(defclass vertex-mesh ()
  ((face-length :initarg :face-length :initform 3 :accessor face-length)
   (vertex-type :initform 'vertex :initarg :vertex-type :reader vertex-type)
   (faces :initform (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T) :accessor faces)
   (vertices :initform (make-array 0 :adjustable T :fill-pointer T) :accessor vertices)))

(defmethod update-vertices (function (mesh vertex-mesh))
  (loop for vertex across (vertices mesh)
        do (funcall function vertex))
  mesh)

(defun push-mesh-vertex (vertex mesh)
  (let ((vertices (vertices mesh)))
    (vector-push-extend (length vertices) (faces mesh))
    (vector-push-extend vertex vertices))
  vertex)

(defmethod add-vertex ((mesh vertex-mesh) &rest initargs)
  (push-mesh-vertex (apply #'make-instance (vertex-type mesh) initargs) mesh))

(defmethod triangulate ((mesh vertex-mesh))
  (when (< 3 (face-length mesh))
    (error "Triangulation is not yet implemented."))
  mesh)

(defmethod check-mesh-valid ((mesh vertex-mesh))
  (when (/= 0 (mod (length (faces mesh)) (face-length mesh)))
    (error "The number of face indices saved is not even.")))

(defmethod pack ((mesh vertex-mesh))
  (check-mesh-valid mesh)
  (let* ((vertices (vertices mesh))
         (faces (faces mesh))
         (new-verts (make-array (floor (length vertices) 2) :adjustable T :fill-pointer 0)))
    (loop for position from 0 below (length vertices)
          for vertex = (aref vertices position)
          for new-pos = (position vertex new-verts :test #'vertex=)
          do (unless new-pos
               (setf new-pos (fill-pointer new-verts))
               (vector-push-extend vertex new-verts))
             ;; Fix up face index. I feel like this could be done outside the
             ;; loop somehow to speed this up. For now though-- good nuff.
             (loop for i from 0 below (length faces)
                   do (when (= position (aref faces i))
                        (setf (aref faces i) new-pos))))
    (setf (vertices mesh) new-verts))
  mesh)

(defmacro with-vertex-filling ((mesh &key pack) &body body)
  (let ((meshg (gensym "MESH")))
    `(let ((,meshg ,mesh))
       (flet ((vertex (&rest initargs)
                (apply #'add-vertex ,meshg initargs)))
         ,@body
         (if ,pack
             (pack ,meshg)
             ,meshg)))))

(defclass vertex ()
  ((location :initform (vec 0 0 0) :initarg :position :initarg :location :accessor location :type vec3)))

(declaim (inline fill-vector-data))
(defun fill-vector-data (vec type array &optional (offset 0))
  (ecase type
    (vec2
     (setf (aref array (+ offset 0)) (vx2 vec))
     (setf (aref array (+ offset 1)) (vy2 vec))
     (+ offset 2))
    (vec3
     (setf (aref array (+ offset 0)) (vx3 vec))
     (setf (aref array (+ offset 1)) (vy3 vec))
     (setf (aref array (+ offset 2)) (vz3 vec))
     (+ offset 3))
    (vec4
     (setf (aref array (+ offset 0)) (vx4 vec))
     (setf (aref array (+ offset 1)) (vy4 vec))
     (setf (aref array (+ offset 2)) (vz4 vec))
     (setf (aref array (+ offset 3)) (vw4 vec))
     (+ offset 4))))

(defmethod vertex-attribute-size ((vertex vertex) (attribute (eql 'location)))
  3)

(defmethod fill-vertex-attribute ((vertex vertex) (attribute (eql 'location)) data offset)
  (fill-vector-data (location vertex) (type-of (location vertex)) data offset))

(defgeneric vertex-attributes (vertex)
  (:method-combination append :most-specific-last))

(defmethod vertex-attributes append ((vertex vertex))
  '(location))

(defgeneric vertex= (a b)
  (:method-combination and))

(defmethod vertex= and ((a vertex) (b vertex))
  (v= (location a) (location b)))

(defclass textured-vertex (vertex)
  ((uv :initform (vec 0 0) :initarg :uv :initarg :texcoord :accessor uv :type vec2)))

(defmethod vertex= and ((a textured-vertex) (b textured-vertex))
  (v= (uv a) (uv b)))

(defmethod vertex-attribute-size ((vertex textured-vertex) (attribute (eql 'uv)))
  2)

(defmethod vertex-attributes append ((vertex textured-vertex))
  '(uv))

(defmethod fill-vertex-attribute ((vertex textured-vertex) (attribute (eql 'uv)) data offset)
  (fill-vector-data (uv vertex) 'vec2 data offset))

(defclass normal-vertex (vertex)
  ((normal :initform (vec 0 0 0 1) :initarg :normal :accessor normal :type vec3)))

(defmethod vertex= and ((a normal-vertex) (b normal-vertex))
  (v= (normal a) (normal b)))

(defmethod vertex-attribute-size ((vertex normal-vertex) (attribute (eql 'normal)))
  3)

(defmethod vertex-attributes append ((vertex normal-vertex))
  '(normal))

(defmethod fill-vertex-attribute ((vertex normal-vertex) (attribute (eql 'normal)) data offset)
  (fill-vector-data (normal vertex) 'vec3 data offset))

(defclass colored-vertex (vertex)
  ((color :initform (vec 0 0 0 1) :initarg :color :accessor color :type vec4)))

(defmethod vertex= and ((a colored-vertex) (b colored-vertex))
  (v= (color a) (color b)))

(defmethod vertex-attribute-size ((vertex colored-vertex) (attribute (eql 'color)))
  4)

(defmethod vertex-attributes append ((vertex colored-vertex))
  '(color))

(defmethod fill-vertex-attribute ((vertex colored-vertex) (attribute (eql 'color)) data offset)
  (fill-vector-data (color vertex) 'vec4 data offset))

(defclass tangent-vertex (vertex)
  ((tangent :initform (vec 0 0 0) :initarg :tangent :accessor tangent :type vec3)))

(defmethod vertex= and ((a tangent-vertex) (b tangent-vertex))
  (v= (tangent a) (tangent b)))

(defmethod vertex-attribute-size ((vertex tangent-vertex) (attribute (eql 'tangent)))
  3)

(defmethod vertex-attributes append ((vertex tangent-vertex))
  '(tangent))

(defmethod fill-vertex-attribute ((vertex tangent-vertex) (attribute (eql 'tangent)) data offset)
  (fill-vector-data (tangent vertex) 'vec3 data offset))

(defclass basic-vertex (normal-vertex textured-vertex)
  ())

(defclass basic+-vertex (tangent-vertex normal-vertex textured-vertex)
  ())

;;;; Translation
(defmethod replace-vertex-data ((buffer vector) (mesh vertex-mesh) &key (attributes T) (buffer-type :array-buffer))
  (cond ((< 0 (length (vertices mesh)))
         (ecase buffer-type
           (:element-array-buffer
            (when (/= (length buffer) (length (faces mesh)))
              (setf buffer (adjust-array buffer (length (faces mesh)) :fill-pointer (length (faces mesh)))))
            (replace buffer (faces mesh)))
           (:array-buffer
            (let* ((vertices (vertices mesh))
                   (primer (aref vertices 0))
                   (attributes (etypecase attributes
                                 ((eql T) (vertex-attributes primer))
                                 (list attributes)))
                   (sizes (loop for attr in attributes collect (vertex-attribute-size primer attr)))
                   (total-size (* (length vertices) (reduce #'+ sizes))))
              (when (/= (length buffer) total-size)
                (setf buffer (if (array-has-fill-pointer-p buffer)
                                 (adjust-array buffer total-size :fill-pointer total-size)
                                 (adjust-array buffer total-size))))
              ;; Copy the contents of the mesh into the data buffer, packed.
              (loop with buffer-offset = 0
                    for vertex across vertices
                    do (dolist (attribute attributes)
                         (setf buffer-offset (fill-vertex-attribute vertex attribute buffer buffer-offset))))))))
        (T
         (setf buffer (if (array-has-fill-pointer-p buffer)
                          (adjust-array buffer 0 :fill-pointer 0)
                          (make-array 0 :adjustable T :fill-pointer T :element-type (array-element-type buffer))))))
  buffer)

(defmethod replace-vertex-data ((buffer vertex-buffer) (mesh vertex-mesh) &key (attributes T) update)
  (setf (buffer-data buffer) (replace-vertex-data (buffer-data buffer) mesh :attributes attributes :buffer-type (buffer-type buffer)))
  (when update
    (let ((new-size (* (length (buffer-data buffer)) (gl-type-size (element-type buffer)))))
      (cond ((= 0 new-size)
             (setf (size buffer) 0))
            ((/= (size buffer) new-size)
             (resize-buffer buffer new-size :data (buffer-data buffer)))
            (T
             (update-buffer-data buffer T)))))
  buffer)

(defmethod replace-vertex-data ((array vertex-array) (mesh vertex-mesh) &key (attributes T) update)
  (let ((buffers ()))
    (dolist (binding (bindings array))
      (pushnew (unlist binding) buffers))
    (dolist (buffer buffers)
      (replace-vertex-data buffer mesh :attributes attributes :update update)
      (when (eq (buffer-type buffer) :element-array-buffer)
        (setf (size array) (length (buffer-data buffer)))))
    array))

(defmethod make-vertex-data ((mesh vertex-mesh) &key (attributes T))
  ;; Would be better if we didn't have to create an adjustable vector...
  (replace-vertex-data (make-array 0 :adjustable T :element-type 'single-float)
                       mesh :attributes attributes))

(defmethod update-instance-for-different-class ((mesh vertex-mesh) (array vertex-array) &key (data-usage :static-draw) (vertex-attributes T) vertex-form)
  (setf (vertex-form array)
        (or vertex-form
            (ecase (face-length mesh)
              (1 :points)
              (2 :lines)
              (3 :triangles))))
  (let* ((primer (if (= 0 (length (vertices mesh)))
                     (allocate-instance (find-class (vertex-type mesh)))
                     (aref (vertices mesh) 0)))
         (attributes (etypecase vertex-attributes
                       ((eql T) (vertex-attributes primer))
                       (list vertex-attributes)))
         (sizes (loop for attr in attributes collect (vertex-attribute-size primer attr)))
         (buffer (make-vertex-data mesh :attributes attributes)))
    (setf (data-pointer array) NIL)
    ;; Construct the buffers and specs
    (let* ((vbo (make-instance 'vertex-buffer :buffer-data buffer :buffer-type :array-buffer
                                              :data-usage data-usage :element-type :float
                                              :size (* (length buffer) (gl-type-size :float))))
           (ebo (make-instance 'vertex-buffer :buffer-data (faces mesh) :buffer-type :element-array-buffer
                                              :data-usage data-usage :element-type :unsigned-int
                                              :size (* (length (faces mesh)) (gl-type-size :unsigned-int))))
           (specs (loop with stride = (reduce #'+ sizes)
                        for offset = 0 then (+ offset size)
                        for size in sizes
                        for index from 0
                        collect (list vbo :stride (* stride (gl-type-size :float))
                                          :offset (* offset (gl-type-size :float))
                                          :size size
                                          :index index))))
      (setf (bindings array) (list* ebo specs))
      (setf (size array) (length (faces mesh))))))
