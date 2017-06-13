#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-pool geometry
  :base 'trial)

(define-asset (geometry fullscreen-square) packed-vertex-array
    (#(0 1 2 2 3 0)
     3 #(+1.0 +1.0 +0.0
         +1.0 -1.0 +0.0
         -1.0 -1.0 +0.0
         -1.0 +1.0 +0.0)
     2 #(1.0 1.0
         1.0 0.0
         0.0 0.0
         0.0 1.0)))

(defclass geometry ()
  ((meshes :initform (make-hash-table :test 'equal) :accessor meshes)))

(defgeneric read-geometry (file format &key &allow-other-keys))

(defmethod read-geometry (file (format (eql T)) &rest args)
  (apply #'read-geometry file (intern (string-upcase (pathname-type file)) :keyword) args))

(defgeneric write-geometry (geometry file format &key &allow-other-keys))

(defmethod write-geometry (geometry file (format (eql T)) &rest args)
  (apply #'write-geometry geometry file (intern (string-upcase (pathname-type file)) :keyword) args))

(defclass mesh ()
  ())

(defclass sphere (mesh)
  ((size :initarg :size :accessor size)))

(defclass vertex-mesh (mesh)
  ((vertices :initform (make-array 0 :adjustable T :fill-pointer T) :accessor vertices)
   (faces :initform (make-array 0 :element-type 'fixnum :adjustable T :fill-pointer T) :accessor faces)
   (face-length :initform 3 :accessor face-length)
   (vertex-type :initform 'vertex :initarg :vertex-type :reader vertex-type)))

(defun push-mesh-vertex (vertex mesh)
  (let ((vertices (vertices mesh)))
    (vector-push-extend (length vertices) (faces mesh))
    (vector-push-extend vertex vertices))
  vertex)

(defmethod add-vertex ((mesh vertex-mesh) &rest initargs)
  (push-mesh-vertex (apply #'make-instance (vertex-type mesh) initargs) mesh))

(defmethod triangulate ((mesh vertex-mesh))
  (unless (= 3 (face-length mesh))
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

(defmethod update-instance-for-different-class :after ((mesh vertex-mesh) (vao vertex-array) &key pack load (data-usage :static-draw) attributes)
  (when pack (pack mesh))
  (let* ((vertices (vertices mesh))
         (primer (aref vertices 0))
         (attributes (or attributes (vertex-attributes primer)))
         (sizes (loop for attr in attributes collect (vertex-attribute-size primer attr)))
         (total-size (* (length vertices) (reduce #'+ sizes)))
         (buffer (static-vectors:make-static-vector total-size :element-type 'single-float)))
    (loop with offset = 0
          for vertex across vertices
          do (dolist (attribute attributes)
               (setf offset (fill-vertex-attribute vertex attribute buffer offset))))
    (let* ((vbo (make-asset 'vertex-buffer buffer
                            :data-usage data-usage :element-type :float :buffer-type :array-buffer))
           (ebo (make-asset 'vertex-buffer (faces mesh)
                            :data-usage data-usage :element-type :uint :buffer-type :element-array-buffer))
           (specs (loop with stride = (reduce #'+ sizes)
                        for offset = 0 then (+ offset size)
                        for size in sizes
                        for index from 0
                        collect (list vbo :stride (* stride (cffi:foreign-type-size :float))
                                          :offset (* offset (cffi:foreign-type-size :float))
                                          :size size
                                          :index index))))
      (setf (inputs vao) (list* ebo specs))
      (when load
        (load vao)
        ;; Clean up
        (offload vbo)
        (offload ebo)
        (setf (inputs vbo) NIL)
        (setf (inputs ebo) NIL)
        (setf (inputs vao) NIL)
        (static-vectors:free-static-vector buffer))
      vao)))

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
  (fill-vector-data (location vertex) 'vec3 data offset))

(defgeneric vertex-attributes (vertex)
  (:method-combination list))

(defmethod vertex-attributes list ((vertex vertex))
  'location)

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

(defmethod vertex-attributes list ((vertex textured-vertex))
  'uv)

(defmethod fill-vertex-attribute ((vertex textured-vertex) (attribute (eql 'uv)) data offset)
  (fill-vector-data (uv vertex) 'vec2 data offset))

(defclass normal-vertex (vertex)
  ((normal :initform (vec 0 0 0 1) :initarg :normal :accessor normal :type vec3)))

(defmethod vertex= and ((a normal-vertex) (b normal-vertex))
  (v= (normal a) (normal b)))

(defmethod vertex-attribute-size ((vertex normal-vertex) (attribute (eql 'normal)))
  3)

(defmethod vertex-attributes list ((vertex normal-vertex))
  'normal)

(defmethod fill-vertex-attribute ((vertex normal-vertex) (attribute (eql 'normal)) data offset)
  (fill-vector-data (normal vertex) 'vec3 data offset))

(defclass colored-vertex (vertex)
  ((color :initform (vec 0 0 0 1) :initarg :color :accessor color :type vec4)))

(defmethod vertex= and ((a colored-vertex) (b colored-vertex))
  (v= (color a) (color b)))

(defmethod vertex-attribute-size ((vertex colored-vertex) (attribute (eql 'color)))
  4)

(defmethod vertex-attributes list ((vertex colored-vertex))
  'color)

(defmethod fill-vertex-attribute ((vertex colored-vertex) (attribute (eql 'color)) data offset)
  (fill-vector-data (color vertex) 'vec4 data offset))

(defclass basic-vertex (normal-vertex textured-vertex)
  ())
