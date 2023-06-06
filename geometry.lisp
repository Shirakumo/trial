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
  (apply #'read-geometry file (kw (pathname-type file)) args))

(defgeneric write-geometry (geometry file format &key &allow-other-keys))

(defmethod write-geometry (geometry file (format (eql T)) &rest args)
  (apply #'write-geometry geometry file (kw (pathname-type file)) args))

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

(defgeneric vertex-attribute-size (attribute))
(defgeneric vertex-attribute-offset (attribute container))
(defgeneric vertex-attribute-category (attribute))
(defgeneric vertex-attribute-order (attribute))

;; KLUDGE: this kinda sucks, doesn't it?

(defmethod vertex-attribute-size ((_ (eql 'location))) 3)
(defmethod vertex-attribute-size ((_ (eql 'uv))) 2)
(defmethod vertex-attribute-size ((_ (eql 'normal))) 3)
(defmethod vertex-attribute-size ((_ (eql 'color))) 4)
(defmethod vertex-attribute-size ((_ (eql 'tangent))) 3)
(defmethod vertex-attribute-size ((_ (eql 'joints))) 4)
(defmethod vertex-attribute-size ((_ (eql 'weights))) 4)
(defmethod vertex-attribute-size ((_ (eql 'uv-0))) 2)
(defmethod vertex-attribute-size ((_ (eql 'uv-1))) 2)
(defmethod vertex-attribute-size ((_ (eql 'uv-2))) 2)
(defmethod vertex-attribute-size ((_ (eql 'uv-3))) 2)
(defmethod vertex-attribute-size ((_ (eql 'joints-0))) 4)
(defmethod vertex-attribute-size ((_ (eql 'joints-1))) 4)
(defmethod vertex-attribute-size ((_ (eql 'joints-2))) 4)
(defmethod vertex-attribute-size ((_ (eql 'joints-3))) 4)
(defmethod vertex-attribute-size ((_ (eql 'weights-0))) 4)
(defmethod vertex-attribute-size ((_ (eql 'weights-1))) 4)
(defmethod vertex-attribute-size ((_ (eql 'weights-2))) 4)
(defmethod vertex-attribute-size ((_ (eql 'weights-3))) 4)

(defmethod vertex-attribute-category (attr) attr)
(defmethod vertex-attribute-category ((_ (eql 'uv-0))) 'uv)
(defmethod vertex-attribute-category ((_ (eql 'uv-1))) 'uv)
(defmethod vertex-attribute-category ((_ (eql 'uv-2))) 'uv)
(defmethod vertex-attribute-category ((_ (eql 'uv-3))) 'uv)
(defmethod vertex-attribute-category ((_ (eql 'joints-0))) 'joints)
(defmethod vertex-attribute-category ((_ (eql 'joints-1))) 'joints)
(defmethod vertex-attribute-category ((_ (eql 'joints-2))) 'joints)
(defmethod vertex-attribute-category ((_ (eql 'joints-3))) 'joints)
(defmethod vertex-attribute-category ((_ (eql 'weights-0))) 'weights)
(defmethod vertex-attribute-category ((_ (eql 'weights-1))) 'weights)
(defmethod vertex-attribute-category ((_ (eql 'weights-2))) 'weights)
(defmethod vertex-attribute-category ((_ (eql 'weights-3))) 'weights)

(defmethod vertex-attribute-order ((_ (eql 'location))) 0)
(defmethod vertex-attribute-order ((_ (eql 'normal))) 1)
(defmethod vertex-attribute-order ((_ (eql 'uv))) 2)
(defmethod vertex-attribute-order ((_ (eql 'tangent))) 3)
(defmethod vertex-attribute-order ((_ (eql 'color))) 4)
(defmethod vertex-attribute-order ((_ (eql 'joints))) 5)
(defmethod vertex-attribute-order ((_ (eql 'weights))) 6)
(defmethod vertex-attribute-order (attr) (vertex-attribute-order (vertex-attribute-category attr)))

(defun vertex-attribute< (a b)
  (let ((a-cat (vertex-attribute-category a))
        (b-cat (vertex-attribute-category b)))
    (cond ((eql a b) NIL)
          ((eql a-cat b-cat)
           (string< a b))
          (T
           (< (vertex-attribute-order a-cat)
              (vertex-attribute-order b-cat))))))

(defmethod vertex-attribute-offset (attribute (container list))
  (loop for attr in container
        until (eq attr attribute)
        sum (vertex-attribute-size attr)))

(defclass mesh-data ()
  ((name :initarg :name :initform NIL :accessor name)
   (vertex-data :initarg :vertex-data :initform (make-array 0 :element-type 'single-float) :accessor vertex-data)
   (index-data :initarg :index-data :initform NIL :accessor index-data)
   (material :initform NIL :accessor material)
   (vertex-form :initarg :vertex-form :initform :triangles :accessor vertex-form)
   (vertex-attributes :initarg :vertex-attributes :initform '(location normal uv) :accessor vertex-attributes)))

(defmethod shared-initialize :after ((data mesh-data) slots &key (material NIL material-p))
  (when material-p (setf (material data) material)))

(defmethod (setf material) ((name string) (data mesh-data))
  (setf (material data) (material name)))

(defmethod (setf material) ((none null) (data mesh-data))
  (setf (material data) NIL))

(defmethod (setf material) ((name symbol) (data mesh-data))
  (setf (material data) (material name)))

(defmethod make-vertex-array ((mesh mesh-data) vao)
  (let ((vertex-data (make-instance 'vertex-buffer :buffer-data (vertex-data mesh)))
        (index-data (index-data mesh)))
    (ensure-instance vao 'vertex-array
                     :dependencies (list (material mesh))
                     :vertex-form (vertex-form mesh)
                     :bindings (append (loop with stride = (vertex-attribute-stride mesh)
                                             for attribute in (vertex-attributes mesh)
                                             for offset = 0 then (+ offset size)
                                             for size = (vertex-attribute-size attribute)
                                             collect `(,vertex-data :size ,size :offset ,offset :stride ,stride))
                                       (when index-data
                                         (list (make-instance 'vertex-buffer :buffer-data index-data
                                                                             :buffer-type :element-array-buffer
                                                                             :element-type :unsigned-int))))
                     :size (if index-data
                               (length index-data)
                               (truncate (length (vertex-data mesh)) (+ 3 3 2))))))

(defmethod gl-source ((mesh mesh-data))
  `(glsl-toolkit:shader
    ,@(loop for i from 0
            for attribute in (vertex-attributes mesh)
            for size = (vertex-attribute-size attribute)
            for name = (format NIL "in_~a" (symbol->c-name attribute))
            for type = (ecase size (1 :float) (2 :vec2) (3 :vec3) (4 :vec4))
            collect `(glsl-toolkit:variable-declaration
                      (glsl-toolkit:type-qualifier
                       (glsl-toolkit:layout-qualifier
                        (glsl-toolkit:layout-qualifier-id "location" ,i))
                       :in)
                      (glsl-toolkit:type-specifier ,type)
                      ,name glsl-toolkit:no-value))))

(defmethod vertex-attribute-stride ((mesh mesh-data))
  (loop for attribute in (vertex-attributes mesh)
        sum (vertex-attribute-size attribute)))

(defmethod vertex-attribute-offset (attribute (mesh mesh-data))
  (vertex-attribute-offset attribute (vertex-attributes mesh)))

(defmethod update-buffer-data ((vbo vertex-buffer) (mesh mesh-data) &key)
  (update-buffer-data vbo (vertex-data mesh)))

(defmethod update-buffer-data ((vao vertex-array) (mesh mesh-data) &key)
  (let ((buffer (caar (bindings vao))))
    (update-buffer-data buffer (vertex-data mesh))))

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

(defmethod fill-vertex-attribute ((vertex vertex) (attribute (eql 'location)) data offset)
  (fill-vector-data (location vertex) (type-of (location vertex)) data offset))

(defgeneric vertex-attributes (vertex))

(defmethod vertex-attributes ((vertex vertex))
  '(location))

(defgeneric vertex= (a b)
  (:method-combination and))

(defmethod vertex= and ((a vertex) (b vertex))
  (v= (location a) (location b)))

(defclass textured-vertex (vertex)
  ((uv :initform (vec 0 0) :initarg :uv :initarg :texcoord :accessor uv :type vec2)))

(defmethod vertex= and ((a textured-vertex) (b textured-vertex))
  (v= (uv a) (uv b)))

(defmethod vertex-attributes ((vertex textured-vertex))
  (append (call-next-method) '(uv)))

(defmethod fill-vertex-attribute ((vertex textured-vertex) (attribute (eql 'uv)) data offset)
  (fill-vector-data (uv vertex) 'vec2 data offset))

(defclass normal-vertex (vertex)
  ((normal :initform (vec 0 0 1) :initarg :normal :accessor normal :type vec3)))

(defmethod vertex= and ((a normal-vertex) (b normal-vertex))
  (v= (normal a) (normal b)))

(defmethod vertex-attributes ((vertex normal-vertex))
  (append (call-next-method) '(normal)))

(defmethod fill-vertex-attribute ((vertex normal-vertex) (attribute (eql 'normal)) data offset)
  (fill-vector-data (normal vertex) 'vec3 data offset))

(defclass colored-vertex (vertex)
  ((color :initform (vec 0 0 0 1) :initarg :color :accessor color :type vec4)))

(defmethod vertex= and ((a colored-vertex) (b colored-vertex))
  (v= (color a) (color b)))

(defmethod vertex-attributes ((vertex colored-vertex))
  (append (call-next-method) '(color)))

(defmethod fill-vertex-attribute ((vertex colored-vertex) (attribute (eql 'color)) data offset)
  (fill-vector-data (color vertex) 'vec4 data offset))

(defclass tangent-vertex (vertex)
  ((tangent :initform (vec 0 0 0) :initarg :tangent :accessor tangent :type vec3)))

(defmethod vertex= and ((a tangent-vertex) (b tangent-vertex))
  (v= (tangent a) (tangent b)))

(defmethod vertex-attributes append ((vertex tangent-vertex))
  (append (call-next-method) '(tangent)))

(defmethod fill-vertex-attribute ((vertex tangent-vertex) (attribute (eql 'tangent)) data offset)
  (fill-vector-data (tangent vertex) 'vec3 data offset))

(defclass basic-vertex (textured-vertex normal-vertex)
  ())

(defclass basic+-vertex (tangent-vertex textured-vertex normal-vertex)
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
                   (sizes (loop for attr in attributes collect (vertex-attribute-size attr)))
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
  (setf (buffer-data buffer) (replace-vertex-data (or (buffer-data buffer) (make-array 0 :adjustable T :element-type 'single-float)) mesh
                                                  :attributes attributes :buffer-type (buffer-type buffer)))
  (when update
    (let ((size (* (length (buffer-data buffer)) (gl-type-size (element-type buffer)))))
      (if (gl-name buffer)
          (resize-buffer buffer size :data (buffer-data buffer))
          (setf (size buffer) size))))
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
  ;; TODO: Would be better if we didn't have to create an adjustable vector...
  (simplify
   (replace-vertex-data (make-array 0 :adjustable T :element-type 'single-float)
                        mesh :attributes attributes)))
