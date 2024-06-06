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

(defmethod vertex-attribute-stride ((attributes list))
  (loop for attribute in attributes
        sum (vertex-attribute-size attribute)))

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
  (let ((offset 0))
    (dolist (attr container NIL)
      (when (eq attr attribute)
        (return offset))
      (incf offset (vertex-attribute-size attr)))))

(defclass mesh-data ()
  ((name :initarg :name :initform NIL :accessor name)
   (vertex-data :initarg :vertex-data :initform (make-array 0 :element-type 'single-float) :accessor vertex-data)
   (faces :initarg :faces :initform NIL :accessor faces)
   (material :initform NIL :accessor material)
   (vertex-form :initarg :vertex-form :initform :triangles :accessor vertex-form)
   (vertex-attributes :initarg :vertex-attributes :initform '(location normal uv) :accessor vertex-attributes)))

(defmethod shared-initialize :after ((data mesh-data) slots &key (material NIL material-p))
  (when material-p (setf (material data) material)))

(defmethod (setf material) ((name string) (data mesh-data))
  (setf (material data) (material name)))

(defmethod (setf material) ((name symbol) (data mesh-data))
  (setf (material data) (material name)))

(defmethod (setf material) ((name null) (data mesh-data))
  (setf (slot-value data 'material) NIL))

(defmethod reordered-vertex-data ((mesh mesh-data) new-attributes)
  (if (equal new-attributes (vertex-attributes mesh))
      (vertex-data mesh)
      (let* ((old-attributes (vertex-attributes mesh))
             (old-stride (vertex-attribute-stride mesh))
             (old-offsets (loop for attribute in old-attributes
                                collect (vertex-attribute-offset attribute old-attributes)))
             (old-data (vertex-data mesh))
             (vertices (truncate (length old-data) old-stride))
             (new-stride (loop for attribute in new-attributes
                               sum (vertex-attribute-size attribute)))
             (new-data (make-array (* new-stride vertices) :element-type 'single-float))
             (new-offsets (loop for attribute in new-attributes
                                collect (vertex-attribute-offset attribute new-attributes)))
             (new-base 0)
             (old-base 0))
        (dotimes (vertex vertices new-data)
          (loop for attr in new-attributes
                for new in new-offsets
                for old = (position attr old-attributes)
                do (if old
                       (let ((old (nth old old-offsets)))
                         (dotimes (i (vertex-attribute-size attr))
                           (setf (aref new-data (+ i new new-base)) (aref old-data (+ i old old-base)))))
                       (dotimes (i (vertex-attribute-size attr))
                         (setf (aref new-data (+ i new new-base)) 0f0))))
          (incf new-base new-stride)
          (incf old-base old-stride)))))

(defmethod make-vertex-array ((mesh mesh-data) vao)
  (let ((vertex-data (make-instance 'vertex-buffer :buffer-data (vertex-data mesh)))
        (faces (faces mesh)))
    (ensure-instance vao 'vertex-array
                     :dependencies (list (material mesh))
                     :vertex-form (vertex-form mesh)
                     :index-buffer (make-instance 'vertex-buffer :buffer-data faces
                                                                 :buffer-type :element-array-buffer
                                                                 :element-type (cl-type->pixel-type (array-element-type faces)))
                     :bindings (loop with stride = (vertex-attribute-stride mesh)
                                     for attribute in (vertex-attributes mesh)
                                     for offset = 0 then (+ offset size)
                                     for size = (vertex-attribute-size attribute)
                                     collect `(,vertex-data :size ,size :offset ,(* 4 offset) :stride ,(* 4 stride)))
                     :size (if faces
                               (length faces)
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

(defmethod update-buffer-data ((vbo vertex-buffer) (mesh mesh-data) &rest args &key)
  (apply #'update-buffer-data vbo (vertex-data mesh) args))

(defmethod update-buffer-data ((vao vertex-array) (mesh mesh-data) &rest args &key)
  (let ((buffer (caar (bindings vao))))
    (apply #'update-buffer-data buffer (vertex-data mesh) args)))

(defmethod replace-vertex-data ((vao vertex-array) (mesh mesh-data) &key)
  (resize-buffer-data (index-buffer vao) (faces mesh))
  (let ((vertex-data (caar (bindings vao))))
    (resize-buffer-data vertex-data (vertex-data mesh))
    (setf (bindings vao) (loop with stride = (vertex-attribute-stride mesh)
                               for attribute in (vertex-attributes mesh)
                               for offset = 0 then (+ offset size)
                               for size = (vertex-attribute-size attribute)
                               collect `(,vertex-data :size ,size :offset ,(* 4 offset) :stride ,(* 4 stride))))
    (setf (size vao) (length (faces mesh)))
    vao))

(defmethod coerce-object ((vao vertex-array) (type (eql 'mesh-data)) &rest args &key index-attribute &allow-other-keys)
  (let ((primary (caar (bindings vao)))
        (attributes (loop for index from 0
                          for (buffer . attrs) in (bindings vao)
                          collect (let ((index (or (getf attrs :index) index)))
                                    (if index-attribute
                                        (funcall index-attribute index)
                                        (case index
                                          (0 'location)
                                          (1 'normal)
                                          (2 'uv)
                                          (3 'tangent)
                                          (4 'color)
                                          (5 'joints)
                                          (6 'weights)
                                          (7 'unknown)))))))
    (remf args :index-attribute)
    (if (loop for (binding) in (cdr (bindings vao))
              always (eq binding primary))
        (apply #'make-instance 'mesh-data
               :faces (buffer-data (index-buffer vao))
               :vertex-data (buffer-data (caar (bindings vao)))
               :vertex-attributes attributes
               args)
        (implement!))))

(defmethod compute-vertex-attribute :around ((mesh mesh-data) new-attribute)
  (unless (find new-attribute (vertex-attributes mesh))
    (call-next-method))
  mesh)

(defmethod compute-vertex-attribute :before ((mesh mesh-data) new-attribute)
  ;; Since we have to reshuffle the vertex data for every computation,
  ;; just do so here.
  (let* ((new-attributes (sort (append (vertex-attributes mesh) (list 'normal)) #'vertex-attribute<))
         (new-vertices (reordered-vertex-data mesh new-attributes)))
    (setf (vertex-attributes mesh) new-attributes)
    (setf (vertex-data mesh) new-vertices)))

(defmethod compute-vertex-attribute ((mesh mesh-data) (_ (eql 'uv)))
  ;; TODO: unwrap UVs
  (implement!))

(defmethod compute-vertex-attribute ((mesh mesh-data) (new-attribute (eql 'normal)))
  (let* ((adjacency (org.shirakumo.fraf.manifolds:vertex-adjacency-list (faces mesh)))
         (vertices (vertex-data mesh))
         (offset (vertex-attribute-offset new-attribute mesh)))
    (loop for i from 0 below (length vertices) by (vertex-attribute-stride mesh)
          for v from 0
          for normal = (org.shirakumo.fraf.manifolds:vertex-normal vertices v (aref adjacency v))
          do (setf (aref vertices (+ i offset 0)) (vx normal))
             (setf (aref vertices (+ i offset 1)) (vy normal))
             (setf (aref vertices (+ i offset 2)) (vz normal)))))

(defmethod compute-vertex-attribute ((mesh mesh-data) (_ (eql 'tangent)))
  (compute-vertex-attribute mesh 'uv)
  (let ((vertices (vertex-data mesh))
        (faces (faces mesh))
        (stride (vertex-attribute-stride mesh))
        (voff (vertex-attribute-offset 'location mesh))
        (uoff (vertex-attribute-offset 'uv mesh))
        (toff (vertex-attribute-offset 'tangent mesh)))
    (flet ((v (f)
             (vec (aref vertices (+ (* stride (aref faces f)) voff 0))
                  (aref vertices (+ (* stride (aref faces f)) voff 1))
                  (aref vertices (+ (* stride (aref faces f)) voff 2))))
           (u (f)
             (vec (aref vertices (+ (* stride (aref faces f)) uoff 0))
                  (aref vertices (+ (* stride (aref faces f)) uoff 1)))))
      (loop for face from 0 below (length faces) by 3
            for v0 = (v (+ face 0))
            for u0 = (u (+ face 0))
            for e0 = (nv- (v (+ face 1)) v0)
            for e1 = (nv- (v (+ face 2)) v0)
            for d0 = (nv- (u (+ face 1)) u0)
            for d1 = (nv- (u (+ face 2)) u0)
            for tt = (nv* (nv- (v* e0 (vy d1)) (v* e1 (vy d0)))
                          (/ (- (* (vx d0) (vy d1)) (* (vx d1) (vy d0)))))
            do (dotimes (j 3)
                 (setf (aref vertices (+ (* stride (aref faces (+ face j))) toff 0)) (vx tt))
                 (setf (aref vertices (+ (* stride (aref faces (+ face j))) toff 1)) (vy tt))
                 (setf (aref vertices (+ (* stride (aref faces (+ face j))) toff 2)) (vz tt)))))))

(defmethod append-vertex-data ((a mesh-data) (b mesh-data))
  (assert (eq (vertex-form a) (vertex-form b)))
  (let ((data (make-array (+ (length (vertex-data a)) (length (vertex-data b)))
                          :element-type 'single-float))
        (faces (when (and (faces a) (faces b))
                 (make-array (+ (length (faces a)) (length (faces b)))
                             :element-type '(unsigned-byte 16)))))
    (replace data (vertex-data a))
    (replace data (vertex-data b) :start1 (length (vertex-data a)))
    (when faces
      (replace faces (faces a))
      (replace faces (faces b) :start1 (length (faces a))))
    (make-instance 'mesh-data :vertex-data data :faces faces
                              :material (material a)
                              :vertex-form (vertex-form a)
                              :vertex-attributes (vertex-attributes a))))

(defmethod combine-vertex-data ((a mesh-data) (b mesh-data))
  (assert (eq (vertex-form a) (vertex-form b)))
  (let* ((attributes (union (vertex-attributes a) (vertex-attributes b)))
         (stride (vertex-attribute-stride attributes))
         (vertex-count (/ (length (vertex-data a)) (vertex-attribute-stride a))))
    (unless (= vertex-count (/ (length (vertex-data b)) (vertex-attribute-stride b)))
      (error "The meshes contain different numbers of vertices. Can't combine their attributes!"))
    (let ((data (make-array (* vertex-count stride) :element-type 'single-float)))
      (dolist (attribute attributes)
        (let* ((source (if (find attribute (vertex-attributes a)) b a))
               (src-data (vertex-data source))
               (src-stride (vertex-attribute-stride source))
               (dst-offset (vertex-attribute-offset attribute attributes))
               (src-offset (vertex-attribute-offset attribute source)))
          (loop for src from src-offset by src-stride
                for dst from dst-offset below (length data) by stride
                do (dotimes (i (vertex-attribute-size attribute))
                     (setf (aref data (+ dst i)) (aref src-data (+ src i)))))))
      (make-instance 'mesh-data :vertex-data data
                                :faces (faces a)
                                :material (material a)
                                :vertex-form (vertex-form a)
                                :vertex-attributes attributes))))

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

(defmethod vertex-attributes ((vertex tangent-vertex))
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
          (resize-buffer-data buffer size :data (buffer-data buffer))
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
