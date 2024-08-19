(in-package #:org.shirakumo.fraf.trial)

(defvar *known-vertex-attributes*
  '(location normal uv tangent color joints weights
    uv-0 uv-1 uv-2 uv-3
    joints-0 joints-1 joints-2 joints-3
    weights-0 weights-1 weights-2 weights-3
    index))

(defgeneric vertex-attribute-size (attribute))
(defgeneric vertex-attribute-offset (attribute container))
(defgeneric vertex-attribute-category (attribute))
(defgeneric vertex-attribute-order (attribute))
(defgeneric vertex-attribute-stride (attributes))

(defmethod vertex-attribute-size ((_ (eql 'location))) 3)
(defmethod vertex-attribute-size ((_ (eql 'uv))) 2)
(defmethod vertex-attribute-size ((_ (eql 'normal))) 3)
(defmethod vertex-attribute-size ((_ (eql 'color))) 4)
(defmethod vertex-attribute-size ((_ (eql 'tangent))) 3)
(defmethod vertex-attribute-size ((_ (eql 'joints))) 4)
(defmethod vertex-attribute-size ((_ (eql 'weights))) 4)
(defmethod vertex-attribute-size ((_ (eql 'index))) 1)
(defmethod vertex-attribute-size ((attr symbol))
  (let ((cat (vertex-attribute-category attr)))
    (if (eq cat attr)
        (error "Vertex attribute ~s has no defined size!" attr)
        (vertex-attribute-size cat))))

(defmethod vertex-attribute-category ((attr symbol)) attr)
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

(defmethod vertex-index-attribute ((index integer))
  (dolist (attr *known-vertex-attributes*)
    (when (= index (vertex-attribute-order attr))
      (return attr))))

(macrolet ((distribute-vertex-indices (&rest attributes)
             `(progn ,@(loop for group in attributes
                             for i from 0
                             append (loop for attribute in (enlist group)
                                          collect `(defmethod vertex-attribute-order ((_ (eql ',attribute))) ,i))))))
  (distribute-vertex-indices
   location
   normal
   (uv uv-0)
   tangent
   color
   (joints joints-0)
   (weights weights-0)
   uv-1
   joints-1
   weights-1
   uv-2
   joints-2
   weights-2
   uv-3
   joints-3
   weights-3
   index))

(defmethod vertex-attribute-stride ((attributes list))
  (loop for attribute in attributes
        sum (vertex-attribute-size attribute)))

(defun vertex-attribute< (a b)
  (< (vertex-attribute-order a)
     (vertex-attribute-order b)))

(defmethod vertex-attribute-offset (attribute (attributes list))
  (let ((offset 0))
    (dolist (attr attributes NIL)
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

(defmethod vertex-count ((data mesh-data))
  (truncate (length (vertex-data data)) (vertex-attribute-stride data)))

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
                     :dependencies (when (material mesh) (list (material mesh)))
                     :vertex-form (vertex-form mesh)
                     :index-buffer (when faces
                                     (make-instance 'vertex-buffer :buffer-data faces
                                                                   :buffer-type :element-array-buffer
                                                                   :element-type (cl-type->pixel-type (array-element-type faces))))
                     :bindings (loop with stride = (vertex-attribute-stride mesh)
                                     for attribute in (vertex-attributes mesh)
                                     for offset = 0 then (+ offset size)
                                     for size = (vertex-attribute-size attribute)
                                     for index = (vertex-attribute-order attribute)
                                     collect `(,vertex-data :index ,index :size ,size :offset ,(* 4 offset) :stride ,(* 4 stride)))
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

(defmethod coerce-object ((vao vertex-array) (type (eql 'mesh-data)) &rest args &key (index-attribute #'vertex-index-attribute) &allow-other-keys)
  (let ((primary (caar (bindings vao)))
        (attributes (loop for index from 0
                          for (buffer . attrs) in (bindings vao)
                          collect (let ((index (or (getf attrs :index) index)))
                                    (funcall index-attribute index)))))
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
        (faces (if (or (faces a) (faces b))
                   (make-array (+ (length (faces a)) (length (faces b)))
                               :element-type '(unsigned-byte 16))))
        (attributes (if (equal (vertex-attributes a) (vertex-attributes b))
                        (vertex-attributes a)
                        (union (vertex-attributes a) (vertex-attributes b)))))
    (cond ((eq attributes (vertex-attributes a))
           (replace data (vertex-data a))
           (replace data (vertex-data b) :start1 (length (vertex-data a))))
          (T
           (let ((data-a (reordered-vertex-data a attributes))
                 (data-b (reordered-vertex-data b attributes)))
             (replace data data-a)
             (replace data data-b :start1 (length data-a)))))
    (when faces
      (if (faces a)
          (replace faces (faces a))
          ;; Missing, just use every vertex in sequence
          (loop for v from 0 below (vertex-count a)
                do (setf (aref faces v) v)))
      ;; Since the vertex data got appended, we have to offset B's
      ;; faces by the vertex-count of A.
      (if (faces b)
          (loop with src = (faces b)
                with off = (vertex-count a)
                for i from (length (faces a))
                for j from 0 below (length src)
                do (setf (aref faces i) (+ (aref src j) off)))
          ;; Missing, just use every vertex in sequence
          (loop for v from (vertex-count a)
                for i from (length (faces a)) below (length faces)
                do (setf (aref faces i) v))))
    (make-instance 'mesh-data :vertex-data data :faces faces
                              :material (material a)
                              :vertex-form (vertex-form a)
                              :vertex-attributes attributes)))

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

(defmacro with-mesh-construction ((constructor &optional (finalizer 'finalize) (attributes '(location)) (deduplicate T)) &body body)
  (let ((vertices (gensym "VERTICES"))
        (faces (gensym "FACES"))
        (face-table (gensym "FACE-TABLE"))
        (stride (vertex-attribute-stride attributes))
        (data-finalizer (intern (format NIL "~a-~a" (string finalizer) (string '#:data))))
        (i (gensym "I")))
    `(let ((,vertices (make-array 0 :element-type 'single-float :adjustable T))
           (,faces (make-array 0 :element-type '(unsigned-byte 16) :adjustable T :fill-pointer T))
           ,@(when deduplicate `((,face-table (make-hash-table :test 'equal))))
           (,i 0))
       (labels ((,constructor (&rest bits)
                  (let* ((c bits)
                         (e ,(when deduplicate `(gethash c ,face-table))))
                    (cond (e
                           (vector-push-extend e ,faces))
                          (T
                           (vector-push-extend ,i ,faces)
                           (let ((j (* ,stride ,i)))
                             (when (< (length ,vertices) (+ j ,stride))
                               (adjust-array ,vertices (+ j ,stride)))
                             (dolist (e c)
                               (setf (aref ,vertices j) (float e 0f0))
                               (incf j)))
                           ,@(when deduplicate `((setf (gethash c ,face-table) ,i)))
                           (incf ,i)))))
                (,finalizer ()
                  (values (make-array (length ,vertices) :element-type 'single-float :initial-contents ,vertices)
                          (make-array (length ,faces) :element-type '(unsigned-byte 16) :initial-contents ,faces)))
                (,data-finalizer (&rest args)
                  (multiple-value-bind (,vertices ,faces) (,finalizer)
                    (apply #'make-instance 'mesh-data :vertex-attributes ',attributes :vertex-data ,vertices :faces ,faces args))))
         (declare (ignorable #',data-finalizer))
         ,@body))))
