(in-package #:org.shirakumo.fraf.trial)

(defclass static-mesh (mesh-data)
  ())

(defmethod skinned-p ((mesh static-mesh)) NIL)
(defmethod morphed-p ((mesh static-mesh)) NIL)

(defclass animated-mesh (mesh-data)
  ((vertex-attributes :initform '(location normal uv joints weights))
   (position-normals :initform (make-array 0 :element-type 'single-float) :accessor position-normals)
   (morphs :initform (make-array 0) :accessor morphs)
   (model-name :initarg :model-name :initform NIL :accessor model-name)
   (initial-weights :initform #() :accessor initial-weights)
   (skinned-p :initarg :skinned-p :initform T :accessor skinned-p)))

(defmethod (setf vertex-data) :after (data (mesh animated-mesh))
  (let ((vertices (vertex-count mesh)))
    (setf (position-normals mesh) (adjust-array (position-normals mesh) (* vertices (+ 3 3))
                                                :initial-element 0f0))))

(defmethod morphed-p ((mesh animated-mesh))
  (< 0 (length (morphs mesh))))

(defmethod cpu-skin ((mesh animated-mesh) pose)
  (let ((pos-normal (position-normals mesh))
        (vertex-data (vertex-data mesh)))
    (flet ((transform (mat out-i in-i w)
             (let ((vec (vec (aref vertex-data (+ in-i 0))
                             (aref vertex-data (+ in-i 1))
                             (aref vertex-data (+ in-i 2))
                             w)))
               (n*m mat vec)
               (setf (aref pos-normal (+ out-i 0)) (vx vec))
               (setf (aref pos-normal (+ out-i 1)) (vy vec))
               (setf (aref pos-normal (+ out-i 2)) (vz vec)))))
      (loop for i from 0 below (length pos-normal) by (+ 3 3)
            for j from 0 by (+ 3 3 2 4 4)
            for mat = (meye 4)
            do (loop for idx from 0 below 4
                     for joint = (floor (aref vertex-data (+ j idx 3 3 2)))
                     for weight = (aref vertex-data (+ j idx 3 3 2 4))
                     do (nm+ mat (m* (svref pose joint) weight)))
               (transform mat (+ i 0) (+ j 0) 1.0)
               (transform mat (+ i 3) (+ j 3) 0.0)))))

(defmethod make-vertex-array ((mesh animated-mesh) vao)
  (let ((position-normals (make-instance 'vertex-buffer :buffer-data (position-normals mesh)))
        (stride (vertex-attribute-stride mesh)))
    (loop for i from 0 below (length (vertex-data mesh)) by stride
          for j from 0 below (length (position-normals mesh)) by (+ 3 3)
          do (setf (aref (position-normals mesh) (+ j 0)) (aref (vertex-data mesh) (+ i 0)))
             (setf (aref (position-normals mesh) (+ j 1)) (aref (vertex-data mesh) (+ i 1)))
             (setf (aref (position-normals mesh) (+ j 2)) (aref (vertex-data mesh) (+ i 2)))
             (setf (aref (position-normals mesh) (+ j 3)) (aref (vertex-data mesh) (+ i 3)))
             (setf (aref (position-normals mesh) (+ j 4)) (aref (vertex-data mesh) (+ i 4)))
             (setf (aref (position-normals mesh) (+ j 5)) (aref (vertex-data mesh) (+ i 5))))
    (let ((vao (call-next-method)))
      (setf (elt (bindings vao) 0) `(,position-normals :index 0 :size 3 :offset 0 :stride 24))
      (setf (elt (bindings vao) 1) `(,position-normals :index 1 :size 3 :offset 12 :stride 24))
      vao)))

(defmethod update-buffer-data ((vao vertex-array) (mesh animated-mesh) &key)
  (let ((buffer (caar (bindings vao))))
    (update-buffer-data buffer (position-normals mesh))))

(defmethod reorder ((mesh animated-mesh) map)
  (let ((data (vertex-data mesh)))
    (loop for i from (vertex-attribute-offset 'joints mesh) below (length data) by (vertex-attribute-stride mesh)
          do (setf (aref data (+ i 0)) (float (gethash (truncate (aref data (+ i 0))) map) 0f0))
             (setf (aref data (+ i 1)) (float (gethash (truncate (aref data (+ i 1))) map) 0f0))
             (setf (aref data (+ i 2)) (float (gethash (truncate (aref data (+ i 2))) map) 0f0))
             (setf (aref data (+ i 3)) (float (gethash (truncate (aref data (+ i 3))) map) 0f0)))
    mesh))

(defmethod make-morph-texture ((mesh animated-mesh))
  (let* ((attributes
           ;; TODO: compute reduced or expanded set of attributes from targets.
           #++
           (loop for target across targets
                 for attributes = (vertex-attributes target) then (union attributes (vertex-attributes target))
                 finally (return attributes))
           '(location normal uv))
         (vertex-count (vertex-count mesh))
         (morph-count (length (morphs mesh)))
         ;; The stride is 9, 3 for every color. This wastes space for the UV, since it only needs RG.
         (stride 9)
         (data (make-array (* vertex-count morph-count stride 3) :element-type 'single-float))
         (texture (make-instance 'texture :target :texture-1d-array
                                          :internal-format :rgb32f
                                          :min-filter :nearest
                                          :mag-filter :nearest
                                          :width (* stride vertex-count)
                                          :height morph-count
                                          :pixel-data data
                                          :pixel-type :float
                                          :pixel-format :rgb)))
    ;; Compact the targets into a slice per target
    (loop for target across (morphs mesh)
          for src-data = (vertex-data target)
          for src-stride = (vertex-attribute-stride target)
          for slice from 0 by (* vertex-count stride)
          do (unless (= (vertex-count target) vertex-count)
               (error "Not all morph targets have the same number of vertices!"))
             (loop for attribute in attributes
                   for src-offset = (vertex-attribute-offset attribute target)
                   for dst-offset = (vertex-attribute-offset attribute attributes)
                   do (when src-offset
                        (loop for src from src-offset below (length src-data) by src-stride
                              for dst from dst-offset by stride
                              do (setf (aref data (+ slice dst 0)) (aref src-data (+ src 0)))
                                 (setf (aref data (+ slice dst 1)) (aref src-data (+ src 1)))
                                 (unless (eq attribute 'uv)
                                   (setf (aref data (+ dst 2)) (aref src-data (+ src 2))))))))
    texture))

(defmethod make-morph-weights ((mesh animated-mesh))
  (let ((weights (make-array (length (morphs mesh)) :element-type 'single-float :initial-element 0f0)))
    (map-into weights (lambda (x) (float x 0f0)) (initial-weights mesh))
    weights))
