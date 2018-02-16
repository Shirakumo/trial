#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(declaim (inline vformat-write-float))
(defun vformat-write-float (buffer float)
  (fast-io:writeu32-le (ieee-floats:encode-float32 float) buffer))

(declaim (inline vformat-write-double))
(defun vformat-write-double (buffer float)
  (fast-io:writeu64-le (ieee-floats:encode-float64 float) buffer))

(declaim (inline vformat-read-float))
(defun vformat-read-float (buffer)
  (ieee-floats:decode-float32 (fast-io:readu32-le buffer)))

(declaim (inline vformat-read-double))
(defun vformat-read-double (buffer)
  (ieee-floats:decode-float64 (fast-io:readu64-le buffer)))

(defun vformat-write-vector (buffer array type)
  (when (<= (expt 2 32) (length array))
    (error "Array is longer than 2³² elements."))
  (fast-io:write32-be (length array) buffer)
  (ecase type
    (:char
     (fast-io:writeu8 0 buffer)
     (loop for value across array
           do (fast-io:write8 value buffer)))
    (:int
     (fast-io:writeu8 1 buffer)
     (loop for value across array
           do (fast-io:write32-be value buffer)))
    (:uint
     (fast-io:writeu8 2 buffer)
     (loop for value across array
           do (fast-io:writeu32-be value buffer)))
    (:float
     (fast-io:writeu8 3 buffer)
     (loop for value across array
           do (vformat-write-float buffer value)))
    (:double
     (fast-io:writeu8 4 buffer)
     (loop for value across array
           do (vformat-write-double buffer value)))))

(defun vformat-read-vector (buffer)
  (let* ((size (fast-io:read32-be buffer))
         (type (ecase (fast-io:readu8 buffer)
                 (0 :char)
                 (1 :int)
                 (2 :uint)
                 (3 :float)
                 (4 :double)))
         (array (make-static-vector size :element-type (gl-type->cl-type type))))
    (ecase type
      (:char
       (loop for i from 0 below size
             do (setf (aref array i) (fast-io:read8 buffer))))
      (:int
       (loop for i from 0 below size
             do (setf (aref array i) (fast-io:read32-be buffer))))
      (:uint
       (loop for i from 0 below size
             do (setf (aref array i) (fast-io:readu32-be buffer))))
      (:float
       (loop for i from 0 below size
             do (setf (aref array i) (vformat-read-float buffer))))
      (:double
       (loop for i from 0 below size
             do (setf (aref array i) (vformat-read-double buffer)))))
    (values array
            size
            type)))

(defun vformat-write-string (buffer string)
  (loop for char across string
        do (fast-io:writeu8 (char-code char) buffer))
  (fast-io:writeu8 0 buffer))

(defun vformat-read-string (buffer)
  (with-output-to-string (out)
    (loop for code = (fast-io:readu8 buffer)
          until (= 0 code)
          do (write-char (code-char code) out))))

(defun vformat-write-symbol (buffer symbol)
  (vformat-write-string buffer (package-name (symbol-package symbol)))
  (vformat-write-string buffer (symbol-name symbol)))

(defun vformat-read-symbol (buffer)
  (let ((package (vformat-read-string buffer))
        (name (vformat-read-string buffer)))
    (intern name package)))

(defun vertex-buffer-type->int (type)
  (position type *vertex-buffer-type-list*))

(defun int->vertex-buffer-type (int)
  (elt *vertex-buffer-type-list* int))

(defun vertex-buffer-usage->int (type)
  (position type *vertex-buffer-data-usage-list*))

(defun int->vertex-buffer-usage (int)
  (elt *vertex-buffer-data-usage-list* int))

(defun vformat-write-type (buffer value type)
  (ecase type
    (vec2
     (vformat-write-float buffer (vx2 value))
     (vformat-write-float buffer (vy2 value)))
    (vec3
     (vformat-write-float buffer (vx3 value))
     (vformat-write-float buffer (vy3 value))
     (vformat-write-float buffer (vz3 value)))
    (vec4
     (vformat-write-float buffer (vx4 value))
     (vformat-write-float buffer (vy4 value))
     (vformat-write-float buffer (vz4 value))
     (vformat-write-float buffer (vw4 value)))
    ((integer fixnum)
     (unless (< value (expt 2 32))
       (error "Cannot write an integer bigger than 32 bits."))
     (fast-io:write32-le value buffer))
    (single-float
     (vformat-write-float buffer value))
    (double-float
     (vformat-write-double buffer value))
    (string
     (vformat-write-string buffer value))
    (T
     (vformat-write buffer value))))

(defun vformat-read-type (buffer type)
  (ecase type
    (vec2
     (vec2 (vformat-read-float buffer)
           (vformat-read-float buffer)))
    (vec3
     (vec3 (vformat-read-float buffer)
           (vformat-read-float buffer)
           (vformat-read-float buffer)))
    (vec4
     (vec4 (vformat-read-float buffer)
           (vformat-read-float buffer)
           (vformat-read-float buffer)
           (vformat-read-float buffer)))
    ((integer fixnum)
     (fast-io:read32-le buffer))
    (single-float
     (vformat-read-float buffer))
    (double-float
     (vformat-read-double buffer))
    (string
     (vformat-read-string buffer))
    (T
     (vformat-read buffer T))))

(defun vformat-slot-data (class)
  (let ((class (ensure-class class)))
    (unless (c2mop:class-finalized-p class)
      (c2mop:finalize-inheritance class))
    (loop for slot in (c2mop:class-slots class)
          for initargs = (c2mop:slot-definition-initargs slot)
          when initargs
          collect (list (c2mop:slot-definition-name slot)
                        (c2mop:slot-definition-type slot)
                        (minimize initargs #'< :key (lambda (a) (length (symbol-name a))))))))

(defun vformat-write-vertices (buffer vertices type)
  (fast-io:writeu32-le (length vertices) buffer)
  (let ((map (vformat-slot-data type)))
    (fast-io:writeu8 (length map) buffer)
    (loop for (slot type initarg) in map
          do (vformat-write-string buffer (symbol-name initarg)))
    (loop for vertex across vertices
          do (loop for (slot type initarg) in map
                   for value = (slot-value vertex slot)
                   do (vformat-write-type buffer value type)))))

(defun vformat-read-vertices (buffer type)
  (let* ((size (fast-io:readu32-le buffer))
         (map (vformat-slot-data type))
         (initargs (loop repeat (fast-io:readu8 buffer)
                         collect (intern (vformat-read-string buffer) :keyword)))
         (vertices (make-array size)))
    (loop for i from 0 below size
          do (setf (aref vertices i)
                   (apply #'make-instance type
                          (loop for initarg in initargs
                                for (slot type) = (find initarg map :key #'third)
                                when slot collect initarg
                                when slot collect (vformat-read-type buffer type)))))))

(defmethod vformat-write (buffer (mesh vertex-mesh))
  (let ((face-length (face-length mesh)))
    (when (< 256 face-length)
      (error "Faces with more than 2⁸ vertices are not supported."))
    (fast-io:writeu8 face-length buffer))
  (vformat-write-symbol buffer (vertex-type mesh))
  (vformat-write-vector buffer (faces mesh) :uint)
  (vformat-write-vertices buffer (vertices mesh) (vertex-type mesh)))

(defmethod vformat-read (buffer (mesh vertex-mesh))
  (initialize-instance mesh
                       :face-length (fast-io:readu8 buffer)
                       :vertex-type (vformat-read-symbol buffer))
  (setf (faces mesh) (vformat-read-vector buffer))
  (setf (vertices mesh) (vformat-read-vertices buffer (vertex-type mesh)))
  mesh)

;; FIXME
;; (defmethod vformat-write (buffer (vbo vertex-buffer))
;;   (fast-io:writeu8 (vertex-buffer-type->int (buffer-type vbo)) buffer)
;;   (fast-io:writeu8 (vertex-buffer-usage->int (data-usage vbo)) buffer)
;;   (vformat-write-vector buffer (coerced-inputs vbo) (element-type vbo)))

;; (defmethod vformat-read (buffer (vbo vertex-buffer))
;;   (let ((btype (int->vertex-buffer-type (fast-io:readu8 buffer)))
;;         (usage (int->vertex-buffer-usage (fast-io:readu8 buffer))))
;;     (multiple-value-bind (array size etype) (vformat-read-vector buffer)
;;       (initialize-instance vbo :buffer-type btype
;;                                :data-usage usage
;;                                :element-type etype
;;                                :size size
;;                                :input array))))

;; (defmethod vformat-write (buffer (vao vertex-array))
;;   (fast-io:write32-le (or (size vao) -1) buffer)
;;   (let* ((inputs (coerced-inputs vao))
;;          (count (length inputs))
;;          (buffers (remove-duplicates (mapcar #'first inputs))))
;;     (when (< 256 count)
;;       (error "More than 2⁸ buffers are not supported."))
;;     ;; Write input list
;;     (fast-io:writeu8 count buffer)
;;     (loop for i from 0
;;           for input in inputs
;;           do (destructuring-bind (vbo &key (index i) (size 3) (stride 0) (offset 0) (normalized NIL)) input
;;                (fast-io:writeu8 (position vbo buffers) buffer)
;;                (fast-io:writeu8 index buffer)
;;                (fast-io:writeu8 size buffer)
;;                (fast-io:writeu32-le stride buffer)
;;                (fast-io:writeu32-le offset buffer)
;;                (fast-io:writeu8 (if normalized 1 0) buffer)))
;;     ;; Write buffer list
;;     (fast-io:writeu8 (length buffers) buffer)
;;     (dolist (vbo buffers)
;;       (vformat-write buffer vbo))))

;; (defmethod vformat-read (buffer (vao vertex-array))
;;   (let* ((size (fast-io:read32-le buffer))
;;          (inputs (loop repeat (fast-io:readu8 buffer)
;;                        collect (list (fast-io:readu8 buffer)
;;                                      :index (fast-io:readu8 buffer)
;;                                      :size (fast-io:readu8 buffer)
;;                                      :stride (fast-io:readu32-le buffer)
;;                                      :offset (fast-io:readu32-le buffer)
;;                                      :normalized (= (fast-io:readu8 buffer) 1))))
;;          (buffers (loop repeat (fast-io:readu8 buffer)
;;                         collect (vformat-read buffer T))))
;;     ;; Resolve buffer indexing
;;     (dolist (input inputs)
;;       (setf (first input) (nth (first input) buffers)))
;;     (when (< size 0) (setf size NIL))
;;     (initialize-instance vao :size size :inputs inputs)))

(defmethod vformat-write (buffer (mesh sphere-mesh))
  (vformat-write-double buffer (size mesh)))

(defmethod vformat-read (buffer (mesh sphere-mesh))
  (initialize-instance mesh :size (vformat-read-double buffer)))

(defmethod vformat-write (buffer (geometry geometry))
  (let ((count (hash-table-count (meshes geometry))))
    (when (< 256 count)
      (error "More than 2⁸ meshes is not supported."))
    (fast-io:writeu8 count buffer))
  (loop for name being the hash-keys of (meshes geometry)
        for mesh being the hash-values of (meshes geometry)
        do (vformat-write-symbol buffer name)
           (vformat-write buffer mesh)))

(defmethod vformat-read (buffer (geometry geometry))
  (let* ((count (fast-io:readu8 buffer))
         (table (make-hash-table :test 'eql :size count)))
    (loop repeat count
          for name = (vformat-read-symbol buffer)
          for mesh = (vformat-read buffer T)
          do (setf (gethash name table) mesh))
    (initialize-instance geometry)
    (setf (meshes geometry) table)
    geometry))

(defmethod vformat-write :before (buffer (object standard-object))
  (vformat-write-symbol buffer (class-name (class-of object))))

(defmethod vformat-read (buffer (type (eql T)))
  (vformat-read buffer (allocate-instance (find-class (vformat-read-symbol buffer)))))

(defmethod write-geometry ((geometry geometry) file (format (eql :vf)) &key (if-exists :error))
  (with-open-file (stream file :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists if-exists)
    (when stream
      (fast-io:with-fast-output (buffer stream)
        (vformat-write buffer geometry))
      file)))

(defmethod read-geometry (file (format (eql :vf)) &key (if-does-not-exist :error))
  (with-open-file (stream file :direction :input
                               :element-type '(unsigned-byte 8)
                               :if-does-not-exist if-does-not-exist)
    (when stream
      (fast-io:with-fast-input (buffer NIL stream)
        (vformat-read buffer T)))))
