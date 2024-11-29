(in-package #:org.shirakumo.fraf.trial)

(define-global +current-vertex-array+ NIL)

(defclass vertex-array (gl-resource)
  ((size :initarg :size :initform NIL :accessor size)
   (bindings :accessor bindings)
   (vertex-form :initarg :vertex-form :accessor vertex-form)
   (index-buffer :initform NIL :accessor index-buffer :reader indexed-p)
   (instanced-p :initform NIL :accessor instanced-p))
  (:default-initargs
   :bindings (error "BINDINGS required.")
   :vertex-form :triangles))

(defmethod print-object ((array vertex-array) stream)
  (print-unreadable-object (array stream :type T :identity T)
    (format stream "~@[~a~]~:[~; ALLOCATED~]" (size array) (allocated-p array))))

(defmethod shared-initialize :after ((array vertex-array) slots &key bindings (index-buffer NIL index-buffer-p) size)
  (when index-buffer-p
    (setf (index-buffer array) index-buffer))
  ;; Set the size again explicitly since setting the index buffer can reset it
  (when size (setf (size array) size))
  (setf (bindings array) bindings))

(defmethod describe-object :after ((array vertex-array) stream)
  (when (gl-name array)
    (format stream "~&~%Bindings:~%")
    (with-context ()
      (with-gl-binding (:vertex-array (gl-name array))
        (cffi:with-foreign-objects ((param :int))
          (macrolet ((query (form) `(progn ,form (cffi:mem-ref param :int))))
            (let ((name (query (%gl:get-vertex-array-iv (gl-name array) :element-array-buffer-binding param))))
              (when (< 0 name)
                (format stream "  :INDEX-BUFFER ~a~%" name)))
            (dotimes (i (gl:get* :max-vertex-attribs))
              (let ((binding (query (%gl:get-vertex-attrib-iv i :vertex-attrib-array-buffer-binding param))))
                (when (< 0 binding)
                  (format stream "  ~a :INDEX ~2d :SIZE ~d :TYPE ~a :STRIDE ~d :OFFSET ~d :NORMALIZE ~a :INSTANCING ~d~%"
                          binding i
                          (query (%gl:get-vertex-attrib-iv i :vertex-attrib-array-size param))
                          (cffi:foreign-enum-keyword '%gl:enum (query (%gl:get-vertex-attrib-iv i :vertex-attrib-array-type param)))
                          (query (%gl:get-vertex-attrib-iv i :vertex-attrib-array-stride param))
                          (query (%gl:get-vertex-attrib-iv i :vertex-attrib-relative-offset param))
                          (< 0 (query (%gl:get-vertex-attrib-iv i :vertex-attrib-array-normalized param)))
                          (query (%gl:get-vertex-attrib-iv i :vertex-attrib-array-divisor param))))))))))))

(defmethod dependencies ((array vertex-array))
  (delete-duplicates
   (append (call-next-method)
           (if (index-buffer array) (list (index-buffer array)))
           (mapcar #'unlist (bindings array)))))

(defun normalize-array-bindings (bindings)
  (let ((new-bindings ()) ebo
        (indices ()))
    (loop for binding in bindings
          for i from 0
          do (destructuring-bind (buffer &key (index i)
                                              (size 3)
                                              (type (element-type buffer))
                                              (stride (* size (gl-type-size type)))
                                              (offset 0)
                                              (normalize NIL)
                                              (instancing 0))
                 (enlist binding)
               (ecase (buffer-type buffer)
                 (:element-array-buffer
                  (setf ebo buffer)
                  (decf i))
                 (:array-buffer
                  (if (member index indices)
                      (error "Duplicated binding to index ~d!~%  ~a" index bindings)
                      (push index indices))
                  (push (list buffer :index index :size size :type type :stride stride
                                     :offset offset :normalize normalize :instancing instancing)
                        new-bindings)))))
    (setf new-bindings (sort new-bindings #'< :key (lambda (e) (getf (rest e) :index))))
    (when ebo (push ebo new-bindings))
    new-bindings))

(defun update-array-bindings (array &optional (bindings (bindings array)) (index (index-buffer array)))
  (with-gl-binding (:vertex-array (gl-name array))
    (when index
      (check-allocated index)
      (activate index))
    (setf (instanced-p array) NIL)
    (loop for binding in bindings
          do (destructuring-bind (buffer &key index
                                              (size 3)
                                              (type (element-type buffer))
                                              (stride (* size (gl-type-size type)))
                                              (offset 0)
                                              (normalize NIL)
                                              (instancing 0))
                 (enlist binding)
               (check-allocated buffer)
               (activate buffer)
               (ecase (buffer-type buffer)
                 (:element-array-buffer
                  (setf (index-buffer array) buffer))
                 (:array-buffer
                  (ecase type
                    ((:half-float :float :fixed)
                     (gl:vertex-attrib-pointer index size type normalize stride offset))
                    ((:byte :unsigned-byte :short :unsigned-short :int :unsigned-int)
                     (gl:vertex-attrib-ipointer index size type stride offset))
                    (:double
                     (%gl:vertex-attrib-lpointer index size type stride offset)))
                  (gl:enable-vertex-attrib-array index)
                  (when (/= 0 instancing)
                    (%gl:vertex-attrib-divisor index instancing)
                    (setf (instanced-p array) T))))))))

(defmethod (setf bindings) (bindings (array vertex-array))
  (setf (slot-value array 'bindings) (normalize-array-bindings bindings))
  (when (allocated-p array)
    (update-array-bindings array)))

(defmethod (setf index-buffer) :after ((buffer vertex-buffer) (array vertex-array))
  (setf (size array) (cond ((size buffer)
                            (/ (size buffer) (gl-type-size (element-type buffer))))
                           ((buffer-data buffer)
                            (length (buffer-data buffer)))
                           (T (error "???"))))
  (when (allocated-p array)
    (activate array)
    (activate buffer)))

(defmethod (setf index-buffer) :after ((null null) (array vertex-array))
  (setf (size array) NIL)
  (when (allocated-p array)
    (activate array)
    (gl:bind-buffer :element-array-buffer 0)))

(defmethod allocate ((array vertex-array))
  (let ((vao (gl:gen-vertex-array)))
    (with-cleanup-on-failure (progn (gl:delete-vertex-arrays (list vao))
                                    (setf (data-pointer array) NIL))
      (setf (data-pointer array) vao)
      (update-array-bindings array))))

(defmethod deallocate ((array vertex-array))
  (gl:delete-vertex-arrays (list (gl-name array)))
  (when (eq array +current-vertex-array+)
    (setf +current-vertex-array+ NIL)))

(defmethod unload ((array vertex-array))
  (loop for binding in (bindings array)
        do (if (listp binding)
               (unload (first binding))
               (unload binding))))

(defmethod activate ((array vertex-array))
  (unless (eq array +current-vertex-array+)
    (setf +current-vertex-array+ array)
    (gl:bind-vertex-array (gl-name array))))

(defmethod deactivate ((array vertex-array))
  (when (eq array +current-vertex-array+)
    (setf +current-vertex-array+ NIL)
    (gl:bind-vertex-array 0)))

(declaim (inline render-array))
(defun render-array (array &key (vertex-start 0) (vertex-count (size array)) (instances 1)
                                (vertex-form (vertex-form array)) patch-size)
  (declare (type (unsigned-byte 32) vertex-start vertex-count instances))
  (activate array)
  (when patch-size (gl:patch-parameter :patch-vertices patch-size))
  (if (instanced-p array)
      (if (indexed-p array)
          (%gl:draw-elements-instanced vertex-form vertex-count (element-type (indexed-p array)) vertex-start instances)
          (%gl:draw-arrays-instanced vertex-form vertex-start vertex-count instances))
      (if (indexed-p array)
          (%gl:draw-elements vertex-form vertex-count (element-type (indexed-p array))
                             (* vertex-start (gl-type-size (element-type (index-buffer array)))))
          (%gl:draw-arrays vertex-form vertex-start vertex-count)))
  (deactivate array))

(defmethod render ((array vertex-array) target)
  (render-array array))

(defun compute-buffer-bindings (buffer fields)
  (let* ((fields (loop for field in fields
                       collect (etypecase field
                                 (cons field)
                                 (integer (list field :float))
                                 (keyword (case field
                                            (:vec2 '(2 :float))
                                            (:vec3 '(3 :float))
                                            (:vec4 '(4 :float))
                                            (T (list 1 field)))))))
         (stride (loop for (size type) in fields
                       sum (* size (gl-type-size type)))))
    (loop with offset = 0
          for (size type instance) in fields
          for i from 0
          collect `(,buffer :index ,i :size ,size :offset ,offset :type ,type :stride ,stride :instancing ,(or instance 0))
          do (incf offset (* size (gl-type-size type))))))

(defun find-vertex-buffer-binding (vao index)
  (loop for binding in (bindings vao)
        when (eql index (getf (rest binding) :index))
        return binding))
