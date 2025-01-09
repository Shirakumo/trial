(in-package #:org.shirakumo.fraf.trial)

(defclass model ()
  ((materials :initform (make-hash-table :test 'equal) :accessor materials)
   (meshes :initform (make-hash-table :test 'equal) :accessor meshes)
   (clips :initform (make-hash-table :test 'equal) :accessor clips)
   (scenes :initform (make-hash-table :test 'equal) :accessor scenes)))

(defmacro define-table-accessor (class type accessor)
  (let ((find (intern (format NIL "~a-~a" 'find type)))
        (list (intern (format NIL "~a-~a" 'list type))))
    `(progn
       (defmethod ,find (name (,class ,class) &optional (errorp T))
         (or (gethash name (,accessor ,class))
             (when errorp (error "No ~(~a~) with name~%  ~a~%on~%  ~a~%Known ~(~a~)s:~%  ~a"
                                 ',type name ,class ',type (alexandria:hash-table-keys (,accessor ,class))))))

       (defmethod (setf ,find) (value name (,class ,class))
         (setf (gethash name (,accessor ,class)) value))

       (defmethod (setf ,find) ((value null) name (,class ,class))
         (remhash name (,accessor ,class))
         value)

       (defmethod ,list ((,class ,class))
         (alexandria:hash-table-values (clips ,class))))))

(define-table-accessor model material materials)
(define-table-accessor model mesh meshes)
(define-table-accessor model clip clips)
(define-table-accessor model scene scenes)

(defmethod find-scene ((first (eql T)) (model model) &optional (errorp T))
  (let ((keys (sort (alexandria:hash-table-keys (scenes model)) #'string<)))
    (if keys
        (find-scene (first keys) model)
        (when errorp (error "Model contains no scenes.")))))

(defmethod node (name (model model))
  (loop for scene being the hash-values of (scenes model)
        thereis (node name scene)))

(defmethod clear ((model model))
  (clrhash (materials model))
  (clrhash (meshes model))
  (clrhash (clips model))
  (clrhash (scenes model)))

;; TODO: coerce-object multi-mesh-entity mesh-entity etc to model
