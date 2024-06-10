(in-package #:org.shirakumo.fraf.trial)

(defclass model ()
  ((materials :initform (make-hash-table :test 'equal) :accessor materials)
   (meshes :initform (make-hash-table :test 'equal) :accessor meshes)
   (clips :initform (make-hash-table :test 'equal) :accessor clips)
   (scenes :initform (make-hash-table :test 'equal) :accessor scenes)
   (skeleton :initform NIL :accessor skeleton)))

(defmacro %define-model-accessor (type accessor)
  (let ((find (intern (format NIL "~a-~a" 'find type))))
    `(progn
       (defmethod ,find (name (model model) &optional (errorp T))
         (or (gethash name (,accessor model))
             (when errorp (error "No ~(~a~) with name~%  ~a~%on~%  ~a~%Known ~(~a~)s:~%  ~a"
                                 ',type name model ',type (alexandria:hash-table-keys (,accessor model))))))

       (defmethod (setf ,find) (value name (model model))
         (setf (gethash name (,accessor model)) value))

       (defmethod (setf ,find) ((value null) name (model model))
         (remhash name (,accessor model))
         value))))

(%define-model-accessor material materials)
(%define-model-accessor mesh meshes)
(%define-model-accessor clip clips)
(%define-model-accessor scene scenes)

(defmethod find-scene ((first (eql T)) (model model) &optional (errorp T))
  (let ((keys (sort (alexandria:hash-table-keys (scenes model)) #'string<)))
    (if keys
        (find-scene (first keys) model)
        (when errorp (error "Model contains no scenes.")))))

(defmethod list-clips ((model model))
  (alexandria:hash-table-values (clips model)))

(defmethod list-meshes ((model model))
  (alexandria:hash-table-values (meshes model)))

(defmethod list-scenes ((model model))
  (alexandria:hash-table-values (scenes model)))

(defmethod node (name (model model))
  (loop for scene being the hash-values of (scenes model)
        thereis (node name scene)))

(defmethod clear ((model model))
  (clrhash (materials model))
  (clrhash (meshes model))
  (clrhash (clips model))
  (clrhash (scenes model))
  (setf (skeleton model) NIL))

;; TODO: coerce-object multi-mesh-entity mesh-entity etc to model
