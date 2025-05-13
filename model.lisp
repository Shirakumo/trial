(in-package #:org.shirakumo.fraf.trial)

(defclass model ()
  ((name :initarg :name :initform NIL :accessor name)
   (materials :initform (make-hash-table :test 'equal) :accessor materials)
   (meshes :initform (make-hash-table :test 'equal) :accessor meshes)
   (clips :initform (make-hash-table :test 'equal) :accessor clips)
   (scenes :initform (make-hash-table :test 'equal) :accessor scenes)))

(defmethod print-object ((model model) stream)
  (print-unreadable-object (model stream :type T :identity T)
    (format stream "~@[~a~]" (name model))))

(defmethod describe-object :after ((model model) stream)
  (format stream "~&~%Materials:~{~%  ~a~}" (or (list-materials model) (list "No materials")))
  (format stream "~&~%Meshes:~{~%  ~a~}" (or (list-meshes model) (list "No meshes")))
  (format stream "~&~%Clips:~{~%  ~a~}" (or (list-clips model) (list "No clips")))
  (format stream "~&~%Scenes:~{~%  ~a~}" (or (list-scenes model) (list "No scenes"))))

(defmacro define-table-accessor (class type accessor)
  (let ((find (intern (format NIL "~a-~a" 'find type)))
        (list (intern (format NIL "~a-~a" 'list accessor))))
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
         (alexandria:hash-table-values (,accessor ,class))))))

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

(defmethod find-meshes (name (meshes hash-table) &optional (errorp T))
  (let ((mesh (gethash name meshes)))
    (if mesh
        (list mesh)
        (or (loop for i from 0
                  for mesh = (gethash (cons name i) meshes)
                  while mesh collect mesh)
            (when errorp
              (error "No mesh named ~s found~:[, as there are no meshes available at all.~;. The following are known:~:*~{~%  ~s~}~]"
                     name (alexandria:hash-table-keys meshes)))))))

(defmethod find-meshes (name (model model) &optional (errorp T))
  (find-meshes name (meshes model) errorp))

(defmethod clear ((model model))
  (clrhash (materials model))
  (clrhash (meshes model))
  (clrhash (clips model))
  (clrhash (scenes model)))

;; TODO: coerce-object multi-mesh-entity mesh-entity etc to model
