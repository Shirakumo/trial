(in-package #:org.shirakumo.fraf.trial)

(defvar *prefab-translation-functions*
  (make-hash-table :test 'eql))

(defmacro define-prefab-translator (name (instancevar assetvar &rest args) &body body)
  `(setf (gethash ',name *prefab-translation-functions*)
         (lambda (,instancevar ,assetvar ,@args)
           (declare (ignorable ,instancevar ,assetvar))
           ,@body)))

(defun expand-prefab-expression (type instancevar assetvar args)
  (apply (or (gethash type *prefab-translation-functions*)
             (error "The prefab translation form ~a is unknown." type))
         instancevar assetvar args))

(defgeneric prefab-asset (prefab))
(defgeneric instantiate-prefab (prefab asset))

(defclass prefab (scene-node)
  ((instantiated :initform NIL)))

(defmethod observe-load-state :before ((prefab prefab) (asset model-file) (state (eql :loaded)) (area staging-area))
  (instantiate-prefab prefab asset))

(defmethod stage :before ((prefab prefab) (area staging-area))
  (when (prefab-asset prefab)
    (register-load-observer area prefab (prefab-asset prefab))
    (stage (prefab-asset prefab) area)))

(defun make-prefab (thing &rest initargs)
  (apply #'reinitialize-instance (instantiate-prefab thing T) initargs))

(defmethod instantiate-prefab ((prefab symbol) asset)
  (instantiate-prefab (make-instance prefab) asset))

(defmethod instantiate-prefab ((prefab class) asset)
  (instantiate-prefab (make-instance prefab) asset))

(defmethod instantiate-prefab ((prefab prefab) (asset (eql T)))
  (instantiate-prefab prefab (prefab-asset prefab)))

#-trial-release
(defmethod instantiate-prefab :before ((prefab prefab) (asset asset))
  (check-loaded asset))

(defmethod instantiate-prefab :around ((prefab prefab) (asset asset))
  (unless (slot-value prefab 'instantiated)
    ;; We do this rigamarole here to avoid REGISTER being called prematurely
    ;; on child nodes during the prefab process, and instead call REGISTER
    ;; manually at the end.
    (let ((container (container prefab)))
      (setf (container prefab) NIL)
      (prog1 (call-next-method)
        (when container
          (setf (container prefab) container)
          (register prefab container))
        (setf (slot-value prefab 'instantiated) T)))))

(defmethod clear :after ((prefab prefab))
  (setf (slot-value prefab 'instantiated) NIL))

(defmethod reload ((prefab prefab))
  (deallocate (prefab-asset prefab))
  (clear prefab)
  (commit prefab (loader +main+) :unload NIL))

(defmacro define-prefab-instantiation (class asset &body changes)
  (let ((assetvar (gensym "ASSET")))
    `(progn
       ,@(when asset
           `((defmethod prefab-asset ((,class ,class))
               ,(if (eql asset 'null)
                    'NIL
                    `(asset ,@(loop for part in asset collect `',part))))))
       
       (defmethod instantiate-prefab ((,class ,class) (,assetvar model-file))
         ,@(loop for (type . args) in (or changes '((<-)))
                 collect (expand-prefab-expression type class assetvar args))
         ,class))))

(defmacro do-nodes% ((node instance &optional (id '(constantly T))) &body body)
  (etypecase id
    ((member NIL T)
     `(let ((,node ,instance))
        ,@body))
    ((or string symbol real)
     `(let ((,node (node ,id ,instance)))
        ,@body))
    (cons
     `(do-scene-graph (,node ,instance)
        (when (funcall ,id ,node)
          ,@body)))))

(define-prefab-translator enter (instance asset node &key children-only (scene T))
  (let ((nodevar (gensym "NODE")))
    `(do-nodes% (,nodevar (find-scene ,scene ,asset) ,node)
       ,(if children-only
            `(sequences:dosequence (,nodevar ,nodevar)
               (enter (clone ,nodevar) ,instance))
            `(enter (clone ,nodevar) ,instance)))))

(define-prefab-translator leave (instance asset &optional node)
  (let ((nodevar (gensym "NODE")))
    `(do-nodes% (,nodevar ,instance ,node)
       (leave ,nodevar T))))

(define-prefab-translator scaling (instance asset scale &optional node)
  (let ((nodevar (gensym "NODE")))
    `(do-nodes% (,nodevar ,instance ,node)
       (setf (scaling ,nodevar) ,scale))))

(define-prefab-translator location (instance asset location &optional node)
  (let ((nodevar (gensym "NODE")))
    `(do-nodes% (,nodevar ,instance ,node)
      (setf (location ,nodevar) ,location))))

(define-prefab-translator orientation (instance asset quat &optional node)
  (let ((nodevar (gensym "NODE")))
    `(do-nodes% (,nodevar ,instance ,node)
       (setf (orientation ,nodevar) ,quat))))

(define-prefab-translator physics-primitives (instance asset node)
  (let ((nodevar (gensym "NODE")))
    `(do-nodes% (,nodevar ,instance ,node)
       (when (= 0 (length (physics-primitives ,nodevar)))
         (error "No physics primitives stored on ~a" ,nodevar))
       (setf (physics-primitives ,instance) (physics-primitives ,nodevar))
       (setf (mass ,instance) (mass ,nodevar))
       (typecase ,nodevar
         (animated-entity
          (change-class ,nodevar 'basic-animated-entity))
         (T
          (change-class ,nodevar 'basic-entity))))))

(define-prefab-translator change-class (instance asset node class)
  (let ((nodevar (gensym "NODE")))
    `(do-nodes% (,nodevar ,instance ,node)
       (change-class ,nodevar ,(if (typep class 'symbol)
                                   `',class
                                   `(,class ,nodevar))))))

(define-prefab-translator play (instance asset animation)
  `(play ,animation ,instance))

(define-prefab-translator <- (instance asset &optional node)
  `(<- ,instance ,(if node
                      `(node ,node ,asset)
                      `(elt (find-scene T ,asset) 0))))

(define-prefab-translator update-material (instance asset type &rest initargs)
  ;; KLUDGE: This will also change materials on other instances that might share it.
  ;;         However, allocating a new material entirely will wreck sharing between
  ;;         multiple instances of the same prefab.
  (let ((nodevar (gensym "NODE")))
    `(do-nodes% (,nodevar ,instance (lambda (x) (typep x 'per-array-material-renderable)))
       (loop for material across (materials ,nodevar)
             do (ensure-instance material ',type ,@initargs)))))

(define-prefab-translator materials (instance asset node &rest materials)
  (let ((nodevar (gensym "NODE")))
    `(do-nodes% (,nodevar ,instance ,node)
       (setf (materials ,nodevar) (vector ,@(loop for material in materials
                                                  collect `(material ',material)))))))

(define-prefab-translator eval (instance asset args &rest body)
  `((lambda ,args
      (declare (ignorable ,@args))
      ,@body)
    ,instance ,asset))

(define-prefab-translator make-instance (instance asset class &rest args)
  `(enter (make-instance ,class ,@args)
          ,instance))
