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

(defclass prefab () ())

(defmethod observe-load-state :before ((prefab prefab) (asset model-file) (state (eql :loaded)) (area staging-area))
  (instantiate-prefab prefab asset))

(defmethod stage :before ((prefab prefab) (area staging-area))
  (register-load-observer area prefab (prefab-asset prefab))
  (stage (prefab-asset prefab) area))

(defmethod instantiate-prefab ((prefab symbol) asset)
  (apply #'instantiate-prefab (make-instance prefab) asset))

(defmethod instantiate-prefab ((prefab prefab) (asset (eql T)))
  (instantiate-prefab prefab (prefab-asset prefab)))

(defmacro define-prefab-instantiation (class asset &body changes)
  (let ((assetvar (gensym "ASSET")))
    `(progn
       (defmethod prefab-asset ((,class ,class))
         (asset ,@(loop for part in asset collect `',part)))
       
       (defmethod instantiate-prefab ((,class ,class) (,assetvar model-file))
         ,@(loop for (type . args) in (or changes '((reparent)))
                 collect (expand-prefab-expression type class assetvar args))
         ,class))))

(define-prefab-translator scaling (instance asset scale &optional node)
  `(setf (scaling ,(if node `(node ,node ,instance) instance)) ,scale))

(define-prefab-translator location (instance asset location &optional node)
  `(setf (location ,(if node `(node ,node ,instance) instance)) ,location))

(define-prefab-translator orientation (instance asset quat &optional node)
  `(setf (orientation ,(if node `(node ,node ,instance) instance)) ,quat))

(define-prefab-translator enter (instance asset &optional node)
  `(sequence:dosequence (child ,(if node
                                    `(node ,node ,asset)
                                    `(find-scene T ,asset)))
     (enter (clone child) ,instance)))

(define-prefab-translator leave (instance asset &optional node)
  `(leave (node ,node ,instance) T))

(define-prefab-translator physics-primitives (instance asset node)
  (let ((nodevar (gensym "NODE")))
    `(let ((,nodevar (node ,node ,instance)))
       (setf (physics-primitives ,instance) (physics-primitives ,nodevar))
       (setf (mass ,instance) (mass ,nodevar))
       (typecase ,nodevar
         (animated-entity
          (change-class ,nodevar 'basic-animated-entity))
         (T
          (change-class ,nodevar 'basic-entity))))))

(define-prefab-translator change-class (instance asset node class)
  `(change-class (node ,node ,instance) ',class))

(define-prefab-translator play (instance asset animation)
  `(play ,animation ,instance))
