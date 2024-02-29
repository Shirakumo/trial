(in-package #:org.shirakumo.fraf.trial)

(defvar *materials* (make-hash-table :test 'equal))

(define-asset (trial random) image
    #p "random.png"
  :min-filter :linear
  :mag-filter :linear
  :wrapping '(:repeat :repeat :repeat))

(define-asset (trial missing) image
    #p "missing.png"
  :min-filter :nearest
  :mag-filter :nearest)

(define-asset (trial black) image
    #p "black.png"
  :min-filter :nearest
  :mag-filter :nearest)

(define-asset (trial white) image
    #p "white.png"
  :min-filter :nearest
  :mag-filter :nearest)

(define-asset (trial neutral-mro) image
    #p "neutral-mro.png"
  :min-filter :nearest
  :mag-filter :nearest)

(define-asset (trial neutral-normal) image
    #p "neutral-normal.png"
  :min-filter :nearest
  :mag-filter :nearest)

(defclass material ()
  ((name :initform NIL :initarg :name :accessor name)
   (double-sided-p :initform NIL :initarg :double-sided :accessor double-sided-p)
   (textures :initform #() :accessor textures)))

(defmethod stage ((material material) (area staging-area))
  (loop for texture across (textures material)
        do (stage texture area)))

(defmethod dependencies ((material material))
  (coerce (textures material) 'list))

(defgeneric texture-names (material))

(defun same-texture-p (resource source)
  (and (typep resource 'texture)
       (generator resource)
       (equal (input* (generator resource)) source)))

(defmethod shared-initialize :after ((material material) slots &rest args &key &allow-other-keys)
  (loop for i from 0 below (length (texture-names material))
        for texture = (getf args (aref (texture-names material) i))
        ;; Try to gracefully update material textures.
        do (etypecase texture
             (null)
             (resource
              (setf (aref (textures material) i) texture))
             (image
              (setf (aref (textures material) i) (resource texture T)))
             (pathname
              (unless (same-texture-p (aref (textures material) i) texture)
                (setf (aref (textures material) i)
                      (first (generate-resources 'image-loader texture)))))
             (cons
              (unless (same-texture-p (aref (textures material) i) texture)
                (setf (aref (textures material) i)
                      (first (apply #'generate-resources 'image-loader texture))))))))

(defmethod material ((name string))
  (or (gethash name *materials*)
      (error "No material named ~s" name)))

(defmethod material ((name symbol))
  (or (gethash name *materials*)
      (error "No material named ~s" name)))

(defmethod (setf material) ((material material) (name string))
  (setf (gethash name *materials*) material))

(defmethod (setf material) ((material material) (name symbol))
  (setf (gethash name *materials*) material))

(defmethod (setf material) ((null null) (name string))
  (remhash name *materials*)
  null)

(defmethod (setf material) ((null null) (name symbol))
  (remhash name *materials*)
  null)

(defun list-materials ()
  (alexandria:hash-table-values *materials*))

(defun update-material (name type &rest initargs)
  (let ((existing (gethash name *materials*)))
    (cond ((null existing)
           (setf (material name) (apply #'make-instance type :name name :storage NIL initargs)))
          ((eql type (type-of existing))
           (apply #'reinitialize-instance existing initargs))
          (T
           (apply #'change-class existing type initargs)))))

(defmacro define-material ((name type) &body args)
  `(update-material ',name ',type ,@args))
