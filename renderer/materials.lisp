(in-package #:org.shirakumo.fraf.trial)

(defvar *materials* (make-hash-table :test 'equal))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-image (w h c fun)
    (let ((array (make-array (* w h c) :element-type '(unsigned-byte 8)))
          (i 0))
      (dotimes (y h array)
        (dotimes (x w)
          (dotimes (z c)
            (setf (aref array i) (funcall fun x y z))
            (incf i)))))))

(defmacro with-image ((w h c &optional (x 'x) (y 'y) (z 'z)) &body body)
  `(gen-image ,w ,h ,c (lambda (,x ,y ,z)
                         (declare (ignorable ,x ,y ,z))
                         ,@body)))

(define-asset (trial random) static 'texture
  :pixel-data (with-image (512 512 4) (random 256))
  :width 512 :height 512 :internal-format :rgba
  :min-filter :linear :mag-filter :linear
  :wrapping '(:repeat :repeat :repeat))

(define-asset (trial missing) static 'texture
  :pixel-data (with-image (16 16 3)
                (if (< (mod (+ x y) 16) 8)
                    (case z (0 83) (1 0) (2 83))
                    (case z (0 255) (1 0) (2 255))))
  :width 16 :height 16 :internal-format :rgb
  :min-filter :nearest :mag-filter :nearest
  :wrapping '(:repeat :repeat :repeat))

(define-asset (trial black) static 'texture
  :pixel-data (with-image (1 1 3) 0)
  :width 1 :height 1 :internal-format :rgb
  :min-filter :nearest  :mag-filter :nearest)

(define-asset (trial white) static 'texture
  :pixel-data (with-image (1 1 3) 255)
  :width 1 :height 1 :internal-format :rgb
  :min-filter :nearest :mag-filter :nearest)

(define-asset (trial neutral-mro) static 'texture
  :pixel-data (with-image (1 1 3) (case z ((0 1) 255) (2 0)))
  :width 1 :height 1 :internal-format :rgb
  :min-filter :nearest :mag-filter :nearest)

(define-asset (trial neutral-normal) static 'texture
  :pixel-data (with-image (1 1 3) (case z ((0 1) 128) (2 255)))
  :width 1 :height 1 :internal-format :rgb
  :min-filter :nearest :mag-filter :nearest)

(defclass material ()
  ((name :initform NIL :initarg :name :accessor name)
   (double-sided-p :initform NIL :initarg :double-sided :accessor double-sided-p)
   (transparent-p :initform NIL :initarg :transparent-p :accessor transparent-p)
   (textures :initform #() :accessor textures)))

(defmethod print-object ((material material) stream)
  (print-unreadable-object (material stream :type T :identity (null (name material)))
    (format stream "~@[~s~]" (name material))))

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
