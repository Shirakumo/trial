#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass shader-entity-class (standard-class)
  ((effective-shaders :initform () :accessor effective-shaders)
   (direct-shaders :initform () :initarg :shaders :accessor direct-shaders)
   (inhibited-shaders :initform () :initarg :inhibit-shaders :accessor inhibited-shaders)
   (effective-shader-class :accessor effective-shader-class)))

(defmethod c2mop:validate-superclass ((class shader-entity-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass shader-entity-class))
  NIL)

(defmethod c2mop:validate-superclass ((class shader-entity-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class shader-entity-class) (superclass shader-entity-class))
  T)

(defmethod compute-effective-shaders ((class shader-entity-class))
  (let ((effective-shaders ())
        (inhibited (inhibited-shaders class))
        (superclasses (remove 'shader-entity-class
                              (c2mop:compute-class-precedence-list class)
                              :test-not (lambda (type class) (typep class type)))))
    ;; Check whether inhibits are effective
    (loop for (name type) in inhibited
          for super = (find name superclasses :key #'class-name)
          do (cond ((not super)
                    (warn "No superclass ~s in hierarchy of ~s. Cannot inhibit its shader ~s." name (class-of super) (class-name class))
                    (setf (inhibited-shaders class) (remove (list name type) inhibited :test #'equal)))
                   ((not (getf (direct-shaders super) type))
                    (warn "No shader of type ~s is defined on ~s. Cannot inhibit it for ~s." type name (class-name class))
                    (setf (inhibited-shaders class) (remove (list name type) inhibited :test #'equal)))))
    ;; Compute effective inhibited list
    (loop for super in superclasses
          do (setf inhibited (append inhibited (inhibited-shaders super))))
    ;; Make all direct shaders effective
    (loop for (type shader) on (direct-shaders class) by #'cddr
          do (setf (getf effective-shaders type)
                   (list shader)))
    ;; Go through all superclasses in order
    (loop for super in superclasses
          do (loop for (type shader) on (direct-shaders super) by #'cddr
                   unless (find (list (class-name super) type) inhibited :test #'equal)
                   do (pushnew shader (getf effective-shaders type))))
    ;; Compute effective single shader sources
    (loop for (type shaders) on effective-shaders by #'cddr
          do (setf (getf effective-shaders type)
                   (glsl-toolkit:merge-shader-sources
                    (loop for (priority shader) in (stable-sort shaders #'> :key #'first)
                          collect (etypecase shader
                                    (string shader)
                                    (list (destructuring-bind (pool path) shader
                                            (pool-path pool path))))))))
    effective-shaders))

(defmethod compute-effective-shader-class ((class shader-entity-class))
  (if (direct-shaders class)
      class
      (let* ((effective-superclasses (list (find-class 'shader-entity))))
        ;; Loop through superclasses and push new, effective superclasses.
        (loop for superclass in (c2mop:class-direct-superclasses class)
              for effective-class = (effective-shader-class superclass)
              do (when (and effective-class (not (find effective-class effective-superclasses)))
                   (push effective-class effective-superclasses)))
        ;; If we have one or two --one always being the shader-entity class--
        ;; then we just return the more specific of the two, as there's no class
        ;; combination happening that would produce new shaders.
        (if (<= (length effective-superclasses) 2)
            (first effective-superclasses)
            class))))

(defmethod c2mop:finalize-inheritance :after ((class shader-entity-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (setf (effective-shaders class) (compute-effective-shaders class))
  (setf (effective-shader-class class) (compute-effective-shader-class class))
  (handle (make-instance 'class-changed :changed-class class) T))

(defmethod (setf direct-shaders) :after (value (class shader-entity-class))
  (when (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class)))

(defmethod effective-shaders ((class symbol))
  (effective-shaders (find-class class)))

(defmethod (setf effective-shaders) (value (class symbol))
  (setf (effective-shaders (find-class class)) value))

(defmethod direct-shaders ((class symbol))
  (direct-shaders (find-class class)))

(defmethod (setf direct-shaders) (value (class symbol))
  (setf (direct-shaders (find-class class)) value))

(defmethod class-shader (type (class shader-entity-class))
  (getf (direct-shaders class) type))

(defmethod class-shader (type (class symbol))
  (class-shader type (find-class class)))

(defmethod (setf class-shader) (shader type (class shader-entity-class))
  (setf (getf (direct-shaders class) type) shader))

(defmethod (setf class-shader) (shader type (class symbol))
  (setf (class-shader type (find-class class)) shader))

(defmethod remove-class-shader (type (class shader-entity-class))
  (remf (direct-shaders class) type))

(defmethod remove-class-shader (type (class symbol))
  (remove-class-shader type (find-class class)))

(defmethod effective-shader-class ((name symbol))
  (effective-shader-class (find-class name)))

(defmethod effective-shader-class ((class standard-class))
  NIL)

(defmethod make-class-shader-program ((class shader-entity-class))
  (make-instance 'shader-program
                 :shaders (loop for (type source) on (effective-shaders class) by #'cddr
                                collect (make-instance 'shader :source source :type type))))

(defmethod make-class-shader-program ((class symbol))
  (make-class-shader-program (find-class class)))

(defmacro define-class-shader ((class type &optional (priority 0)) &body definitions)
  `(setf (class-shader ,type ',class)
         (list ,priority (progn ,@definitions))))

(defclass shader-entity (entity)
  ()
  (:metaclass shader-entity-class))

(defmethod effective-shaders ((subject shader-entity))
  (effective-shaders (class-of subject)))

(defmethod (setf effective-shaders) (value (subject shader-entity))
  (setf (effective-shaders (class-of subject)) value))

(defmethod direct-shaders ((subject shader-entity))
  (direct-shaders (class-of subject)))

(defmethod (setf direct-shaders) (value (subject shader-entity))
  (setf (direct-shaders (class-of subject)) value))

(defmethod class-shader (type (subject shader-entity))
  (class-shader type (class-of subject)))

(defmethod (setf class-shader) (source type (subject shader-entity))
  (setf (class-shader type (class-of subject)) source))

(defmethod remove-class-shader (type (subject shader-entity))
  (remove-class-shader type (class-of subject)))

(defmethod effective-shader-class ((object shader-entity))
  (effective-shader-class (class-of object)))

(defmethod make-class-shader-program ((subject shader-entity))
  (make-class-shader-program (class-of subject)))

(defmacro define-shader-entity (&environment env name direct-superclasses direct-slots &rest options)
  (setf direct-superclasses (append direct-superclasses (list 'shader-entity)))
  (unless (find :metaclass options :key #'first)
    (push '(:metaclass shader-entity-class) options))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(define-class-shader (shader-entity :vertex-shader)
  "#version 330 core")

(define-class-shader (shader-entity :fragment-shader)
  "#version 330 core
out vec4 color;

void main(){
  color = vec4(1.0, 1.0, 1.0, 1.0);
}")
