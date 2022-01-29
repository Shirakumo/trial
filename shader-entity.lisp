#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: allow parametrising class-shaders so that things like
;;        constants can be backed-in, but still be changed for each
;;        instance without needing to change source code

(defclass shader-entity-class (standard-class)
  ((effective-shaders :initform () :accessor effective-shaders)
   (direct-shaders :initform () :initarg :shaders :accessor direct-shaders)
   (inhibited-shaders :initform () :initarg :inhibit-shaders :accessor inhibited-shaders)
   (effective-buffers :initform () :accessor effective-buffers)
   (direct-buffers :initform () :initarg :buffers :accessor direct-buffers)
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
                    (mapcar #'second (stable-sort shaders #'> :key #'first))
                    :min-version NIL)))
    effective-shaders))

(defmethod compute-effective-buffers ((class shader-entity-class))
  (let ((effective-buffers ())
        (superclasses (remove 'shader-entity-class
                              (c2mop:compute-class-precedence-list class)
                              :test-not (lambda (type class) (typep class type)))))
    ;; FIXME: inhibition logic
    (loop for super in superclasses
          do (loop for buffer in (direct-buffers super)
                   do (pushnew buffer effective-buffers :test #'equal)))
    effective-buffers))

(defmethod compute-effective-shader-class ((class shader-entity-class))
  (if (or (direct-shaders class)
          ;; break-out path for shader-entity.
          (null (find-class 'shader-entity NIL)))
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
  (setf (effective-buffers class) (compute-effective-buffers class))
  (setf (effective-shader-class class) (compute-effective-shader-class class))
  (when +main+
    (handle (make-instance 'class-changed :changed-class class) +main+)))

(defmethod apply-class-changes ((class shader-entity-class))
  (call-next-method)
  (setf (effective-shaders class) (compute-effective-shaders class))
  (setf (effective-buffers class) (compute-effective-buffers class))
  (setf (effective-shader-class class) (compute-effective-shader-class class))
  (when +main+
    (handle (make-instance 'class-changed :changed-class class) +main+)))

(defmethod (setf direct-shaders) :after (value (class shader-entity-class))
  (when (c2mop:class-finalized-p class)
    (apply-class-changes class)))

(defmethod (setf direct-buffers) :after (value (class shader-entity-class))
  (when (c2mop:class-finalized-p class)
    (apply-class-changes class)))

(defmethod class-shader (type (class shader-entity-class))
  (getf (direct-shaders class) type))

(defmethod class-shader (type (class symbol))
  (class-shader type (find-class class)))

(defmethod (setf class-shader) (shader type (class shader-entity-class))
  (check-shader-type type)
  (setf (getf (direct-shaders class) type) shader))

(defmethod (setf class-shader) (shader type (class symbol))
  (setf (class-shader type (find-class class)) shader))

(defmethod remove-class-shader (type (class shader-entity-class))
  (remf (direct-shaders class) type))

(defmethod remove-class-shader (type (class symbol))
  (remove-class-shader type (find-class class)))

(defmethod effective-shader (type thing)
  (getf (effective-shaders thing) type))

(defmethod effective-shader-class ((name symbol))
  (effective-shader-class (find-class name)))

(defmethod effective-shader-class ((class standard-class))
  NIL)

(defmethod make-class-shader-program ((class shader-entity-class))
  (make-instance 'shader-program
                 :shaders (loop for (type source) on (effective-shaders class) by #'cddr
                                collect (make-instance 'shader :source source :type type))
                 :buffers (loop for resource-spec in (effective-buffers class)
                                collect (apply #'// resource-spec))))

(defmethod make-class-shader-program ((class symbol))
  (make-class-shader-program (find-class class)))

(defun combine-shader-sources (&rest sources)
  (list* 'glsl-toolkit:shader
         (loop for source in sources
               for parsed = (etypecase source
                              (cons source)
                              (T (glsl-toolkit:parse source)))
               append (case (car parsed)
                        (glsl-toolkit:shader
                         (cdr parsed))
                        (T (list parsed))))))

(defmacro define-class-shader ((class type &optional (priority 0)) &body definitions)
  `(setf (class-shader ,type ',class)
         (list ,priority (combine-shader-sources ,@definitions))))

(defclass shader-entity (entity)
  ()
  (:metaclass shader-entity-class))

(define-accessor-wrapper-methods effective-shaders
  (symbol (find-class symbol))
  (shader-entity (class-of shader-entity)))

(define-accessor-wrapper-methods direct-shaders
  (symbol (find-class symbol))
  (shader-entity (class-of shader-entity)))

(define-accessor-wrapper-methods effective-buffers
  (symbol (find-class symbol))
  (shader-entity (class-of shader-entity)))

(define-accessor-wrapper-methods direct-buffers
  (symbol (find-class symbol))
  (shader-entity (class-of shader-entity)))

(defmethod class-shader (type (entity shader-entity))
  (class-shader type (class-of entity)))

(defmethod (setf class-shader) (source type (entity shader-entity))
  (setf (class-shader type (class-of entity)) source))

(defmethod remove-class-shader (type (entity shader-entity))
  (remove-class-shader type (class-of entity)))

(defmethod effective-shader-class ((object shader-entity))
  (effective-shader-class (class-of object)))

(defmethod make-class-shader-program ((entity shader-entity))
  (make-class-shader-program (class-of entity)))

(defmacro define-shader-entity (&environment env name direct-superclasses direct-slots &rest options)
  (setf direct-superclasses (append direct-superclasses (list 'shader-entity)))
  (unless (find :metaclass options :key #'first)
    (push '(:metaclass shader-entity-class) options))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(define-class-shader (shader-entity :fragment-shader)
  "
out vec4 color;

void main(){
  color = vec4(1.0, 1.0, 1.0, 1.0);
}")

(defclass standalone-shader-entity (shader-entity)
  ((shader-program :accessor shader-program))
  (:metaclass shader-entity-class))

(defmethod initialize-instance :after ((entity standalone-shader-entity) &key)
  (setf (shader-program entity) (make-class-shader-program entity)))

(defmethod stage :after ((entity standalone-shader-entity) (area staging-area))
  (stage (shader-program entity) area))

(defmethod render ((entity standalone-shader-entity) target)
  (let ((program (shader-program entity)))
    (gl:use-program (gl-name program))
    (render entity program)))
