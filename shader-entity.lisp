(in-package #:org.shirakumo.fraf.trial)

;; FIXME: allow parametrising class-shaders so that things like
;;        constants can be backed-in, but still be changed for each
;;        instance without needing to change source code

(defgeneric resolve-shader-include (source))

(defmethod resolve-shader-include ((source string))
  (if (position #\: source)
      (resolve-shader-include (read-from-string source))
      (resolve-shader-include (parse-namestring (read-from-string source)))))

(defmethod resolve-shader-include ((source pathname))
  (let* ((source (merge-pathnames source *default-pathname-defaults*))
         (*default-pathname-defaults* source)
         (parts (glsl-toolkit:preprocess (glsl-toolkit:parse source) :include-resolution #'resolve-shader-include)))
    (or (getf parts :global)
        (loop for (k v) on parts by #'cddr thereis v))))

(defmethod resolve-shader-include ((source symbol))
  (gl-source (find-class source)))

(defmethod resolve-shader-include ((source cons))
  (destructuring-bind (pool name) source
    (etypecase name
      (symbol
       (gl-source (asset pool name T)))
      ((or string pathname)
       (resolve-shader-include (pool-path pool name))))))

(defclass buffer-slot-definition ()
  ((buffer-type :initarg :buffer :initform NIL :accessor buffer-type)
   (binding :initarg :binding :initform NIL :accessor binding)
   (qualifiers :initarg :qualifiers :initform () :accessor qualifiers)))

(defmethod print-object ((slotdef buffer-slot-definition) stream)
  (print-unreadable-object (slotdef stream :type T)
    (format stream "~s ~s" (c2mop:slot-definition-name slotdef) (buffer-type slotdef))))

(defmethod gl-source ((definition buffer-slot-definition))
  (case (buffer-type definition)
    ((T) NIL)
    (vertex-buffer
     (gl-source (make-instance 'vertex-buffer :binding (binding definition)
                                              :qualifiers (qualifiers definition))))
    (T
     (gl-source (make-instance 'shader-storage-buffer :binding (binding definition)
                                                      :qualifiers (qualifiers definition)
                                                      :struct-class (buffer-type definition))))))

(defclass direct-buffer-slot-definition (buffer-slot-definition c2mop:standard-direct-slot-definition) ())
(defclass effective-buffer-slot-definition (buffer-slot-definition c2mop:standard-effective-slot-definition) ())

(defclass constant-slot-definition ()
  ((constant-name :initarg :constant :initform NIL :accessor constant-name)))

(defmethod shared-initialize :after ((definition constant-slot-definition) slots &key constant)
  (when (eql T constant)
    (setf (constant-name definition) (string-upcase (symbol->c-name (c2mop:slot-definition-name definition))))))

(defmethod print-object ((slotdef constant-slot-definition) stream)
  (print-unreadable-object (slotdef stream :type T)
    (format stream "~s ~s" (c2mop:slot-definition-name slotdef) (constant-name slotdef))))

(defclass direct-constant-slot-definition (constant-slot-definition c2mop:standard-direct-slot-definition) ())
(defclass effective-constant-slot-definition (constant-slot-definition c2mop:standard-effective-slot-definition) ())

(defclass uniform-slot-definition ()
  ((uniform-name :initarg :uniform :initform NIL :accessor uniform-name)))

(defmethod shared-initialize :after ((definition uniform-slot-definition) slots &key uniform)
  (when (eql T uniform)
    (setf (uniform-name definition) (symbol->c-name (c2mop:slot-definition-name definition)))))

(defmethod print-object ((slotdef uniform-slot-definition) stream)
  (print-unreadable-object (slotdef stream :type T)
    (format stream "~s ~s" (c2mop:slot-definition-name slotdef) (uniform-name slotdef))))

(defclass direct-uniform-slot-definition (uniform-slot-definition c2mop:standard-direct-slot-definition) ())
(defclass effective-uniform-slot-definition (uniform-slot-definition c2mop:standard-effective-slot-definition) ())

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

(defmethod shared-initialize :after ((class shader-entity-class) slots &key shader-file)
  (when shader-file
    (destructuring-bind (pool path) (first shader-file)
      (let ((*default-pathname-defaults* (pool-path pool path)))
        (loop for (type source) on (glsl-toolkit:preprocess *default-pathname-defaults* :include-resolution #'resolve-shader-include) by #'cddr
              do (setf (getf (direct-shaders class) type) (list 0 source)))))))

;; SIGH. Why oh why do we have to replicate this shit just to customise the effective slot definition
;; class conditionally.
(defun compute-effective-slot-definition-initargs (direct-slotds)
  (let ((args (list :name (c2mop:slot-definition-name (first direct-slotds)) :type T))
        (_ '#:no-value))
    (dolist (slotd direct-slotds args)
      (when slotd
        (when (and (eq _ (getf args :initfunction _)) (c2mop:slot-definition-initfunction slotd))
          (setf (getf args :initfunction) (c2mop:slot-definition-initfunction slotd))
          (setf (getf args :initform) (c2mop:slot-definition-initform slotd)))
        (when (and (eq _ (getf args :documentation _)) (documentation slotd T))
          (setf (getf args :documentation) (documentation slotd T)))
        (when (and (eq _ (getf args :allocation _)) (c2mop:slot-definition-allocation slotd))
          (setf (getf args :allocation) (c2mop:slot-definition-allocation slotd)))
        (setf (getf args :initargs) (union (getf args :initargs) (c2mop:slot-definition-initargs slotd)))
        (let ((slotd-type (c2mop:slot-definition-type slotd)))
          (setf (getf args :type) (cond ((eq (getf args :type) T) slotd-type)
                                        (T `(and ,slotd-type ,(getf args :type))))))))))

(defmethod c2mop:compute-effective-slot-definition ((class shader-entity-class) name direct-slots)
  (let ((initargs (compute-effective-slot-definition-initargs direct-slots))
        (uniform (loop for direct in direct-slots
                       thereis (when (typep direct 'uniform-slot-definition) direct)))
        (constant (loop for direct in direct-slots
                        thereis (when (typep direct 'constant-slot-definition) direct)))
        (buffer (loop for direct in direct-slots
                      thereis (when (typep direct 'buffer-slot-definition) direct))))
    (cond ((and (or buffer uniform constant) (not (xor buffer uniform constant)))
           (error "Slot ~s cannot be constant and uniform at the same time." name))
          (uniform
           (setf initargs (list* :uniform (uniform-name uniform) initargs))
           (apply #'make-instance (apply #'c2mop:effective-slot-definition-class class initargs) initargs))
          (constant
           (setf initargs (list* :constant (constant-name constant) initargs))
           (apply #'make-instance (apply #'c2mop:effective-slot-definition-class class initargs) initargs))
          (buffer
           (setf initargs (list* :buffer (buffer-type buffer) :qualifiers (qualifiers buffer) :binding (binding buffer) initargs))
           (apply #'make-instance (apply #'c2mop:effective-slot-definition-class class initargs) initargs))
          (T
           (call-next-method)))))

(defmethod c2mop:direct-slot-definition-class ((class shader-entity-class) &key name uniform constant buffer)
  (cond ((and (or buffer uniform constant) (not (xor buffer uniform constant)))
         (error "Slot ~s cannot be constant and uniform at the same time." name))
        (uniform
         'direct-uniform-slot-definition)
        (constant
         'direct-constant-slot-definition)
        (buffer
         'direct-buffer-slot-definition)
        (T
         (call-next-method))))

(defmethod c2mop:effective-slot-definition-class ((class shader-entity-class) &key name uniform constant buffer)
  (cond ((and (or buffer uniform constant) (not (xor buffer uniform constant)))
         (error "Slot ~s cannot be constant and uniform at the same time." name))
        (uniform
         'effective-uniform-slot-definition)
        (constant
         'effective-constant-slot-definition)
        (buffer
         'effective-buffer-slot-definition)
        (T
         (call-next-method))))

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
                   (mapcar #'second (stable-sort shaders #'> :key #'first))))
    ;; Add constant slot definitions
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
  (when (and +main+ (slot-boundp +main+ 'scene) (scene +main+))
    (handle (make-event 'class-changed :changed-class class) +main+)))

(defmethod apply-class-changes ((class shader-entity-class))
  (call-next-method)
  (setf (effective-shaders class) (compute-effective-shaders class))
  (setf (effective-buffers class) (compute-effective-buffers class))
  (setf (effective-shader-class class) (compute-effective-shader-class class))
  (when (and +main+ (scene +main+))
    (handle (make-event 'class-changed :changed-class class) +main+)))

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

(defmethod make-shader-program ((class shader-entity-class))
  (make-instance 'shader-program
                 :shaders (loop for (type source) on (effective-shaders class) by #'cddr
                                for processed = (glsl-toolkit:merge-shader-sources
                                                 (list (glsl-toolkit:combine-methods source))
                                                 :min-version (glsl-target-version T))
                                collect (make-instance 'shader :source processed :type type))
                 :buffers (loop for resource-spec in (effective-buffers class)
                                collect (apply #'// resource-spec))))

(defmethod make-shader-program ((class symbol))
  (make-shader-program (find-class class)))

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

(defmacro undefine-class-shader ((class type &optional (priority 0)) &body definitions)
  (declare (ignore priority definitions))
  `(remove-class-shader ',type ',class))

(defmethod describe-object ((class shader-entity-class) stream)
  (call-next-method)
  (loop for (type parts) on (compute-effective-shaders class) by #'cddr
        do (format stream "~&~%~a:~%" type)
           (format-with-line-numbers
            (glsl-toolkit:merge-shader-sources
             (list (glsl-toolkit:combine-methods parts)))
            stream)))

(defmethod buffers ((object shader-entity-class))
  (loop for spec in (effective-buffers object)
        collect (apply #'// spec)))

(defmethod buffer-sources ((object shader-entity-class))
  (loop for spec in (effective-buffers object)
        collect (gl-source (apply #'// spec))))

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

(defmethod stage :after ((object shader-entity) (area staging-area))
  (dolist (buffer (buffers object))
    (stage buffer area)))

(defmethod compute-preprocessor-directives ((class shader-entity-class))
  (loop for slot in (c2mop:class-slots class)
        for name = (c2mop:slot-definition-name slot)
        when (typep slot 'constant-slot-definition)
        collect `(glsl-toolkit:preprocessor-directive
                  ,(format NIL "#define ~a"
                           (constant-name slot)))))

(defmethod compute-preprocessor-directives ((entity shader-entity))
  (loop for slot in (c2mop:class-slots (class-of entity))
        for name = (c2mop:slot-definition-name slot)
        when (typep slot 'constant-slot-definition)
        collect `(glsl-toolkit:preprocessor-directive
                  ,(format NIL "#define ~a~@[ ~a~]"
                           (constant-name slot)
                           (when (slot-boundp entity name)
                             (slot-value entity name))))))

(defmethod shader-source ((class shader-entity-class))
  (loop with constants = `(glsl-toolkit:shader ,@(compute-preprocessor-directives class))
        with sources = (buffer-sources class)
        for (type source) on (effective-shaders class) by #'cddr
        for processed = (glsl-toolkit:merge-shader-sources
                         (append (list* constants sources)
                                 (list (glsl-toolkit:combine-methods source)))
                         :min-version (glsl-target-version T))
        collect (list type processed)))

(defmethod shader-source ((entity shader-entity))
  (loop with constants = `(glsl-toolkit:shader ,@(compute-preprocessor-directives entity))
        with sources = (buffer-sources entity)
        for (type source) on (effective-shaders entity) by #'cddr
        for processed = (glsl-toolkit:merge-shader-sources
                         (append (list* constants sources)
                                 (list (glsl-toolkit:combine-methods source)))
                         :min-version (glsl-target-version T))
        collect (list type processed)))

(defmethod make-shader-program ((entity shader-entity))
  (make-instance 'shader-program
                 :shaders (loop for (type source) in (shader-source entity)
                                collect (make-instance 'shader :type type :source source))
                 :buffers (buffers entity)))

;; This is hacky and I hate it.
(defmethod buffer-sources ((object shader-entity))
  (append (buffer-sources (class-of object))
          (loop for slot in (c2mop:class-slots (class-of object))
                when (typep slot 'buffer-slot-definition)
                collect (or (gl-source slot)
                            (gl-source (or (slot-value object (c2mop:slot-definition-name slot))
                                           (error "Buffer slot~%  ~s~%is unset in~%  ~a!"
                                                  (c2mop:slot-definition-name slot) object)))))))

(defmethod buffers ((object shader-entity))
  (delete-duplicates
   (append (buffers (class-of object))
           (loop for slot in (c2mop:class-slots (class-of object))
                 when (typep slot 'buffer-slot-definition)
                 collect (or (slot-value object (c2mop:slot-definition-name slot))
                             (error "Buffer slot~%  ~s~%is unset in~%  ~a!"
                                    (c2mop:slot-definition-name slot) object))))))

(defmethod update-uniforms ((object shader-entity) program)
  ;; TODO: this is slow. We *could* COMPILE on finalize-inheritance, but that would be ugly.
  (loop for slot in (c2mop:class-slots (class-of object))
        when (typep slot 'uniform-slot-definition)
        do (setf (uniform program (uniform-name slot))
                 (c2mop:standard-instance-access object (c2mop:slot-definition-location slot)))))

(defmacro define-shader-entity (name direct-superclasses direct-slots &rest options)
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
  ((shader-program :initarg :shader-program :accessor shader-program))
  (:metaclass shader-entity-class))

(defmethod initialize-instance :around ((entity standalone-shader-entity) &key)
  (call-next-method)
  (unless (slot-boundp entity 'shader-program)
    (setf (shader-program entity) (make-shader-program entity))))

(defmethod stage :after ((entity standalone-shader-entity) (area staging-area))
  (stage (shader-program entity) area))

(defmethod render ((entity standalone-shader-entity) target)
  (let ((program (shader-program entity)))
    (activate program)
    (update-uniforms entity program)
    (bind-textures entity)
    (render entity program)))

(defclass dynamic-shader-entity (standalone-shader-entity)
  ((shaders :initarg :shaders :initform () :accessor shaders)
   (buffers :initarg :buffers :initform () :accessor buffers))
  (:metaclass shader-entity-class))

(defmethod make-shader-program ((entity dynamic-shader-entity))
  (let ((class (class-of entity)))
    (make-instance 'shader-program
                   :shaders (loop with shaders = (shaders entity)
                                  for (type source) on (effective-shaders class) by #'cddr
                                  for dynamic = (getf shaders type)
                                  for processed = (glsl-toolkit:merge-shader-sources
                                                   (list (glsl-toolkit:combine-methods
                                                          (if dynamic (list* dynamic source) source)))
                                                   :min-version (glsl-target-version T))
                                  collect (make-instance 'shader :source processed :type type))
                   :buffers (append (loop for resource-spec in (effective-buffers class)
                                          collect (apply #'// resource-spec))
                                    (buffers entity)))))
