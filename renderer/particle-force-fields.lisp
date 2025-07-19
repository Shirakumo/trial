(in-package #:org.shirakumo.fraf.trial)

(define-global +particle-force-field-types+ (make-array 64 :initial-element NIL))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-gl-struct (particle-force-field :layout-standard std430)
    (type :int :initform 0)
    (position :vec3 :initform (vec 0 0 0))
    (strength :float :initform 0.0)
    (range :float :initform 0.0)
    (inv-range :float :initform 0.0)
    (normal :vec3 :initform (vec 0 0 0))))

(defstruct particle-force-field-type
  (id 0 :type (unsigned-byte 8))
  (name NIL :type symbol)
  (shader-source NIL :type T)
  (function NIL :type function))

(define-structure-delegate-methods particle-force-field-type id name shader-source)

(defmethod print-object ((type particle-force-field-type) stream)
  (print-unreadable-object (type stream :type T)
    (format stream "~d ~s" (id type) (name type))))

(defmethod describe-object ((type particle-force-field-type) stream)
  (call-next-method)
  (format stream "~&~%Shader:~%")
  (format-with-line-numbers (glsl-toolkit:serialize (shader-source type) NIL) stream))

(defun map-particle-force-field-type (type)
  (typecase type
    (fixnum type)
    (T (position type +particle-force-field-types+ :key
                 (lambda (x) (if x (particle-force-field-type-name x)))))))

(defun list-particle-force-field-types ()
  (loop for type across +particle-force-field-types+
        when type collect type))

(defun %particle-force-field-type (id)
  (aref +particle-force-field-types+ id))

(defun particle-force-field-type (name &key (if-does-not-exist :error))
  (let ((id (map-particle-force-field-type name)))
    (or (when id (aref +particle-force-field-types+ id))
        (ecase if-does-not-exist
          (:error (error "No particle force field type with name~%  ~s" name))
          (:create
           (setf id (or (map-particle-force-field-type NIL)
                        (error "Ran out of particle force field type slots.")))
           (setf (aref +particle-force-field-types+ id)
                 (make-particle-force-field-type :id id :name name :shader-source "" :function #'identity)))
          ((NIL) NIL)))))

(defmacro define-particle-force-field-type (name args field-args &body body)
  (destructuring-bind (force &optional (location (gensym "LOCATION")) (velocity (gensym "VELOCITY"))
                                       (dt (gensym "DT")) (age (gensym "AGE"))) args
    (destructuring-bind (&key position strength range inv-range normal) field-args
      (form-fiddle:with-destructured-lambda-form (:declarations decls :forms body) `(lambda () ,@body)
        `(let ((type (particle-force-field-type ',name :if-does-not-exist :create)))
           (setf (particle-force-field-type-function type)
                 (lambda (field ,force ,location ,velocity ,age ,dt)
                   (declare (ignorable ,location ,velocity ,age ,dt))
                   (declare (type vec3 ,force ,location ,velocity))
                   (declare (type single-float ,dt ,age))
                   ,@decls
                   (with-gl-slots (particle-force-field
                                   ,@(if position `((position ,position)))
                                   ,@(if strength `((strength ,strength)))
                                   ,@(if range `((range ,range)))
                                   ,@(if inv-range `((inv-range ,inv-range)))
                                   ,@(if normal `((normal ,normal))))
                                  field
                     ,@body))))))))

(defmacro define-particle-force-field-shader (name (force-field particle) &body shader)
  `(let ((type (particle-force-field-type ',name :if-does-not-exist :create)))
     (setf (particle-force-field-type-shader-source type)
           (glsl-toolkit:parse
            (format NIL "vec3 evaluate_force_field_~(~a~)(in ParticleForceField ~(~a~), in Particle ~(~a~)){~{~%  ~a~}~%}"
                    (particle-force-field-type-name type)
                    ',force-field ',particle
                    (list ,@shader))))))

(defun particle-force-field-shader ()
  (append '(glsl-toolkit:shader)
          (loop for type across +particle-force-field-types+
                when type append (rest (shader-source type)))
          `((GLSL-TOOLKIT:FUNCTION-DEFINITION
             (GLSL-TOOLKIT:FUNCTION-PROTOTYPE GLSL-TOOLKIT:NO-VALUE
              (GLSL-TOOLKIT:TYPE-SPECIFIER :VEC3) "evaluate_force_field"
              ((GLSL-TOOLKIT:TYPE-QUALIFIER :IN)
               (GLSL-TOOLKIT:TYPE-SPECIFIER (GLSL-TOOLKIT:TYPE-NAME "ParticleForceField"))
               "field")
              ((GLSL-TOOLKIT:TYPE-QUALIFIER :IN)
               (GLSL-TOOLKIT:TYPE-SPECIFIER (GLSL-TOOLKIT:TYPE-NAME "Particle"))
               "particle"))
             (GLSL-TOOLKIT:COMPOUND-STATEMENT
              (GLSL-TOOLKIT:SWITCH-STATEMENT
               (GLSL-TOOLKIT:MODIFIED-REFERENCE "field" (GLSL-TOOLKIT:FIELD-MODIFIER "type"))
               (GLSL-TOOLKIT:COMPOUND-STATEMENT
                ,@(loop for type across +particle-force-field-types+
                        when type append `((GLSL-TOOLKIT:CASE-LABEL ,(id type))
                                           (RETURN (GLSL-TOOLKIT:MODIFIED-REFERENCE
                                                    ,(format NIL "evaluate_force_field_~(~a~)" (particle-force-field-type-name type))
                                                    (GLSL-TOOLKIT:CALL-MODIFIER "field" "particle")))))
                (GLSL-TOOLKIT:CASE-LABEL :DEFAULT)
                (RETURN (GLSL-TOOLKIT:MODIFIED-REFERENCE :VEC3 (GLSL-TOOLKIT:CALL-MODIFIER 0))))))))))

(define-particle-force-field-type :null (force) ()
  (declare (ignore force))
  NIL)

(define-particle-force-field-shader :null (field particle)
  "return vec3(0);")

(define-particle-force-field-type :point (force location)
    (:position position :strength strength :inv-range inv-range)
  (let ((dir (v- position location)))
    (declare (dynamic-extent dir))
    (nv+* force dir (* strength (- 1 (clamp 0.0 (* (vlength dir) inv-range) 1.0))))))

(define-particle-force-field-shader :point (field particle)
  "vec3 dir = field.position - particle.position;"
  "return dir * field.strength * (1 - clamp(length(dir) * field.inv_range, 0.0, 1.0));")

(define-particle-force-field-type :direction (force)
    (:normal normal :strength strength)
  (nv+* force normal strength))

(define-particle-force-field-shader :direction (field particle)
  "return field.normal * field.strength;")

(define-particle-force-field-type :plane (force location)
    (:position position :normal normal :strength strength :inv-range inv-range)
  (let ((dist (v. normal (v- location position))))
    (nv+* force normal (* strength (- 1 (clamp 0.0 (* dist inv-range) 1.0))))))

(define-particle-force-field-shader :plane (field particle)
  "float dist = dot(field.normal, particle.position - field.position);"
  "return field.normal * field.strength * (1 - clamp(dist * field.inv_range, 0.0, 1.0));")

(define-particle-force-field-type :vortex (force location)
    (:position position :normal normal :strength strength :inv-range inv-range)
  (let* ((dir (v- location position))
         (t0 (/ (v. normal dir) (v. normal normal)))
         (dist (vdistance location (v* position t0)))
         (perp (nvunit* (vc normal dir))))
    (declare (dynamic-extent dir perp))
    (nv+* force perp (* strength (- 1 (clamp 0.0 (* dist inv-range) 1.0))))))

(define-particle-force-field-shader :vortex (field particle)
  "vec3 dir = particle.position - field.position;"
  "float t0 = dot(field.normal, dir) / dot(field.normal, field.normal);"
  "float dist = distance(particle.position, field.position*t0);"
  "vec3 perp = normalize(cross(field.normal, dir));"
  "return perp * field.strength * (1 - clamp(dist * field.inv_range, 0.0, 1.0));")

(define-particle-force-field-type :sphere (force location velocity dt)
    (:position position :range range)
  (let* ((dir (v- position location))
         (dist (vlength dir)))
    (declare (dynamic-extent dir))
    (when (< dist range)
      (let* ((push (nvunit (nv- dir)))
             (slide (nvc (vc velocity push) (nv- push))))
        (declare (dynamic-extent push slide))
        (nv+* force (nv- slide velocity) (/ dt))))))

(define-particle-force-field-shader :sphere (field particle)
  "vec3 dir = field.position - particle.position;"
  "float dist = length(dir);"
  "if(dist < field.range){"
  "  vec3 push = normalize(-dir);"
  "  vec3 slide = cross(cross(particle.velocity, push), -push);"
  "  return (slide-particle.velocity) / dt;"
  "}"
  "return vec3(0);")

(define-particle-force-field-type :planet (force location velocity dt)
    (:position position :strength strength :range range)
  (let* ((dir (v- position location))
         (dist (vlength dir)))
    (declare (dynamic-extent dir))
    (cond ((< dist range)
           (let* ((push (nvunit (nv- dir)))
                  (slide (nvc (vc velocity push) (nv- push))))
             (declare (dynamic-extent push slide))
             (nv+* force (nv- slide velocity) (/ dt))))
          (T
           (nv+* force dir (/ strength (* dist dist)))))))

(define-particle-force-field-shader :planet (field particle)
  "vec3 dir = field.position - particle.position;"
  "float dist = length(dir);"
  "if(dist < field.range){"
  "  // Same as sphere above."
  "  vec3 push = normalize(-dir);"
  "  vec3 slide = cross(cross(particle.velocity, push), -push);"
  "  return (slide-particle.velocity) / dt;"
  "}else{"
  "  return dir * field.strength / (dist * dist);"
  "}")

(define-particle-force-field-type :brake (force location velocity)
    (:strength strength)
  (!v* force velocity (- strength)))

(define-particle-force-field-shader :brake (field particle)
  "return particle.velocity * -field.strength;")

(define-particle-force-field-type :turbulence (force location velocity dt age)
    (:strength strength :range range)
  (let* ((offset (mod age 256.0))
         (varr (make-array 3 :element-type 'single-float))
         (f 4.0))
    (declare (dynamic-extent varr))
    (setf (aref varr 0) (mod (* range (vx location)) 100.0))
    (setf (aref varr 1) (mod (* range (vy location)) 100.0))
    (setf (aref varr 2) (mod (* range (vz location)) 100.0))
    (noise:with-sample s (noise:curl/3d varr f (noise:xxhash) #'noise:perlin/3d offset)
      (declare (ignore s))
      (incf (vx force) (* strength sdx))
      (incf (vy force) (* strength sdy))
      (incf (vz force) (* strength sdz)))))

(define-particle-force-field-shader :turbulence (field particle)
  )
