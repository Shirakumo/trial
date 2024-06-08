(in-package :trial)

(defun internal-format-unit (internal-format)
  (case (internal-format-components internal-format)
    (1 "float")
    (2 "vec2")
    (3 "vec3")
    (4 "vec4")))

(defun compile-pixel-shader-preamble (ports)
  (with-output-to-string (out)
    (write-line "ivec2 uv;" out)
    (loop with i = -1
          for (kind name type) in ports
          do (case kind
               (:uniform
                (format out "uniform ~(~a~) ~a;~%"
                        type (symbol->c-name name)))
               (:input
                (format out "uniform sampler2D ~a_tex;~%"
                        (symbol->c-name name)))
               (:output
                (format out "layout (location = ~d) out ~(~a~) ~a;~%"
                        (incf i)
                        (internal-format-unit type)
                        (symbol->c-name name)))))))

(defun compile-pixel-shader-main (ports)
  (with-output-to-string (out)
    (write-line "void main(){" out)
    (write-line "  uv = ivec2(gl_FragCoord.xy);" out)
    (loop for (kind name type) in ports
          do (case kind
               (:input
                (format out "  ~a ~a = texelFetch(~:*~a_tex, uv, 0).~[~;x~;xy~;xyz~;xyzw~];~%" 
                        (internal-format-unit type) (symbol->c-name name)
                        (internal-format-components type)))))
    (format out "  stage(~{~a~^, ~});~%"
            (loop for (kind name type) in ports
                  when (find kind '(:input :output))
                  collect (symbol->c-name name)))
    (write-line "}" out)))

(defmacro define-pixel-stage (name ports &body body)
  (destructuring-bind (name &key (direct-superclasses '(post-effect-pass)) first iterate) (enlist name)
    (form-fiddle:with-body-options (body options) body
      `(progn
         (define-shader-pass ,name ,direct-superclasses
           ,(loop with attachments = '(:color-attachment0 :color-attachment1 :color-attachment2 :color-attachment3
                                       :color-attachment4 :color-attachment5 :color-attachment6 :color-attachment7)
                  for (kind name type . slot-args) in ports
                  collect `(,name
                            ,@(ecase kind
                                (:uniform `(:port-type uniform-port))
                                (:input `(:port-type ,(if first 'static-input 'input) :texspec (:internal-format ,type)))
                                (:output `(:port-type output :texspec (:internal-format ,type)
                                           :attachment ,(pop attachments)))
                                (:slot (list name type)))
                            ,@slot-args))
           (:inhibit-shaders (shader-entity :fragment-shader))
           ,@(loop for (k v) on options by #'cddr collect (cons k v)))

         (define-class-shader (,name :fragment-shader)
           ,(compile-pixel-shader-preamble ports)
           ,@body
           ,(compile-pixel-shader-main ports))

         ,@(when iterate
             (destructuring-bind (times (a b)) iterate
               `((defmethod render ((pass ,name) (program shader-program))
                   (let ((in (gl-name (slot-value pass ',a)))
                         (out (gl-name (slot-value pass ',b)))
                         (unit-id (unit-id (port pass ',a)))
                         (attachment (attachment (port pass ',b)))
                         (times ,times))
                     (gl:active-texture unit-id)
                     (dotimes (i times)
                       (call-next-method)
                       (when (< i (1- times))
                         (rotatef in out)
                         (%gl:bind-texture :texture-2d in)
                         (%gl:framebuffer-texture :framebuffer attachment out 0)))
                     (setf (gl-name (slot-value pass ',a)) in)
                     (setf (gl-name (slot-value pass ',b)) out))))))))))

(defclass pixel-pipeline (pipeline)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (dirty-p :initform NIL :accessor dirty-p)))

(defmethod shared-initialize :after ((pipeline pixel-pipeline) slots &key)
  (pack-pipeline pipeline (width pipeline) (height pipeline)))

(defgeneric setup-pipeline (pixel-pipeline))

(defmethod pack-pipeline ((pipeline pixel-pipeline) width height)
  (setup-pipeline pipeline)
  (pack-pipeline pipeline width height)
  (loop for pass across (passes pipeline)
        do (setf (clear-bits (framebuffer pass)) 0)))

(defmacro define-pixel-pipeline (pipeline-name slots &body body)
  (form-fiddle:with-body-options (body others pool loopback accessors (struct (mksym *package* pipeline-name '-uniforms))) body
    (let ((names (loop for i from 1 for pass in body
                       collect (mksym *package*  pipeline-name '- i))))
      (loop for (k) on others by #'cddr
            do (error "Unknown option ~s" k))
      `(progn
         (define-gl-struct ,struct
           ,@(loop for (kind name type . args) in slots
                   when (eql :uniform kind)
                   collect `(,name ,type ,@args)))

         (define-asset (,pool ,struct) uniform-block ',struct
           :binding NIL)
         
         ,@(loop for pass in body
                 for name in names
                 for ports = (loop while (consp (car pass))
                                   collect (pop pass))
                 collect (form-fiddle:with-body-options (body options) pass
                           `(define-pixel-stage (,name :first ,(eql name (first names)) ,@options)
                                ,ports
                              :buffers ((,pool ,struct))
                              (gl-source (asset ',pool ',struct))
                              ,@body)))

         (defclass ,pipeline-name (pixel-pipeline)
           ,(loop for (kind . args) in slots
                  when (eql :slot kind)
                  collect args))

         ,@(loop for name in accessors
                 for i = (or (loop for pass in (reverse body)
                                   for i downfrom (1- (length body))
                                   do (when (loop for slot = (pop pass)
                                                  while (consp slot)
                                                  thereis (and (eql :output (first slot))
                                                               (eql name (second slot))))
                                        (return i)))
                             (loop for pass in (reverse body)
                                   for i from 0 below (length body)
                                   do (when (loop for slot = (pop pass)
                                                  while (consp slot)
                                                  thereis (and (eql :input (first slot))
                                                               (eql name (second slot))))
                                        (return i))))
                 collect `(defmethod ,name ((pipeline ,pipeline-name))
                            (slot-value (aref (passes pipeline) ,i) ',name)))

         (defmethod setup-pipeline ((pipeline ,pipeline-name))
           (let ,(loop for name in names
                       collect `(,name (make-instance ',name)))
             ,@(flet ((find-output-pass (slot-name pass-name)
                        (loop for i downfrom (position pass-name names) to 0
                              for pass = (nth i body)
                              do (when (loop for slot = (pop pass)
                                             while (consp slot)
                                             thereis (and (or (eql :output (first slot))
                                                              (eql i 0))
                                                          (eql slot-name (second slot))))
                                   (return (nth i names))))))
                 (loop for name in (rest names)
                       for pass in (rest body)
                       append (loop for slot = (pop pass)
                                    while (consp slot)
                                    when (eql :input (first slot))
                                    collect (let ((slot-name (second slot)))
                                              `(connect
                                                (port ,(find-output-pass slot-name name) ',slot-name)
                                                (port ,name ',slot-name)
                                                pipeline)))))))

         (defmethod render :before ((pipeline ,pipeline-name) (target null))
           (when (dirty-p pipeline)
             (update-buffer-data (// ',pool ',struct) T)
             (setf (dirty-p pipeline) NIL))
           (let ((in (aref (passes pipeline) 0))
                 (out (aref (passes pipeline) (1- (length (passes pipeline))))))
             (%gl:bind-framebuffer :framebuffer (gl-name (framebuffer out)))
             ,@(loop for (out in) in loopback
                     collect `(rotatef (gl-name (slot-value in ',in))
                                       (gl-name (slot-value out ',out)))
                     collect `(%gl:framebuffer-texture
                               :framebuffer (attachment (port out ',out))
                               (gl-name (slot-value out ',out)) 0))))

         ,@(loop for (kind name type . args) in slots
                 when (eql :uniform kind)
                 collect `(defmethod ,name ((pipeline ,pipeline-name))
                            (with-buffer-tx (,pipeline-name (// ',pool ',struct) :update NIL)
                              (,name ,pipeline-name)))
                 collect `(defmethod (setf ,name) (value (pipeline ,pipeline-name))
                            (setf (dirty-p pipeline) T)
                            (with-buffer-tx (,pipeline-name (// ',pool ',struct) :update NIL)
                              (setf (,name ,pipeline-name) value))))))))
