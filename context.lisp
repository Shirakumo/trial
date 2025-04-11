(in-package #:org.shirakumo.fraf.trial)

(defvar *context* NIL)

(defmacro with-context ((&optional (context '*context*) &key force reentrant) &body body)
  (let* ((cont (gensym "CONTEXT"))
         (thunk (gensym "THUNK"))
         (acquiring-body `(progn
                            (acquire-context ,cont :force ,force)
                            (unwind-protect
                                 (,thunk)
                              (release-context ,cont :reentrant ,reentrant)))))
    `(let ((,cont ,context))
       (flet ((,thunk ()
                ,@body))
         ,(if reentrant
              acquiring-body
              `(if (current-p ,cont)
                   (,thunk)
                   (let ((*context* ,cont))
                     ,acquiring-body)))))))

(defun launch-with-context (&optional main &rest initargs)
  (declare (ignore handler initargs))
  (error "No context implementation is present.~%~
          Please load a Trial backend."))

(defun make-context (&optional handler &rest initargs)
  (declare (ignore handler initargs))
  (error "No context implementation is present.~%~
          Please load a Trial backend."))

(defclass monitor ()
  ())

(defclass context ()
  ((current-thread :initform NIL :accessor current-thread)
   (waiting :initform 0 :accessor context-waiting)
   (lock :initform (bt:make-lock "Context lock") :reader context-lock)
   (wait-lock :initform (bt:make-lock "Context wait lock") :reader context-wait-lock)
   (resources :initform (make-hash-table :test 'eq) :accessor resources)
   (handler :initarg :handler :accessor handler)
   (shared-with :initarg :share-with :reader shared-with)
   (glsl-target-version :initarg :glsl-version :initform NIL :accessor glsl-target-version)
   (binding-point-allocator :initform (make-array 256 :element-type 'bit) :accessor binding-point-allocator)
   (framebuffer :reader framebuffer)
   (render-state :initform (make-instance 'render-state) :accessor render-state))
  (:default-initargs
   :title "Trial"
   :width 1280
   :height 720
   :glsl-version NIL
   :version '(3 3)
   :profile :core
   :double-buffering T
   :stereo-buffer NIL
   :vsync :off
   :share-with NIL
   :handler NIL))

(defmethod print-object ((context context) stream)
  (print-unreadable-object (context stream :type T :identity T)))

(defmethod reinitialize-instance :after ((context context) &key)
  (with-context (context)
    (destroy-context context)
    (create-context context)))

(defmethod initialize-instance :before ((context context) &key)
  (setf (slot-value context 'framebuffer) (make-instance 'backbuffer :context context)))

(defmethod initialize-instance :after ((context context) &key)
  (release-context context))

(defstruct (rgba-icon
            (:constructor rgba-icon (width height data))
            (:predicate NIL))
  (width 0 :type (unsigned-byte 16))
  (height 0 :type (unsigned-byte 16))
  (data #() :type (simple-array (unsigned-byte 8))))

(defgeneric create-child-context (context))
(defgeneric create-context (context))
(defgeneric destroy-context (context))
(defgeneric valid-p (context))
(defgeneric make-current (context))
(defgeneric current-p (context &optional thread))
(defgeneric done-current (context))
(defgeneric hide (context))
(defgeneric show (context &key &allow-other-keys))
(defgeneric visible-p (context))
(defgeneric resize (context width height))
(defgeneric quit (context))
(defgeneric swap-buffers (context))
(defgeneric show-cursor (context))
(defgeneric hide-cursor (context))
(defgeneric lock-cursor (context))
(defgeneric unlock-cursor (context))
(defgeneric cursor (context))
(defgeneric (setf cursor) (cursor context))
(defgeneric title (context))
(defgeneric (setf title) (value context))
(defgeneric vsync (context))
(defgeneric (setf vsync) (mode context))
(defgeneric current-monitor (context))
(defgeneric list-monitors (context))
(defgeneric list-video-modes (monitor))
(defgeneric find-monitor (name context))
(defgeneric clipboard (context))
(defgeneric (setf clipboard) (value context))
(defgeneric cursor-position (context))
(defgeneric (setf cursor-position) (pos context))
(defgeneric local-key-string (context scan-code))
(defgeneric (setf icon) (icon context))

(defgeneric width (context))
(defgeneric height (context))
(defgeneric profile (context))
(defgeneric version (context))

(defmethod finalize ((context context))
  (destroy-context context)
  (loop for resource being the hash-keys of (resources context)
        do (when (allocated-p resource)
             (v:warn :trial.context "Context-bound resource ~a still allocated, but the context was freed!"
                     resource)
             (setf (gl-name resource) NIL)))
  (call-next-method))

(defmethod create-child-context ((context context))
  (let ((restore (current-p context))
        (child (make-instance 'context :share-with context)))
    (create-context child)
    (when restore
      (done-current child)
      (make-current context))
    child))

(defmethod destroy-context :around ((context context))
  (when (valid-p context)
    (with-ignored-errors-on-release (:trial.context "Failed to destroy context.")
      (with-context (context :force T)
        (v:info :trial.context "Destroying context.")
        (hide context)
        (call-next-method)
        (setf *context* NIL))))
  context)

(defmethod create-context :around ((context context))
  (unless (valid-p context)
    (call-next-method)
    (v:info :trial.context "Recreated context successfully.")
    (make-current context)
    (context-note-debug-info context))
  context)

(defmethod current-p ((context context) &optional (thread (bt:current-thread)))
  (eql thread (current-thread context)))

(defun check-context-current (&optional (context *context*) (thread (bt:current-thread)))
  (when (or (null context) (not (current-p context thread)))
    (error "Context~%  ~a~%is not current in~%  ~a" context thread)))

(defmethod acquire-context ((context context) &key force)
  (let ((current (current-thread context))
        (this (bt:current-thread)))
    (when (or force (not (eql this current)))
      (cond ((and force current)
             (v:warn :trial.context "~a stealing ~a from ~a." this context current))
            ((eql this current))
            (current
             ;; FIXME: deadlocks somewhere
             (bt:with-lock-held ((context-wait-lock context))
               (incf (context-waiting context))
               (v:info :trial.context "~a waiting to acquire ~a (~a in queue)..." this context (context-waiting context)))
             (unwind-protect
                  (bt:acquire-lock (context-lock context))
               (bt:with-lock-held ((context-wait-lock context))
                 (decf (context-waiting context)))))
            (T
             (bt:acquire-lock (context-lock context))))
      (unless (valid-p context)
        (error "Attempting to acquire invalid context ~a" context))
      (v:info :trial.context "~a acquiring ~a." this context)
      (setf (current-thread context) this)
      (setf *context* context)
      #+trial-release
      (loop repeat 10
            do (handler-case (progn (make-current context) (return))
                 (error ()))
               (sleep 0.1))
      #-trial-release
      (make-current context))))

(defmethod release-context ((context context) &key reentrant)
  (let ((current (current-thread context))
        (this (bt:current-thread)))
    (when (and (eql this current)
               (or (not reentrant) (< 0 (context-waiting context))))
      (cond ((eql *context* context)
             (v:info :trial.context "~a releasing ~a." this context)
             (setf (current-thread context) NIL)
             (when (valid-p context)
               (done-current context))
             (bt:release-lock (context-lock context)))
            (T
             (v:warn :trial.context "~a attempted to release ~a even though ~a is active."
                     this context *context*))))))

(defmethod find-monitor (name (context context))
  (find name (list-monitors context) :key #'name :test #'string=))

(defmethod list-video-modes :around ((context context))
  (flet ((mode> (a b)
           (destructuring-bind (aw ah ar am) a
             (destructuring-bind (bw bh br bm) b
               (if (eq am bm)
                   (if (= aw bw)
                       (if (= ah bh)
                           (> ar br)
                           (> ah bh))
                       (> aw bw))
                   (string> am bm))))))
    (sort (delete-duplicates (call-next-method) :test #'equal) #'mode>)))

(defmethod list-video-modes ((context context))
  (list-video-modes (current-monitor context)))

(defmethod size ((context context))
  (vec (width context) (height context)))

(defmethod (setf render-state) :before (state (context context))
  (activate state))

(define-event resize () width height)
(define-event gain-focus ())
(define-event lose-focus ())
(define-event window-hidden ())
(define-event window-shown ())
(define-event window-close ())

(defmethod describe-object :after ((context context) stream)
  (context-info context :stream stream))

(defun context-info (context &key (stream *standard-output*) (show-extensions T))
  (format stream "OpenGL Version: ~a.~a ~a~%~
                    Sample buffers: ~a (~a sample~:p)~%~
                    Max texture size: ~a~%~
                    Max texture units: ~a ~a ~a ~a ~a ~a~%~
               ~@[~{Max compute groups: ~a ~a ~a~%~
                    Max work groups: ~a ~a ~a (~a)~%~}~]~
                    GL Vendor: ~a~%~
                    GL Renderer: ~a~%~
                    GL Version: ~a~%~
                    GL Shader Language: ~a~%~
                    ~@[GL Extensions: ~{~a~^ ~}~%~]"
          (gl-property :major-version)
          (gl-property :minor-version)
          (profile context)
          (gl-property :sample-buffers)
          (gl-property :samples)
          (gl-property :max-texture-size)
          (gl-property :max-vertex-texture-image-units)
          ;; Fuck you, GL, and your stupid legacy crap.
          (gl-property :max-texture-image-units)
          (gl-property :max-tess-control-texture-image-units)
          (gl-property :max-tess-evaluation-texture-image-units)
          (gl-property :max-geometry-texture-image-units)
          (gl-property :max-compute-texture-image-units)
          (ignore-errors
           (when-gl-extension :GL-ARB-COMPUTE-SHADER
             (append (coerce (gl-property :max-compute-work-group-count) 'list)
                     (coerce (gl-property :max-compute-work-group-size) 'list)
                     (list (gl-property :max-compute-work-group-invocations)))))
          (gl-property :vendor)
          (gl-property :renderer)
          (gl-property :version)
          (gl-property :shading-language-version)
          (when show-extensions
            (ignore-errors
             (loop for i from 0 below (gl:get* :num-extensions)
                   collect (gl:get-string-i :extensions i))))))

(defun context-note-debug-info (context)
  (v:debug :trial.context "Context information: ~%~@<  ~@;~a~;~:>"
           (let ((*print-right-margin* 1000)) ; SBCL fails otherwise. Huh?
             (with-output-to-string (out)
               (context-info context :stream out)))))

(defmethod glsl-target-version ((context context))
  (let ((slot (slot-value context 'glsl-target-version)))
    (or slot (format NIL "~{~d~d~}0" (version context)))))

(defmethod glsl-version-header ((context context))
  (format NIL "#version ~a~@[ ~a~]"
          (glsl-target-version context)
          (case (profile context)
            (:core "core")
            (:es "es"))))

(defmethod glsl-target-version ((default (eql T)))
  (if *context* (glsl-target-version *context*) "330"))

(defmethod (setf icon) ((path pathname) (context context))
  (multiple-value-bind (bits width height pixel-type pixel-format swizzle)
      (load-image path T)
    (let* ((swizzle (or swizzle (infer-swizzle-format pixel-format)))
           (data (convert-image-data bits width height :pixel-type-in pixel-type :pixel-format-in pixel-format :swizzle swizzle
                                                       :pixel-type-out :unsigned-byte :pixel-format-out :rgba)))
      (setf (icon context) (rgba-icon width height data)))))

(defmethod (setf cursor) ((path pathname) (context context))
  (multiple-value-bind (bits width height pixel-type pixel-format swizzle)
      (load-image path T)
    (let* ((swizzle (or swizzle (infer-swizzle-format pixel-format)))
           (data (convert-image-data bits width height :pixel-type-in pixel-type :pixel-format-in pixel-format :swizzle swizzle
                                                       :pixel-type-out :unsigned-byte :pixel-format-out :rgba)))
      (setf (cursor context) (rgba-icon width height data)))))

(defun dump-render-state (&key (stream *standard-output*)
                               (context *context*)
                               (state '(shader-program vertex-array framebuffer)))
  (with-context (context)
    (labels ((resolve (type bind)
               (loop with name = (gl:get* bind)
                     for object being the hash-keys of (resources context)
                     do (when (and (typep object type) (= (gl-name object) name))
                          (return object))))
             (try (type bind)
               (when (member type state)
                 (let ((thing (resolve type bind)))
                   (if thing
                       (describe thing stream)
                       (format stream "~&~%No ~s bound.~%" type))))))
      ;; TODO: also print depth mode, blend mode, etc.
      (try 'shader-program :current-program)
      (try 'vertex-array :vertex-array-binding)
      (try 'framebuffer :framebuffer-binding))))
