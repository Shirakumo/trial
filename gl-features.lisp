#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *gl-features* #(:blend
                        :clip-distance0
                        :clip-distance1
                        :color-logic-op
                        :cull-face
                        :debug-output
                        :debug-output-synchronous
                        :depth-clamp
                        :depth-test
                        :dither
                        :framebuffer-srgb
                        :line-smooth
                        :multisample
                        :polygon-offset-fill
                        :polygon-offset-line
                        :polygon-offset-point
                        :polygon-smooth
                        :primitive-restart
                        :primitive-restart-fixed-index
                        :rasterizer-discard
                        :sample-alpha-to-coverage
                        :sample-alpha-to-one
                        :sample-coverage
                        :sample-shading
                        :sample-mask
                        :scissor-test
                        :stencil-test
                        :texture-cube-map-seamless
                        :program-point-size))

(defvar *default-enabled-gl-features* #(:dither :multisample))

(defun make-feature-table (&optional parent)
  (let ((table (make-hash-table :test 'eq :size (length *gl-features*))))
    (if parent
        (loop for k being the hash-keys of parent
              for v being the hash-values of parent
              do (setf (gethash k table) v))
        (reset-features table))
    table))

(defun reset-features (&optional (table (feature-table)))
  (loop for k across *gl-features*
        do (if (find k *default-enabled-gl-features*)
               (setf (gethash k table) T)
               (setf (gethash k table) NIL))))

(defvar *feature-stack* (list (make-feature-table)))

(defun feature-table ()
  (first *feature-stack*))

(defun enable-feature (&rest features)
  (let ((table (feature-table)))
    (dolist (feature features)
      (unless (gethash feature table)
        (gl:enable feature)
        (setf (gethash feature table) T)))))

(define-compiler-macro enable-feature (&whole whole &environment env &rest features)
  (let ((constants) (variants) (table (gensym "TABLE")))
    (dolist (feature features)
      (if (constantp feature env)
          (push feature constants)
          (push feature variants)))
    (cond ((and (null constants) (null variants))
           ())
          ((null constants)
           whole)
          (T
           `(let ((,table (feature-table)))
              (enable-feature ,@variants)
              ,@(loop for constant in constants
                      for feature = `(load-time-value ,constant)
                      collect `(unless (gethash ,feature ,table)
                                 (gl:enable ,feature)
                                 (setf (gethash ,feature ,table) T))))))))

(defun disable-feature (&rest features)
  (let ((table (feature-table)))
    (dolist (feature features)
      (when (gethash feature table)
        (gl:disable feature)
        (setf (gethash feature table) NIL)))))

(define-compiler-macro disable-feature (&whole whole &environment env &rest features)
  (let ((constants) (variants) (table (gensym "TABLE")))
    (dolist (feature features)
      (if (constantp feature env)
          (push feature constants)
          (push feature variants)))
    (cond ((and (null constants) (null variants))
           ())
          ((null constants)
           whole)
          (T
           `(let ((,table (feature-table)))
              (disable-feature ,@variants)
              ,@(loop for constant in constants
                      for feature = `(load-time-value ,constant)
                      collect `(when (gethash ,feature ,table)
                                 (gl:disable ,feature)
                                 (setf (gethash ,feature ,table) NIL))))))))

(defun push-features (&optional (table (make-feature-table (feature-table))))
  (push table *feature-stack*))

(defun pop-features ()
  (let ((prev (pop *feature-stack*))
        (cur (feature-table)))
    (loop for k being the hash-keys of prev
          for v being the hash-values of prev
          do (cond ((and v (not (gethash k cur)))
                    (gl:disable k))
                   ((and (not v) (gethash k cur))
                    (gl:enable k))))))

(defmacro with-pushed-features (&body body)
  `(progn (push-features)
          (unwind-protect
               (progn ,@body)
            (pop-features))))

(defclass gl-context (context)
  ((glsl-target-version :initarg :glsl-version :initform NIL :accessor glsl-target-version))
  (:default-initargs
   :version '(3 3)
   :profile :core))

(defmethod create-context :after ((context glcontext))
  (cache-gl-extensions))

(defmethod glsl-target-version ((context gl-context))
  (let ((slot (slot-value context 'glsl-target-version)))
    (or slot (format NIL "~{~d~d~}0" (version context)))))

(defmethod glsl-version-header ((context gl-context))
  (format NIL "#version ~a~@[ ~a~]"
          (glsl-target-version context)
          (case (profile context)
            (:core "core")
            (:es "es"))))

(defmethod glsl-target-version ((default (eql T)))
  (if *context* (glsl-target-version *context*) "330"))

(defmacro with-render-settings (settings &body body)
  (let ((thunk (gensym "THUNK"))
        (settings (loop for setting in settings
                        collect (etypecase setting
                                  (keyword
                                   (ecase setting
                                     (:no-depth-writes `(write-to-depth-p T NIL))
                                     (:additive-blend `(blend-mode :additive NIL))
                                     (:front-cull `(cull-face :front :back))
                                     ;; Already defaults
                                     ;; FIXME: make nested with-render-* actually do the right thing
                                     (:depth-writes `(write-to-depth-p T T))
                                     (:source-blend `(blend-mode NIL NIL))
                                     (:back-cull `(cull-face :back :back))))
                                  (cons
                                   )))))
    `(flet ((,thunk () ,@body))
       ,@(loop for (func on off) in settings
               unless (eql on '_) collect `(setf (,func *context*) ,on))
       (multiple-value-prog1
           (,thunk)
         ,@(loop for (func on off) in settings
                 unless (eql off '_) collect `(setf (,func *context*) ,off))))))

(defmethod (setf write-to-depth) (mask (context gl-context))
  (gl:depth-mask mask))

(defmethod (setf depth-mode) (mode (context gl-context))
  (ecase mode
    ((NIL) (gl:depth-func :never))
    ((T) (gl:depth-func :always))
    (= (gl:depth-func :equal))
    (/= (gl:depth-func :notequal))
    (<= (gl:depth-func :lequal))
    (>= (gl:depth-func :lequal))
    (< (gl:depth-func :less))
    (> (gl:depth-func :greater))))

(defmethod (setf blend-mode) (mode (context gl-context))
  (ecase mode
    (:additive
     (gl:blend-func :src-alpha :one))
    ((NIL :default :source-over)
     (gl:blend-func-separate :src-alpha :one-minus-src-alpha :one :one-minus-src-alpha))))

(defmethod (setf culling-mode) (mode (context gl-context))
  (ecase mode
    ((NIL) (disable-feature :cull-face))
    ((T) (enable-feature :cull-face))
    ((:default :back-faces) (gl:cull-face :back))
    (:front-faces (gl:cull-face :front))))

(defmethod (setf stencil-mode) (mode (context gl-context))
  (ecase mode
    ((NIL)
     (gl:stencil-func :never 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :keep))
    ((T)
     (gl:stencil-func :always 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :keep))
    (=
     (gl:stencil-func :equal 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :keep))
    (/=
     (gl:stencil-func :notequal 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :keep))
    ((<= :default)
     (gl:stencil-func :lequal 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :keep))
    (>=
     (gl:stencil-func :gequal 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :keep))
    (<
     (gl:stencil-func :greater 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :keep))
    (>
     (gl:stencil-func :less 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :keep))
    (1+
     (gl:stencil-func :always 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :incr))
    (1-
     (gl:stencil-func :always 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :decr))
    (clear
     (gl:stencil-func :always 127 #xFFFFFF)
     (gl:stencil-op :keep :keep :replace))
    (T
     (gl:stencil-func :always mode #xFFFFFF)
     (gl:stencil-op :keep :keep :replace))))

(defmethod (setf clear-color) ((vec vec3) (context gl-context))
  (gl:clear-color (vx3 vec) (vy3 vec) (vz3 vec) 1.0))

(defmethod (setf clear-color) ((vec vec4) (context gl-context))
  (gl:clear-color (vx4 vec) (vy4 vec) (vz4 vec) (vw4 vec)))

(defmethod (setf clear-color) ((int integer) (context gl-context))
  (let ((r (ldb (byte 8 0) int))
        (g (ldb (byte 8 8) int))
        (b (ldb (byte 8 16) int))
        (a (ldb (byte 8 24) int)))
    (gl:clear-color (/ r 255.0) (/ g 255.0) (/ b 255.0) (/ a 255.0))))

;; https://www.khronos.org/registry/OpenGL/extensions/ATI/ATI_meminfo.txt
(defun %gl-gpu-room-ati ()
  (let* ((vbo-free-memory-ati (gl:get-integer #x87FB 4))
         (tex-free-memory-ati (gl:get-integer #x87FC 4))
         (buf-free-memory-ati (gl:get-integer #x87FD 4))
         (total (+ (aref vbo-free-memory-ati 0)
                   (aref tex-free-memory-ati 0)
                   (aref buf-free-memory-ati 0))))
    (values total total)))

;; http://developer.download.nvidia.com/opengl/specs/GL_NVX_gpu_memory_info.txt
(defun %gl-gpu-room-nvidia ()
  (let ((vidmem-total (gl:get-integer #x9047 1))
        (vidmem-free  (gl:get-integer #x9049 1)))
    (values vidmem-free
            vidmem-total)))

(defmethod gpu-room ((context gl-context))
  (macrolet ((jit (thing)
               `(ignore-errors
                 (return-from gpu-room
                   (multiple-value-prog1 ,thing
                     (compile 'gpu-room (lambda ()
                                          ,thing)))))))
    (jit (%gl-gpu-room-ati))
    (jit (%gl-gpu-room-nvidia))
    (jit (values 1 1))))

(defmethod max-texture-id ((context gl-context))
  (gl:get-integer :max-texture-image-units))

(define-global +gl-extensions+ ())

(defun cache-gl-extensions ()
  (let ((*package* (find-package "KEYWORD")))
    (setf +gl-extensions+
          (loop for i from 0 below (gl:get* :num-extensions)
                for name = (ignore-errors (gl:get-string-i :extensions i))
                when name
                collect (cffi:translate-name-from-foreign name *package*)))))

(defun gl-extension-p (extension)
  (find extension +gl-extensions+))

(defun list-gl-extensions ()
  (or +gl-extensions*
      (cache-gl-extensions)))

(defmacro when-gl-extension (extension &body body)
  (let ((list (enlist extension)))
    ;; TODO: Optimise this by caching the test after first runtime.
    `(when (and ,@(loop for extension in list
                        collect `(find ,extension +gl-extensions+)))
       ,@body)))

(defmacro gl-extension-case (&body cases)
  `(cond ,@(loop for (extensions . body) in cases
                 collect (case extensions
                           ((T otherwise)
                            `(T ,@body))
                           (T
                            `((and ,@(loop for extension in (enlist extensions)
                                           collect `(find ,extension +gl-extensions+)))
                              ,@body))))))

(defun gl-property (name)
  (handler-case (gl:get* name)
    (error (err) (declare (ignore err))
      :unavailable)))

(defun gl-vendor ()
  (let ((vendor (gl:get-string :vendor)))
    (cond ((search "Intel" vendor) :intel)
          ((search "NVIDIA" vendor) :nvidia)
          ((search "ATI" vendor) :amd)
          ((search "AMD" vendor) :amd)
          (T :unknown))))

(defmethod context-info ((context gl-context) &key (stream *standard-output*) (show-extensions T))
  (format stream "~&~%Running GL~a.~a ~a~%~
                    Sample buffers:     ~a (~a sample~:p)~%~
                    Max texture size:   ~a~%~
                    Max texture units:  ~a ~a ~a ~a ~a ~a~%~
               ~@[~{Max compute groups: ~a ~a ~a~%~
                    Max work groups:    ~a ~a ~a (~a)~%~}~]~
                    GL Vendor:          ~a~%~
                    GL Renderer:        ~a~%~
                    GL Version:         ~a~%~
                    GL Shader Language: ~a~%~
                    ~@[GL Extensions:      ~{~a~^ ~}~%~]"
          (gl-property :major-version)
          (gl-property :minor-version)
          (profile gl-context)
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
          (when-gl-extension :GL-ARB-COMPUTE-SHADER
                             (append (coerce (gl-property :max-compute-work-group-count) 'list)
                                     (coerce (gl-property :max-compute-work-group-size) 'list)
                                     (list (gl-property :max-compute-work-group-invocations))))
          (gl-property :vendor)
          (gl-property :renderer)
          (gl-property :version)
          (gl-property :shading-language-version)
          (when show-extensions (list-gl-extensions))))
