#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *native-array-element-types*
  (remove T (remove-duplicates
             (mapcar #'upgraded-array-element-type
                     (append '(fixnum short-float single-float double-float long-float)
                             (loop for i from 1 to 64 collect `(signed-byte ,i))
                             (loop for i from 1 to 64 collect `(unsigned-byte ,i))
                             '(base-char extended-char character)))
             :test #'equal)))

(defgeneric finalize (object))

(defmethod finalize :before (object)
  (v:debug :trial "Finalizing ~a" object))

(defmethod finalize (object)
  object)

(defun round-to (base number)
  (* base (ceiling number base)))

(defun gl-property (name)
  (handler-case (gl:get* name)
    (error (err) (declare (ignore err))
      :unavailable)))

(defun polar->cartesian (vec)
  (etypecase vec
    (vec2 (vec2 (* (vx2 vec) (cos (vy2 vec)))
                (* (vx2 vec) (sin (vy2 vec)))))
    (vec3 (vec3 (* (vx3 vec) (cos (vy3 vec)) (sin (vz3 vec)))
                (* (vx3 vec) (sin (vy3 vec)) (sin (vz3 vec)))
                (* (vx3 vec)                 (cos (vz3 vec)))))))

(defmethod apply-class-changes ((class standard-class)))

(defmethod apply-class-changes :before ((class standard-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super))))

(defmethod apply-class-changes :after ((class standard-class))
  (make-instances-obsolete class)
  (dolist (sub (c2mop:class-direct-subclasses class))
    (apply-class-changes sub)))

#+sbcl
(define-symbol-macro current-time-start
    (load-time-value (logand (sb-ext:get-time-of-day) (1- (expt 2 32)))))

(declaim (inline current-time))
(defun current-time ()
  (declare (optimize speed (safety 0)))
  #+sbcl (multiple-value-bind (s ms) (sb-ext:get-time-of-day)
           (let* ((s (logand s (1- (expt 2 62))))
                  (ms (logand ms (1- (expt 2 62)))))
             (declare (type (unsigned-byte 62) s ms))
             (+ (- s current-time-start)
                (* ms
                   (coerce 1/1000000 'double-float)))))
  #-sbcl (* (get-internal-real-time)
            (coerce (/ internal-time-units-per-second) 'double-float)))

(defmacro undefmethod (name &rest args)
  (flet ((lambda-keyword-p (symbol)
           (find symbol lambda-list-keywords)))
    (destructuring-bind (qualifiers args) (loop for thing = (pop args)
                                                until (listp thing)
                                                collect thing into qualifiers
                                                finally (return (list qualifiers thing)))
      `(remove-method
        #',name
        (find-method
         #',name
         ',qualifiers
         (mapcar #'find-class
                 ',(loop for arg in args
                         until (lambda-keyword-p arg)
                         collect (if (listp arg) (second arg) T))))))))

(defmacro define-unbound-reader (class method &body default)
  (destructuring-bind (method slot) (enlist method method)
    `(defmethod ,method ((,class ,class))
       (cond ((slot-boundp ,class ',slot)
              (slot-value ,class ',slot))
             (T
              ,@default)))))

(defun class-default-initargs (class-ish)
  (let ((class (etypecase class-ish
                 (symbol (find-class class-ish))
                 (standard-class class-ish))))
    (unless (c2mop:class-finalized-p class)
      (c2mop:finalize-inheritance class))
    (c2mop:class-default-initargs class)))

(defmethod copy-instance ((instance standard-object) &key deep)
  (let ((copy (allocate-instance (class-of instance))))
    (loop for slot in (c2mop:class-slots (class-of instance))
          for name = (c2mop:slot-definition-name slot)
          for value = (slot-value instance name)
          do (setf (slot-value copy name) (if deep (copy-instance value) value)))
    copy))

(defun executable-directory ()
  (pathname-utils:to-directory
   (or (first (uiop:command-line-arguments))
       *default-pathname-defaults*)))

(defun enlist (item &rest items)
  (if (listp item) item (list* item items)))

(defun unlist (item)
  (if (listp item) (first item) item))

(defun remf* (list &rest keys)
  (loop for (k v) on list by #'cddr
        for x = (member k keys)
        unless x collect k))

(defun one-of (thing &rest options)
  (find thing options))

(defun input-source (&optional (stream *query-io*))
  (with-output-to-string (out)
    (loop for in = (read-line stream NIL NIL)
          while (and in (string/= in "EOF"))
          do (write-string in out))))

(defun input-value (&optional (stream *query-io*))
  (multiple-value-list (eval (read stream))))

(defun input-literal (&optional (stream *query-io*))
  (read stream))

(defmacro define-accessor-wrapper-methods (name &body wrappers)
  `(progn ,@(loop with value = (gensym "VALUE")
                  for (type resolution) in wrappers
                  collect `(defmethod ,name ((,type ,type))
                             (,name ,resolution))
                  collect `(defmethod (setf ,name) (,value (,type ,type))
                             (setf (,name ,resolution) ,value)))))

(defmacro with-retry-restart ((name report &rest report-args) &body body)
  (let ((tag (gensym "RETRY-TAG"))
        (return (gensym "RETURN"))
        (stream (gensym "STREAM")))
    `(block ,return
       (tagbody
          ,tag (restart-case
                   (return-from ,return
                     (progn ,@body))
                 (,name ()
                   :report (lambda (,stream) (format ,stream ,report ,@report-args))
                   (go ,tag)))))))

(defmacro with-new-value-restart ((place &optional (input 'input-value))
                                  (name report &rest report-args) &body body)
  (let ((tag (gensym "RETRY-TAG"))
        (return (gensym "RETURN"))
        (stream (gensym "STREAM"))
        (value (gensym "VALUE")))
    `(block ,return
       (tagbody
          ,tag (restart-case
                   (return-from ,return
                     (progn ,@body))
                 (,name (,value)
                   :report (lambda (,stream) (format ,stream ,report ,@report-args))
                   :interactive ,input
                   (setf ,place ,value)
                   (go ,tag)))))))

(defmacro with-unwind-protection (cleanup &body body)
  `(unwind-protect
        (progn ,@body)
     ,cleanup))

(defmacro with-cleanup-on-failure (cleanup-form &body body)
  (let ((success (gensym "SUCCESS")))
    `(let ((,success NIL))
       (unwind-protect
            (multiple-value-prog1
                (progn
                  ,@body)
              (setf ,success T))
         (unless ,success
           ,cleanup-form)))))

(defmacro with-accessors* (accessors instance &body body)
  `(with-accessors ,(loop for accessor in accessors
                          collect (enlist accessor accessor))
       ,instance
     ,@body))

(defun acquire-lock-with-starvation-test (lock &key (warn-time 10) timeout)
  (assert (or (null timeout) (< warn-time timeout)))
  (flet ((do-warn () (v:warn :trial.core "Failed to acquire ~a for ~s seconds. Possible starvation!"
                             lock warn-time)))
    #+sbcl (or (sb-thread:grab-mutex lock :timeout warn-time)
               (do-warn)
               (if timeout
                   (sb-thread:grab-mutex lock :timeout (- timeout warn-time))
                   (sb-thread:grab-mutex lock)))
    #-sbcl (loop with start = (get-universal-time)
                 for time = (- (get-universal-time) start)
                 thereis (bt:acquire-lock lock NIL)
                 do (when (and warn-time (< warn-time time))
                      (setf warn-time NIL)
                      (do-warn))
                    (when (and timeout (< timeout time))
                      (return NIL))
                    (bt:thread-yield))))

(defvar *standalone* NIL)
(defun standalone-error-handler (err)
  (when *standalone*
    (v:error :trial err)
    (v:fatal :trial "Encountered unhandled error in ~a, bailing." (bt:current-thread))
    (if (and (uiop:getenv "TRIAL_DEBUG")
             (string/= "" (uiop:getenv "TRIAL_DEBUG")))
        (invoke-debugger err)
        (deploy:quit))))

(defun standalone-logging-handler ()
  (when *standalone*
    (let ((log (uiop:getenv "TRIAL_LOGFILE")))
      (unless (and log (string/= "" log))
        (setf log (merge-pathnames "trial.log" (or (uiop:argv0) (user-homedir-pathname)))))
      (v:define-pipe ()
        (v:file-faucet :file log)))))

(defun make-thread (name func)
  (bt:make-thread (lambda ()
                    (handler-bind ((error #'standalone-error-handler))
                      (funcall func)))
                  :name name
                  :initial-bindings `((*standard-output* . ,*standard-output*)
                                      (*error-output* . ,*error-output*)
                                      (*trace-output* . ,*trace-output*)
                                      (*standard-input* . ,*standard-input*)
                                      (*query-io* . ,*query-io*)
                                      (*debug-io* . ,*debug-io*))))

(defmacro with-thread ((name) &body body)
  `(make-thread ,name (lambda () ,@body)))

(defun wait-for-thread-exit (thread &key (timeout 1) (interval 0.1))
  (loop for i from 0
        while (bt:thread-alive-p thread)
        do (sleep interval)
           (when (= i (/ timeout interval))
             (restart-case
                 (error "Thread ~s did not exit after ~a s." (bt:thread-name thread) (* i interval))
               (continue ()
                 :report "Continue waiting.")
               (debug ()
                 :report "Try to interrupt the thread with a break."
                 (bt:interrupt-thread thread (lambda () (break))))
               (abort ()
                 :report "Kill the thread and exit, risking corrupting the image."
                 (bt:destroy-thread thread)
                 (return))))))

(defmacro with-thread-exit ((thread &key (timeout 1) (interval 0.1)) &body body)
  (let ((thread-g (gensym "THREAD")))
    `(let ((,thread-g ,thread))
       (when (and ,thread-g (bt:thread-alive-p ,thread-g))
         ,@body
         (wait-for-thread-exit ,thread-g :timeout ,timeout :interval ,interval)))))

(defmacro with-error-logging ((&optional (category :trial) (message "") &rest args) &body body)
  (let ((category-g (gensym "CATEGORY")))
    `(let ((,category-g ,category))
       (handler-bind ((error (lambda (err)
                               (v:severe ,category-g "~@[~@? ~]~a" ,message ,@args err)
                               (v:debug ,category-g err))))
         ,@body))))

(defmacro with-timing-report ((level category format &rest args) &body body)
  (let ((run (gensym "RUNTIME"))
        (real (gensym "REALTIME")))
    `(let ((,run (get-internal-run-time))
           (,real (get-internal-real-time)))
       (unwind-protect
            (progn ,@body)
         (v:log ,(intern (string level) :keyword) ,category ,format ,@args
                (/ (- (get-internal-run-time) ,run) INTERNAL-TIME-UNITS-PER-SECOND)
                (/ (- (get-internal-real-time) ,real) INTERNAL-TIME-UNITS-PER-SECOND))))))

(defun ensure-class (class-ish)
  (etypecase class-ish
    (symbol (find-class class-ish))
    (standard-class class-ish)
    (standard-object (class-of class-ish))))

(defmacro with-slots-bound ((instance class) &body body)
  (let ((slots (loop for slot in (c2mop:class-direct-slots
                                  (let ((class (ensure-class class)))
                                    (c2mop:finalize-inheritance class)
                                    class))
                     for name = (c2mop:slot-definition-name slot)
                     collect name)))
    `(with-slots ,slots ,instance
       (declare (ignorable ,@slots))
       ,@body)))

(defmacro with-all-slots-bound ((instance class) &body body)
  (let ((slots (loop for slot in (c2mop:class-slots
                                  (let ((class (ensure-class class)))
                                    (c2mop:finalize-inheritance class)
                                    class))
                     for name = (c2mop:slot-definition-name slot)
                     collect name)))
    `(with-slots ,slots ,instance
       (declare (ignorable ,@slots))
       ,@body)))

(defmacro define-constant-fold-function (name (arg) &body body)
  (let ((whole (gensym "WHOLE"))
        (env (gensym "ENV"))
        (thunk (intern (format NIL "%~a" (string name)) (symbol-package name))))
    `(progn
       (defun ,thunk (,arg)
         ,@body)
       (setf (fdefinition ',name) #',thunk)
       (define-compiler-macro ,name (&whole ,whole &environment ,env ,arg)
         (if (constantp ,arg ,env)
             `(load-time-value (,',thunk ,,arg))
             ,whole)))))

(defun minimize (sequence test &key (key #'identity))
  (etypecase sequence
    (vector (when (< 0 (length sequence))
              (loop with minimal = (aref sequence 0)
                    for i from 1 below (length sequence)
                    for current = (aref sequence i)
                    do (when (funcall test
                                      (funcall key current)
                                      (funcall key minimal))
                         (setf minimal current))
                    finally (return minimal))))
    (list (when sequence
            (loop with minimal = (car sequence)
                  for current in (rest sequence)
                  do (when (funcall test
                                    (funcall key current)
                                    (funcall key minimal))
                       (setf minimal current))
                  finally (return minimal))))))

(defun format-with-line-numbers (text)
  (with-output-to-string (out)
    (with-input-from-string (in text)
      (loop for i from 1
            for line = (read-line in NIL)
            while line
            do (format out "~3d ~a~%" i line)))))

(declaim (inline deg->rad rad->deg))
(defun deg->rad (deg)
  (* deg PI 1/180))

(defun rad->deg (rad)
  (* rad 180 (/ PI)))

(defparameter *c-chars* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_[]")

(defun symbol->c-name (symbol)
  (with-output-to-string (out)
    (loop for c across (symbol-name symbol)
          do (cond ((char= c #\-)
                    (write-char #\_ out))
                   ((find c *c-chars*)
                    (write-char (char-downcase c) out))
                   (T (write-char #\_ out))))))

;; https://www.khronos.org/registry/OpenGL/extensions/ATI/ATI_meminfo.txt
(defun gpu-room-ati ()
  (let* ((vbo-free-memory-ati (gl:get-integer #x87FB 4))
         (tex-free-memory-ati (gl:get-integer #x87FC 4))
         (buf-free-memory-ati (gl:get-integer #x87FD 4))
         (total (+ (aref vbo-free-memory-ati 0)
                   (aref tex-free-memory-ati 0)
                   (aref buf-free-memory-ati 0))))
    (values total total)))

;; http://developer.download.nvidia.com/opengl/specs/GL_NVX_gpu_memory_info.txt
(defun gpu-room-nvidia ()
  (let ((vidmem-total (gl:get-integer #x9047 1))
        (vidmem-free  (gl:get-integer #x9049 1)))
    (values vidmem-free
            vidmem-total)))

(defun gpu-room ()
  (macrolet ((jit (thing)
               `(ignore-errors
                 (return-from gpu-room
                   (multiple-value-prog1 ,thing
                     (compile 'gpu-room (lambda ()
                                          ,thing)))))))
    (jit (gpu-room-ati))
    (jit (gpu-room-nvidia))
    (jit (values 1 1))))

(defun cpu-room ()
  #+sbcl
  (values (round
           (/ (- (sb-ext:dynamic-space-size)
                 (sb-kernel:dynamic-usage))
              1024.0))
          (round
           (/ (sb-ext:dynamic-space-size) 1024.0)))
  #-sbcl (values 1 1))

(defun gl-vendor ()
  (let ((vendor (gl:get-string :vendor)))
    (cond ((search "Intel" vendor) :intel)
          ((search "NVIDIA" vendor) :nvidia)
          ((search "ATI" vendor) :amd)
          ((search "AMD" vendor) :amd)
          (T :unknown))))

(defun check-texture-size (width height)
  (let ((max (gl:get* :max-texture-size)))
    (when (< max (max width height))
      (error "Hardware cannot support a texture of size ~ax~a, max is ~a."
             width height max))))

(defmacro define-enum-check (name &body cases)
  (let* ((*print-case* (readtable-case *readtable*))
         (list (intern (format NIL "*~a-~a*" name '#:list)))
         (func (intern (Format NIL "~a-~a" '#:check name))))
    `(progn (defparameter ,list '(,@cases))
            (defun ,func (enum)
              (unless (find enum ,list)
                (error "~a is not a valid ~a. Needs to be one of the following:~%~a"
                       enum ',name ,list))))))

(defun texture-internal-format->pixel-format (format)
  (ecase format
    ((:stencil-index :stencil-index1 :stencil-index4 :stencil-index8 :stencil-index16)
     :stencil-index)
    ((:depth-component :depth-component16 :depth-component24 :depth-component32 :depth-component32f)
     :depth-component)
    ((:depth-stencil :depth32f-stencil8 :depth24-stencil8)
     :depth-stencil)
    ((:r8i :r8ui :r16i :r16ui :r32i :r32ui)
     :red-integer)
    ((:rgb8i :rgb8ui :rgb16i :rgb16ui :rgb32i :rgb32ui)
     :rg-integer)
    ((:rgba8i :rgba8ui :rgba16i :rgba16ui :rgba32i :rgba32ui)
     :rgba-integer)
    ((:red :r8 :r8-snorm :r16 :r16-snorm :r16f :r32f :compressed-red :compressed-red-rgtc1 :compressed-signed-red-rgtc1)
     :red)
    ((:rg :rg8 :rg8-snorm :rg16 :rg16-snorm :rg16f :rg32f :compressed-rg :compressed-rg-rgtc2 :compressed-signed-rg-rgtc2)
     :rg)
    ((:rgb :r3-g3-b2 :rgb4 :rgb5 :rgb8 :rgb8-snorm :rgb10 :rgb12 :rgb16-snorm :rgba2 :rgba4 :rgb16f :rgb32f
           :r11f-g11f-b10f :rgb9-e5 :srgb :srgb8
           :compressed-rgb :compressed-srgb :compressed-rgb-bptc-signed-float :compressed-rgb-bptc-unsigned-float)
     :rgb)
    ((:rgba :rgb5-a1 :rgba8 :rgba8-snorm :rgb10-a2 :rgb10-a2ui :rgba12 :rgba16 :srgb-alpha :srgb8-alpha8
      :rgba16f :rgba32f
      :compressed-rgba :compressed-srgb-alpha :compressed-rgba-bptc-unorm :compressed-srgb-alpha-bptc-unorm)
     :rgba)))

(defun pixel-format->pixel-type (format)
  (case format
    (:depth-stencil :unsigned-int-24-8)
    (:depth24-stencil8 :unsigned-int-24-8)
    (:depth32f-stencil8 :float-32-unsigned-int-24-8-rev)
    (T :unsigned-byte)))

(defun texture-internal-format->pixel-type (format)
  (case format
    (:depth-stencil :unsigned-int-24-8)
    (:depth24-stencil8 :unsigned-int-24-8)
    (:depth32f-stencil8 :unsigned-int-32-8)
    ((:r8i :rg8i :rgb8i :rgba8i) :byte)
    ((:r16i :rg16i :rgb16i :rgba16i) :short)
    ((:r32i :rg32i :rgb32i :rgba32i) :int)
    ((:r8ui :rg8ui :rgb8ui :rgba8ui) :unsigned-byte)
    ((:r16ui :rg16ui :rgb16ui :rgba16ui) :unsigned-short)
    ((:r32ui :rg32ui :rgb32ui :rgba32ui) :unsigned-int)
    ((:r16f :rg16f :rgb16f :rgba16f) :half-float)
    ((:r32f :rg32f :rgb32f :rgba32f) :float)
    (T :unsigned-byte)))

(define-enum-check texture-target
  :texture-1d :texture-2d :texture-3d
  :texture-1d-array :texture-2d-array
  :texture-cube-map :texture-cube-map-array
  :texture-2d-multisample :texture-2d-multisample-array)

(define-enum-check texture-mag-filter
  :nearest :linear)

(define-enum-check texture-min-filter
  :nearest :linear :nearest-mipmap-nearest :nearest-mipmap-linear
  :linear-mipmap-nearest :linear-mipmap-linear)

(define-enum-check texture-wrapping
  :repeat :mirrored-repeat :clamp-to-edge :clamp-to-border)

(define-enum-check texture-internal-format
  :red :r8 :r8-snorm :r8i :r8ui
  :r16 :r16-snorm :r16f :r16i :r16ui
  :r32f :r32i :r32ui
  :rg :rg8 :rg8-snorm :rg8i :rg8ui
  :rg16 :rg16-snorm :rg16f :rg16i :rg16ui
  :rg32f :rg32i :rg32ui
  :rgb :rgb8 :rgb8-snorm :rgb8i :rgb8ui
  :r3-g3-b2 :rgb4 :rgb5 :rgb9-e5 :rgb10 :r11f-g11f-b10f :rgb12
  :rgb16-snorm :rgb16f :rgb16i :rgb16ui
  :rgb32f :rgb32i :rgb32ui
  :rgba :rgba2 :rgba4 :rgb5-a1 :rgb10-a2 :rgb10-a2ui :rgba12
  :rgba8 :rgba8-snorm :rgba8i :rgba8ui
  :rgba16 :rgba16f :rgba16i :rgba16ui
  :rgba32f :rgba32i :rgba32ui
  :srgb :srgb-alpha :srgb8 :srgb8-alpha8
  :depth-component :depth-component16 :depth-component24 :depth-component32 :depth-component32f
  :stencil-index :stencil-index1 :stencil-index4 :stencil-index8 :stencil-index16
  :depth-stencil :depth24-stencil8 :depth32f-stencil8
  :compressed-red :compressed-red-rgtc1 :compressed-signed-red-rgtc1
  :compressed-rg :compressed-rg-rgtc2 :compressed-signed-rg-rgtc2
  :compressed-rgb :compressed-rgb-bptc-signed-float :compressed-rgb-bptc-unsigned-float
  :compressed-rgba :compressed-rgba-bptc-unorm
  :compressed-srgb :compressed-srgb-alpha :compressed-srgb-alpha-bptc-unorm)

(define-enum-check texture-pixel-format
  :red :rg :rgb :bgr :rgba :bgra
  :red-integer :rg-integer :rgb-integer
  :bgr-integer :rgba-integer :bgra-integer
  :stencil-index :depth-component :depth-stencil)

(define-enum-check texture-pixel-type
  :unsigned-byte :byte
  :unsigned-short :short
  :unsigned-int :int
  :half-float :float
  :unsigned-byte-3-3-2 :unsigned-byte-2-3-3-rev
  :unsigned-short-5-6-5 :unsigned-short-5-6-5-rev
  :unsigned-short-4-4-4-4 :unsigned-short-4-4-4-4-rev
  :unsigned-short-5-5-5-1 :unsigned-short-1-5-5-5-rev
  :unsigned-int-8-8-8-8 :unsigned-int-8-8-8-8-rev
  :unsigned-int-10-10-10-2 :unsigned-int-2-10-10-10-rev
  :unsigned-int-24-8 :float-32-unsigned-int-24-8-rev)

(define-enum-check shader-type
  :compute-shader :vertex-shader
  :geometry-shader :fragment-shader
  :tess-control-shader :tess-evaluation-shader)

(define-enum-check vertex-buffer-element-type
  :byte :unsigned-byte :short :unsigned-short :int :unsigned-int
  :half-float :float :double :fixed)

(define-enum-check buffer-object-type
  :array-buffer :atomic-counter-buffer
  :copy-read-buffer :copy-write-buffer
  :dispatch-indirect-buffer :draw-indirect-buffer
  :element-array-buffer :pixel-pack-buffer
  :pixel-unpack-buffer :query-buffer
  :shader-storage-buffer :texture-buffer
  :transform-feedback-buffer :uniform-buffer)

(define-enum-check buffer-object-data-usage
  :stream-draw :stream-read :stream-copy :static-draw
  :static-read :static-copy :dynamic-draw :dynamic-read
  :dynamic-copy)

(define-enum-check framebuffer-attachment
  :color-attachment0 :color-attachment1 :color-attachment2 :color-attachment3
  :color-attachment4 :color-attachment5 :color-attachment6 :color-attachment7
  :depth-attachment :stencil-attachment :depth-stencil-attachment)

(defun internal-format-components (format)
  (case format
    ((:red :r8 :r8-snorm :r8i :r8ui
      :r16 :r16-snorm :r16f :r16i :r16ui
      :r32f :r32i :r32ui
      :compressed-red :compressed-red-rgtc1
      :compressed-signed-red-rgtc1) 1)
    ((:rg :rg8 :rg8-snorm :rg8i :rg8ui :gr
      :rg16 :rg16-snorm :rg16f :rg16i :rg16ui
      :compressed-rg :compressed-rg-rgtc2 :compressed-signed-rg-rgtc2) 2)
    ((:rg32f :rg32i :rg32ui :bgr
      :rgb :rgb8 :rgb8-snorm :rgb8i :rgb8ui
      :r3-g3-b2 :rgb4 :rgb5 :rgb9-e5 :rgb10 :r11f-g11f-b10f :rgb12
      :rgb16-snorm :rgb16f :rgb16i :rgb16ui
      :rgb32f :rgb32i :rgb32ui :srgb8
      :compressed-rgb :compressed-rgb-bptc-signed-float
      :compressed-rgb-bptc-unsigned-float :compressed-srgb) 3)
    ((:rgba :bgra :rgba2 :rgba4 :rgb5-a1 :rgb10-a2 :rgb10-a2ui :rgba12
      :rgba8 :rgba8-snorm :rgba8i :rgba8ui
      :rgba16 :rgba16f :rgba16i :rgba16ui
      :rgba32f :rgba32i :rgba32ui :srgb8-alpha8
      :compressed-rgba :compressed-rgba-bptc-unorm
      :compressed-srgb-alpha :compressed-srgb-alpha-bptc-unorm) 4)
    ((:depth-component :depth-component16 :depth-component24 :depth-component32 :depth-component32f
      :stencil-index :stencil-index1 :stencil-index4 :stencil-index8 :stencil-index16) 1)
    ((:depth-stencil :depth24-stencil8 :depth32f-stencil8) 2)))

(defun internal-format-pixel-size (format)
  (case format
    ((:red :r8 :r8-snorm :r8i :r8ui) 8)
    ((:r16 :r16-snorm :r16f :r16i :r16ui) 16)
    ((:r32f :r32i :r32ui) 32)
    ((:rg :rg8 :rg8-snorm :rg8i :rg8ui) 16)
    ((:rg16 :rg16-snorm :rg16f :rg16i :rg16ui) 32)
    ((:rg32f :rg32i :rg32ui) 64)
    ((:rgb :rgb8 :rgb8-snorm :rgb8i :rgb8ui) 24)
    (:r3-g3-b2 8)
    (:rgb9-e5 32)
    (:r11f-g11f-b10f 32)
    (:rgb12 32)
    (:rgb4 12)
    (:rgb5 15)
    (:rgb10 30)
    ((:rgb16-snorm :rgb16f :rgb16i :rgb16ui) 48)
    ((:rgb32f :rgb32i :rgb32ui) 96)
    (:rgba2 8)
    (:rgba4 16)
    (:rgb5-a1 16)
    (:rgb10-a2 32)
    (:rgb10-a2ui 32)
    (:rgba12 48)
    ((:rgba :rgba8 :rgba8-snorm :rgba8i :rgba8ui) 32)
    ((:rgba16 :rgba16f :rgba16i :rgba16ui) 64)
    ((:rgba32f :rgba32i :rgba32ui) 128)
    (:srgb8 24) (:srgb8-alpha8 32)
    (:depth-component 8)
    (:depth-component16 16)
    (:depth-component24 24)
    ((:depth-component32 :depth-component32f) 32)
    (:stencil-index 8)
    (:stencil-index1 1)
    (:stencil-index4 4)
    (:stencil-index8 8)
    (:stencil-index16 16)
    (:depth-stencil 8)
    (:depth24-stencil8 32)
    (:depth32f-stencil8 40)))

(defun infer-internal-format (pixel-type pixel-format)
  (intern
   (format NIL "~a~a"
           (ecase pixel-format
             ((:r :red) :r)
             ((:rg :gr) :rg)
             ((:rgb :bgr) :rgb)
             ((:rgba :bgra) :rgba))
           (ecase pixel-type
             ((:byte :unsigned-byte) :8)
             ((:short :unsigned-short) :16)
             ((:int :unsigned-int) :32)
             ((:short-float) :16f)
             ((:float) :32f)))
   "KEYWORD"))

(defun infer-pixel-type (depth type)
  (ecase depth
    ( 8 (ecase type
          (:signed :byte)
          (:unsigned :unsigned-byte)))
    (16 (ecase type
          (:signed :short)
          (:unsigned :unsigned-short)
          (:float :half-float)))
    (32 (ecase type
          (:signed :int)
          (:unsigned :unsigned-int)
          (:float :float)))))

(defvar *gl-extensions* ())

(defun cache-gl-extensions ()
  (let ((*package* (find-package "KEYWORD")))
    (setf *gl-extensions*
          (loop for i from 0 below (gl:get* :num-extensions)
                for name = (ignore-errors (gl:get-string-i :extensions i))
                when name
                collect (cffi:translate-name-from-foreign name *package*)))))

(defmacro when-gl-extension (extension &body body)
  ;; TODO: Optimise this by caching the test after first runtime.
  `(when (find ,extension *gl-extensions*)
     ,@body))
