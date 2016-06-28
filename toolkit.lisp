#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defparameter *time-units* #+sbcl 1000000
              #-sbcl internal-time-units-per-second)

(declaim (inline current-time))
(defun current-time ()
  #+sbcl (let ((usec (nth-value 1 (sb-ext:get-time-of-day))))
           (declare (type (unsigned-byte 31) usec))
           usec)
  #-sbcl (get-internal-real-time))

(defun enlist (item &rest items)
  (if (listp item) item (list* item items)))

(defun unlist (item)
  (if (listp item) (first item) item))

(defmacro with-primitives (primitive &body body)
  `(progn
     (gl:begin ,primitive)
     (unwind-protect
          (progn ,@body)
       (gl:end))))

(defmacro with-pushed-matrix (&body body)
  `(progn (gl:push-matrix)
          (unwind-protect
               (progn ,@body)
            (gl:pop-matrix))))

(defun matrix-4x4 (&rest elements)
  (let ((m (make-array '(4 4) :element-type 'single-float :initial-element 0.0f0)))
    (loop for x in elements
          for i from 0
          do (setf (row-major-aref m i) x))
    m))

(defmethod width ((object qobject))
  (q+:width object))

(defmethod height ((object qobject))
  (q+:height object))

(defun one-of (thing &rest options)
  (find thing options))

(define-compiler-macro one-of (thing &rest options)
  (let ((thing-var (gensym "THING")))
    `(let ((,thing-var ,thing))
       (or ,@(loop for option in options
                   collect `(eql ,thing-var ,option))))))

(defmethod make-painter (target)
  (q+:make-qpainter target))

(defmacro with-painter ((painter target) &body body)
  `(with-finalizing ((,painter (make-painter ,target)))
     ,@body))

(defun input-source (&optional (stream *query-io*))
  (with-output-to-string (out)
    (loop for in = (read-line stream NIL NIL)
          while (and in (string/= in "EOF"))
          do (write-string in out))))

(defun input-value (&optional (stream *query-io*))
  (multiple-value-list (eval (read stream))))

(defun input-literal (&optional (stream *query-io*))
  (read stream))

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

(defun make-thread (name func)
  (bt:make-thread func
                  :name name
                  :initial-bindings `((*standard-output* . ,*standard-output*)
                                      (*error-output* . ,*error-output*)
                                      (*trace-output* . ,*trace-output*)
                                      (*context* . NIL))))

(defmacro with-thread ((name) &body body)
  `(make-thread ,name (lambda () ,@body)))

(defun wait-for-thread-exit (thread &key (timeout 1) (interval 0.1))
  (loop for i from 0
        while (bt:thread-alive-p thread)
        do (sleep interval)
           (with-simple-restart (continue "Continue waiting.")
             (when (= i (/ timeout interval))
               (restart-case
                   (error "Thread ~s did not exit after ~a s." (bt:thread-name thread))
                 (abort ()
                   :report "Kill the thread and exit, risking corrupting the image."
                   (bt:destroy-thread thread)
                   (return)))))))

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

(defun check-texture-size (width height)
  (let ((max (gl:get* :max-texture-size)))
    (when (< max (max width height))
      (error "Hardware cannot support a texture of size ~ax~a, max is ~a."
             width height max))))

(defun check-texture-target (target)
  (ecase target ((:texture-2d :texture-cube-map))))

(defun check-texture-mag-filter (filter)
  (ecase filter ((:nearest :linear))))

(defun check-texture-min-filter (filter)
  (ecase filter ((:nearest :linear :nearest-mipmap-nearest :nearest-mipmap-linear
                           :linear-mipmap-nearest :linear-mipmap-linear))))

(defun check-shader-type (shader-type)
  (ecase shader-type ((:compute-shader :vertex-shader
                       :geometry-shader :fragment-shader
                       :tess-control-shader :tess-evaluation-shader))))

(defun check-vertex-buffer-type (buffer-type)
  (ecase buffer-type ((:array-buffer :atomic-counter-buffer
                       :copy-read-buffer :copy-write-buffer
                       :dispatch-indirect-buffer :draw-indirect-buffer
                       :element-array-buffer :pixel-pack-buffer
                       :pixel-unpack-buffer :query-buffer
                       :shader-storage-buffer :texture-buffer
                       :transform-feedback-buffer :uniform-buffer))))

(defun check-vertex-buffer-element-type (element-type)
  (ecase element-type ((:double :float :int :uint :char))))

(defun check-vertex-buffer-data-usage (data-usage)
  (ecase data-usage ((:stream-draw :stream-read
                      :stream-copy :static-draw
                      :static-read :static-copy
                      :dynamic-draw :dynamic-read
                      :dynamic-copy))))

(defun check-framebuffer-attachment (attachment)
  (ecase attachment ((:depth-stencil :depth NIL))))
