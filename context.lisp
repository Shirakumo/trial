#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *context* NIL)

(defmacro with-context ((context &key force reentrant) &body body)
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

(defclass context ()
  ((current-thread :initform NIL :accessor current-thread)
   (waiting :initform 0 :accessor context-waiting)
   (lock :initform (bt:make-lock "Context lock") :reader context-lock)
   (wait-lock :initform (bt:make-lock "Context wait lock") :reader context-wait-lock)
   (handler :initarg :handler :accessor handler)
   (shared-with :initarg :share-with :reader shared-with))
  (:default-initargs
   :title "Trial"
   :width 800
   :height 600
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

(defmethod initialize-instance :after ((context context) &key)
  (release-context context))

(defgeneric create-context (context))
(defgeneric destroy-context (context))
(defgeneric valid-p (context))
(defgeneric make-current (context))
(defgeneric current-p (context &optional thread))
(defgeneric done-current (context))
(defgeneric hide (context))
(defgeneric show (context &key &allow-other-keys))
(defgeneric resize (context width height))
(defgeneric quit (context))
(defgeneric swap-buffers (context))
(defgeneric show-cursor (context))
(defgeneric hide-cursor (context))
(defgeneric lock-cursor (context))
(defgeneric unlock-cursor (context))
(defgeneric title (context))
(defgeneric (setf title) (value context))
(defgeneric vsync (context))
(defgeneric (setf vsync) (mode context))

(defgeneric width (context))
(defgeneric height (context))
(defgeneric profile (context))
(defgeneric version (context))

(defmethod finalize ((context context))
  (destroy-context context)
  (call-next-method))

(defmethod destroy-context :around ((context context))
  (when (valid-p context)
    (with-context (context :force T)
      (v:info :trial.context "Destroying context.")
      (hide context)
      (call-next-method)
      (setf *context* NIL))))

(defmethod create-context :around ((context context))
  (unless (valid-p context)
    (call-next-method)
    (v:info :trial.context "Recreated context successfully.")
    (make-current context)
    (context-note-debug-info context)
    (cache-gl-extensions)
    (show context)))

(defmethod current-p ((context context) &optional (thread (bt:current-thread)))
  (eql thread (current-thread context)))

(defmethod acquire-context ((context context) &key force)
  (let ((current (current-thread context))
        (this (bt:current-thread)))
    (when (or force (not (eql this current)))
      (cond ((and force current)
             (v:warn :trial.context "~a stealing ~a from ~a." this context current))
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

(defmethod handle (event (global (eql T)))
  (when *context*
    (handle event (handler *context*))))

(defclass resize (event)
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)))

(defclass gain-focus (event)
  ())

(defclass lose-focus (event)
  ())

(defclass window-hidden (event)
  ())

(defclass window-shown (event)
  ())

(defmethod describe-object :after ((context context) stream)
  (context-info context stream))

(defun context-info (context stream)
  (format stream "~&~%Running GL~a.~a ~a~%~
                    Sample buffers:     ~a (~a sample~:p)~%~
                    Max texture size:   ~a~%~
                    Max texture units:  ~a ~a ~a ~a ~a ~a~%~
                    GL Vendor:          ~a~%~
                    GL Renderer:        ~a~%~
                    GL Version:         ~a~%~
                    GL Shader Language: ~a~%~
                    GL Extensions:      ~{~a~^ ~}~%"
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
          (gl-property :vendor)
          (gl-property :renderer)
          (gl-property :version)
          (gl-property :shading-language-version)
          (ignore-errors
           (loop for i from 0 below (gl:get* :num-extensions)
                 collect (gl:get-string-i :extensions i)))))

(defun context-note-debug-info (context)
  (v:debug :trial.context "Context information: ~a"
           (let ((*print-right-margin* 1000)) ; SBCL fails otherwise. Huh?
             (with-output-to-string (out)
               (context-info context out)))))
