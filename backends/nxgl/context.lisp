(in-package #:org.shirakumo.fraf.trial.nxgl)

(defvar *monitor* (make-instance 'monitor))

(defclass context (trial:context)
  ((pointer :initform NIL :accessor pointer)
   (thread :initform NIL :accessor thread)
   (profile :initarg :profile :initform NIL :accessor profile)
   (version :initarg :version :initform NIL :accessor version)
   (vsync :initform NIL :accessor vsync)
   (initargs :initform () :accessor initargs)
   (mouse-pos :initform (vec 0 0) :accessor mouse-pos)
   (close-pending-p :initform NIL :accessor close-pending-p)))

(defmethod shared-initialize :after ((context context) slots
                                     &key (version NIL version-p)
                                          (profile NIL profile-p)
                                          (width NIL width-p)
                                          (height NIL height-p)
                                          (vsync NIL vsync-p)
                                          (robustness NIL robustness-p)
                                          (forward-compat NIL forward-compat-p)
                                          (debug-context NIL debug-context-p)
                                          double-buffering stereo-buffer title visible
                                     &allow-other-keys)
  (declare (ignore double-buffering stereo-buffer title visible))
  (flet (((setf g) (value name) (setf (getf (initargs context) name) value)))
    (macrolet ((maybe-set (var &optional (name (intern (string var) :keyword)))
                 `(when ,(let ((*print-case* (readtable-case *readtable*)))
                           (intern (format NIL "~a-~a" var 'p)))
                    (setf (g ,name) ,var))))
      (maybe-set width)
      (maybe-set height)
      (maybe-set vsync)
      (maybe-set profile)
      (maybe-set robustness)
      (maybe-set forward-compat)
      (maybe-set debug-context)
      (when version-p
        (setf (g :context-version-major) (first version))
        (setf (g :context-version-minor) (second version))))))

(defmethod initialize-instance ((context context) &key)
  (prog1 (call-next-method)
    (create-context context)))

(defmethod create-context ((context context))
  (flet ((arg (arg) (getf (initargs context) arg)))
    (v:info :trial.backend.nxgl "Creating context ~dx~d ~{~s~^ ~}" (arg :width) (arg :height) (initargs context))
    (let ((pointer (apply #'nxgl:create-context (arg :width) (arg :height) (initargs context))))
      (when (cffi:null-pointer-p pointer)
        (error "Failed to create context: ~a" nxgl:error))
      (setf (pointer context) pointer)
      (setf (vsync context) (arg :vsync))
      context)))

(defmethod destroy-context ((context context))
  (when (pointer context)
    (nxgl:destroy-context (pointer context))
    (setf (pointer context) NIL))
  context)

(defmethod valid-p ((context context))
  (not (null (pointer context))))

(defmethod make-current ((context context))
  (unless (nxgl:make-current (pointer context))
    (nxgl:check-error)))

(defmethod done-current ((context context))
  (unless (nxgl:done-current (pointer context))
    (nxgl:check-error)))

(defmethod hide ((context context))
  context)

(defmethod show ((context context) &key mode &allow-other-keys)
  (when mode
    (resize context (first mode) (second mode)))
  context)

(defmethod visible-p ((context context))
  T)

(defmethod resize ((context context) width height)
  (unless (nxgl:resize (pointer context) width height)
    (error "Failed to resize"))
  (handle (make-event 'resize :width width :height height) (handler context))
  context)

(defmethod quit ((context context))
  (setf (close-pending-p context) T)
  context)

(defmethod swap-buffers ((context context))
  (unless (nxgl:swap-buffers (pointer context))
    (nxgl:check-error))
  context)

(defmethod show-cursor ((context context))
  context)

(defmethod hide-cursor ((context context))
  context)

(defmethod lock-cursor ((context context))
  context)

(defmethod unlock-cursor ((context context))
  context)

(defmethod cursor ((context context))
  NIL)

(defmethod (setf cursor) (cursor (context context))
  cursor)

(defmethod title ((context context))
  "Trial")

(defmethod (setf title) (value (context context))
  value)

(defmethod (setf vsync) :before (mode (context context))
  (unless (nxgl:swap-interval (pointer context) (ecase mode ((NIL :off) 0) ((:on T) 1) (:adaptive -1)))
    (nxgl:check-error)))

(defmethod current-monitor ((context context))
  *monitor*)

(defmethod list-monitors ((context context))
  (list *monitor*))

(defmethod list-video-modes ((monitor monitor))
  (list (list 1920 1080 60 "")
        (list 1280 720 60 "")))

(defmethod find-monitor (name (context context))
  *monitor*)

(defmethod name ((monitor monitor))
  "")

(defmethod clipboard ((context context))
  "")

(defmethod (setf clipboard) (value (context context))
  value)

(defmethod cursor-position ((context context))
  (vec 0 0))

(defmethod (setf cursor-position) (pos (context context))
  pos)

(defmethod local-key-string ((context context) scan-code)
  (etypecase scan-code
    (null NIL)
    (integer (local-key-string context (scan-code->keyword scan-code)))
    (keyword (keyword->string scan-code))))

(defmethod (setf icon) (icon (context context))
  icon)

(defmethod width ((context context))
  (nxgl:width (pointer context)))

(defmethod height ((context context))
  (nxgl:height (pointer context)))

(defmethod poll-input ((context context))
  (cffi:with-foreign-objects ((count :size)
                              (events '(:struct nxgl:event) 32))
    (setf (cffi:mem-ref count :size) 32)
    (nxgl:poll (pointer context) count events)
    (loop for i from 0 below (cffi:mem-ref count :size)
          for event = (cffi:mem-aptr events '(:struct nxgl:event) i)
          do (process-event context event)))
  (when (close-pending-p context)
    (exit-render-loop)))

(cffi:define-foreign-library %gl::opengl
  (t "opengl.nso"))

(deploy:define-hook (:boot nxgl) ()
  (deploy:status 1 "Initialising NXGL")
  (cffi:load-foreign-library 'nxgl:nxgl)
  (unless (nxgl:init)
    (error "Failed to initialize NXGL!"))
  (setf %gl:*gl-get-proc-address* #'nxgl:proc-address)
  (%gl::reset-gl-pointers)
  (setf sb-sys::*software-version* (nxgl:software-version))
  (setf sb-sys::*machine-instance* (nxgl:machine-instance))
  (setf sb-sys::*machine-version* "ARM Cortex-A57"))

(deploy:define-hook (:quit nxgl) ()
  (ignore-errors (v:flush))
  (ignore-errors (v:remove-global-controller))
  (ignore-errors (nxgl:shutdown))
  (ignore-errors (nxgl:quit)))

(defun trial:launch-with-context (&optional main &rest initargs)
  (let ((main (apply #'make-instance main initargs)))
    (start main)
    (trial:rename-thread "input-loop")
    (v:debug :trial.backend.nxgl "Entering input loop")
    (unwind-protect
         (let* ((context (trial:context main))
                (*context* context))
           (loop until (close-pending-p context)
                 do (poll-input main)
                    (poll-input context)
                    (sleep 0.0001)))
      (v:debug :trial.backend.nxgl "Cleaning up")
      (finalize main))))

(defun trial:make-context (&optional handler &rest initargs)
  (apply #'make-instance 'context :handler handler initargs))

(defun process-event (context event)
  (let ((rack (cffi:foreign-slot-pointer event '(:struct nxgl:event) 'nxgl::a)))
    (flet ((fire (event-type &rest args)
             (handle (apply #'make-event event-type args) (handler context))))
      (macrolet ((event-case (&body cases)
                   `(case (nxgl:event-type event)
                      ,@(loop for (type args . body) in cases
                              collect `(,type
                                        (let ,(loop for name in args
                                                    for i from 0
                                                    collect `(,name (cffi:mem-aref rack :int ,i)))
                                          ,@body))))))
        (event-case
          (:focus-gain ()
           (fire 'gain-focus))

          (:focus-lose ()
           (fire 'lose-focus))

          (:resize (width height)
           (fire 'resize :width width :height height))

          (:quit ()
           (fire 'window-close))

          (:mouse-move (x y)
           (let ((current (vec x y)))
              (fire 'mouse-move :old-pos (mouse-pos context)
                                :pos current)
              (setf (mouse-pos context) current)))

          (:mouse-press (button)
           (fire 'mouse-press :pos (mouse-pos context) :button (button->keyword button)))

          (:mouse-release (button)
           (fire 'mouse-release :pos (mouse-pos context) :button (button->keyword button)))

          (:mouse-wheel (delta)
           (fire 'mouse-scroll :pos (mouse-pos context) :delta delta))

          (:key-press (code modifiers)
           (fire 'key-press :key (scan-code->keyword code)
                            :modifiers (cffi:foreign-bitfield-symbols 'nxgl:modifier modifiers)))

          (:key-release (code modifiers)
           (let ((modifiers (cffi:foreign-bitfield-symbols 'nxgl:modifier modifiers))
                 (key (scan-code->keyword code)))
             (fire 'key-release :key key :modifiers modifiers)
             (let ((string (keyword->string key :shift (member :shift modifiers))))
               (when string
                 (fire 'text-entered :text string))))))))))

(defmethod org.shirakumo.depot:commit :after ((depot org.shirakumo.depot.zip::zip-file-archive) &key)
  (nxgl:commit-save))

(defun show-keyboard (&key default (type :text) prompt (max-length 128))
  (cffi:with-foreign-objects ((str :char max-length))
    (when default
      (cffi:lisp-string-to-foreign default str max-length :encoding :utf-8))
    (cond ((< 0 (nxgl:show-keyboard str max-length type (if prompt prompt (cffi:null-pointer))))
           (cffi:foreign-string-to-lisp str :max-chars max-length))
          ((eql :input-cancelled nxgl:error)
           NIL)
          (T
           (nxgl:check-error)))))

(defun performance-mode ()
  (nxgl:get-performance-mode))

(defun (setf performance-mode) (mode)
  (when (= 0 (nxgl:set-performance-mode mode))
    (error "Failed to set performance mode."))
  mode)

(defmacro with-performance-mode (mode &body body)
  (let ((previous (gensym "PREVIOUS")))
    `(let ((,previous (performance-mode)))
       (setf (performance-mode) ,mode)
       (unwind-protect
            (progn ,@body)
         (setf (performance-mode) ,previous)))))
