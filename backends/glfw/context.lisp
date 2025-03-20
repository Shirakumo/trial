(in-package #:org.shirakumo.fraf.trial.glfw)

(defclass monitor (trial:monitor glfw:monitor)
  ((name :reader name)))

(defmethod shared-initialize :after ((monitor monitor) slots &key)
  (unless (slot-boundp monitor 'name)
    (setf (slot-value monitor 'name) (format NIL "~d: ~a" (position monitor (glfw:list-monitors))
                                             (handler-case (glfw:name monitor)
                                               (error () "<UNKNOWN>"))))))

(defstruct last-click
  (button NIL :type symbol)
  (time 0 :type (unsigned-byte 64))
  (pos (vec 0 0) :type vec2))

(defclass context (trial:context glfw:window)
  ((profile :initarg :profile :initform NIL :reader profile)
   (cursor-visible :initform T :accessor cursor-visible)
   (mouse-pos :initform (vec 0 0) :accessor mouse-pos)
   (last-click :initform (make-last-click) :accessor last-click)
   (visible-p :initform T :accessor visible-p)
   ;; We track w/h separately here as Trial cares about the framebuffer size, not the window size.
   (fb-width :initform 0 :accessor width)
   (fb-height :initform 0 :accessor height)
   (initargs :initform () :accessor initargs)
   (event-queue :initform (make-event-queue) :accessor event-queue))
  (:default-initargs
   :resizable T
   :visible T
   :decorated T
   :forward-compat T
   :debug-context NIL
   :x11-class-name "trial"
   :x11-instance-name trial:+app-system+))

(defmethod initialize-instance ((context context) &key)
  (v:info :trial.backend.glfw "Creating context ~a" context)
  (handler-case
      (progn (call-next-method)
             (make-current context)
             (trial::context-note-debug-info context))
    (glfw:glfw-error (e)
      (error 'trial:context-creation-error :message (glfw:message e) :context context))))

(defmethod shared-initialize :around ((context context) slots
                                      &rest initargs
                                      &key (version NIL version-p)
                                           (profile NIL profile-p)
                                           (double-buffering NIL double-buffering-p)
                                           (vsync NIL vsync-p)
                                           fullscreen
                                      ;; Extra options
                                           (robustness NIL robustness-p)
                                           (forward-compat NIL forward-compat-p)
                                           (debug-context NIL debug-context-p)
                                           (api (if (eql profile :es) :opengl-es-api :opengl-api)))
  (flet (((setf g) (value name)
           (setf (getf initargs name) value)))
    (macrolet ((maybe-set (var &optional (name (intern (string var) :keyword)))
                 `(when ,(let ((*print-case* (readtable-case *readtable*)))
                           (intern (format NIL "~a-~a" var 'p)))
                    (setf (g ,name) ,var))))
      (maybe-set double-buffering :doublebuffer)
      (maybe-set robustness :context-robustness)
      (maybe-set forward-compat :opengl-forward-compat)
      (maybe-set debug-context :context-debug)
      (setf (g :client-api) api)
      (setf (g :vsync) vsync)
      (setf (g :x11-class-name) "trial")
      (setf (g :x11-instance-name) (or +app-system+ "trial"))
      (setf (g :wayland-app-id) (or +app-system+ "trial"))
      (when version-p
        (setf (g :context-version-major) (first version))
        (setf (g :context-version-minor) (second version)))
      (when profile-p
        (setf (g :opengl-profile) (ecase profile
                                    ((NIL :es) :opengl-any-profile)
                                    (:core :opengl-core-profile)
                                    (:compatibility :opengl-compat-profile)))))
    ;; Merge initargs into retained
    (if (slot-boundp context 'initargs)
        (loop for (k v) on initargs by #'cddr
              do (setf (getf (initargs context) k) v))
        (setf (initargs context) initargs))
    ;; Some extra handling for fullscreening
    (etypecase fullscreen
      ((eql T) (setf initargs (list* :monitor (glfw:primary-monitor) initargs)))
      (string (setf initargs (list* :monitor (ensure-monitor fullscreen context) initargs)))
      ((eql NIL)))
    ;; Do the actual initialization
    (apply #'call-next-method context slots :allow-other-keys T initargs)
    (with-cleanup-on-failure (destroy-context context)
      ;; Some extra handling for internal options
      (gl:clear :color-buffer)
      (glfw:swap-buffers context)
      (refresh-window-size context)
      (when vsync-p (setf (vsync context) vsync)))))

(defmethod create-context ((context context))
  (handler-case
      (apply #'reinitialize-instance context :initialize-context T (initargs context))
    (glfw:glfw-error (e)
      (error 'trial:context-creation-error :message (glfw:message e) :context context))))

(defmethod destroy-context ((context context))
  (glfw:destroy context))

(defmethod valid-p ((context context))
  (not (null (glfw:pointer context))))

(defmethod make-current ((context context))
  (glfw:make-current context))

(defmethod done-current ((context context))
  (glfw:make-current NIL))

(defmethod hide ((context context))
  (glfw:hide context)
  (setf (visible-p context) NIL))

(defun ensure-monitor (monitor context)
  (etypecase monitor
    ((eql T) (glfw:primary-monitor))
    (null (current-monitor context))
    (monitor monitor)
    (string (find-monitor monitor context))))

(defmethod show ((context context) &key (fullscreen NIL f-p) mode)
  (glfw:show context)
  (setf (visible-p context) T)
  (cond (f-p
         (destructuring-bind (w h &optional r monitor)
             (etypecase mode
               (monitor (current-video-mode mode))
               (string (current-video-mode (find-monitor mode context)))
               (null (current-video-mode (current-monitor context)))
               ((eql T) (current-video-mode (glfw:primary-monitor)))
               (cons mode))
           (let ((monitor (ensure-monitor monitor context)))
             (when (eql T w)
               (destructuring-bind (cw ch cr cm) (current-video-mode monitor)
                 (setf w cw h ch r cr)))
             (setf (glfw:monitor context) (list (when fullscreen monitor) :width w :height h :refresh-rate r))))
         (unless fullscreen
           ;; Can fail on Wayland
           (ignore-errors (glfw:center context))))
        (mode
         (resize context (first mode) (second mode)))))

(defmethod resize ((context context) width height)
  (v:info :trial.backend.glfw "Resizing window to ~ax~a" width height)
  (setf (glfw:size context) (list width height))
  ;; Can fail on Wayland
  (ignore-errors (glfw:center context)))

(defmethod quit ((context context))
  (setf (glfw:should-close-p context) T))

(defmethod swap-buffers ((context context))
  (glfw:swap-buffers context))

(defmethod show-cursor ((context context))
  (setf (glfw:input-mode :cursor context) :cursor-normal)
  (setf (cursor-visible context) T))

(defmethod hide-cursor ((context context))
  (setf (glfw:input-mode :cursor context) :cursor-hidden)
  (setf (cursor-visible context) NIL))

(defmethod lock-cursor ((context context))
  (setf (glfw:input-mode :cursor context) :cursor-disabled))

(defmethod unlock-cursor ((context context))
  (if (cursor-visible context)
      (show-cursor context)
      (hide-cursor context)))

(defmethod cursor ((context context))
  (glfw:cursor context))

(defmethod (setf cursor) (cursor (context context))
  (ignore-errors
   (setf (glfw:cursor context) (case cursor
                                 ((:cursor :pointer NIL) :arrow)
                                 (:text :ibeam)
                                 (:hand :pointing-hand)
                                 (:resize :resize-all)
                                 (:disallowed :not-allowed)
                                 (:ew-resize :resize-ew)
                                 (:ns-resize :resize-ns)
                                 (:nwse-resize :resize-nwse)
                                 (:nesw-resize :resize-nesw)
                                 (:all-resize :resize-all)
                                 (T cursor)))
   cursor))

(defmethod (setf icon) ((icon rgba-icon) (context context))
  (setf (glfw:icon context) (list (list (rgba-icon-data icon)
                                        (rgba-icon-width icon)
                                        (rgba-icon-height icon)))))

(defmethod (setf icon) ((none null) (context context))
  (setf (glfw:icon context) none))

(defmethod title ((context context))
  (glfw:title context))

(defmethod (setf title) (value (context context))
  (setf (glfw:title context) value))

(defmethod vsync ((context context))
  (ecase (glfw:swap-interval context)
    (0 :off)
    (-1 :adaptive)
    (T :on)))

(defmethod (setf vsync) (value (context context))
  (setf (glfw:swap-interval context) (ecase value ((NIL :off) 0) ((:on T) 1) (:adaptive -1))))

(defmethod version ((context context))
  (list (glfw:attribute :context-version-major context)
        (glfw:attribute :context-version-minor context)))

(defmethod clipboard ((context context))
  (request-event-queue (event-queue context) :get-clipboard))

(defmethod (setf clipboard) ((text string) (context context))
  (request-event-queue (event-queue context) :set-clipboard text))

(defmethod cursor-position ((context context))
  (destructuring-bind (x y) (glfw:cursor-location context)
    (let ((x-scale 1.0)
          (y-scale 1.0))
      (case (glfw:platform)
        ((:cocoa :wayland)
         (destructuring-bind (x y) (glfw:content-scale context)
           (setf x-scale x y-scale y))))
      (vec (* x-scale x)
           (* y-scale (- (glfw:height context) y))))))

(defmethod (setf cursor-position) (pos (context context))
  (let ((x-scale 1.0)
        (y-scale 1.0))
    (case (glfw:platform)
      ((:cocoa :wayland)
       (destructuring-bind (x y) (glfw:content-scale context)
         (setf x-scale x y-scale y))))
    (setf (glfw:cursor-location context) (list (* x-scale (vx pos))
                                               (* y-scale (- (glfw:height context) (vy pos))))))
  pos)

(defmethod poll-input ((context context))
  (glfw:poll-events :timeout NIL)
  (when (glfw:should-close-p context)
    (exit-render-loop)))

(defun make-context (&optional handler &rest initargs)
  (handler-case (glfw:init)
    #+trial-release (error () (error 'trial:context-creation-error :message "Failed to initialize GLFW.")))
  (apply #'make-instance 'context :handler handler initargs))

(defun launch-with-context (&optional main &rest initargs)
  (declare (optimize speed))
  (labels ((launch ()
             (handler-case (glfw:init)
               #+trial-release (error () (error 'trial:context-creation-error :message "Failed to initialize GLFW.")))
             (let ((main (apply #'make-instance main initargs)))
               (start main)
               (unwind-protect
                    (unless (typep main 'trial::single-threaded-display)
                      (let ((context (trial:context main)))
                        (trial:rename-thread "input-loop")
                        (v:debug :trial.backend.glfw "Entering input loop")
                        (flet ((handler (request arg)
                                 (handler-case
                                     (ecase request
                                       (:get-clipboard (glfw:clipboard-string context))
                                       (:set-clipboard (setf (glfw:clipboard-string context) arg)))
                                   #+trial-release
                                   (error (e)
                                     (v:debug :trial.backend.glfw e)
                                     (v:error :trial.backend.glfw "Failed to execute ~a: ~a" request e)
                                     ""))))
                          (declare (dynamic-extent #'handler))
                          (loop until (glfw:should-close-p context)
                                do (glfw:poll-events :timeout 0.005d0)
                                   (poll-input main)
                                   (handle-event-queue (event-queue context) #'handler)))))
                 (v:debug :trial.backend.glfw "Cleaning up")
                 (trial:with-ignored-errors-on-release (:trial.backend.glfw "Failed to shut down cleanly.")
                   (unwind-protect (finalize main)
                     (glfw:shutdown))))))
           (body ()
             (if (or (find :darwin *features*) (deploy:deployed-p))
                 (float-features:with-float-traps-masked T
                   (launch))
                 (launch))))
    #+darwin
    (trivial-main-thread:with-body-in-main-thread (:blocking T)
      (handler-bind ((error #'trial:standalone-error-handler))
        (body)))
    #-darwin
    (body)))

(defun refresh-window-size (context)
  (destructuring-bind (w h) (glfw:framebuffer-size context)
    (glfw:framebuffer-resized context w h)))

(defmethod glfw:debug-log ((context context) source type id severity message)
  (v:log (case type
           (:error :error)
           (:deprecated-behavior :debug)
           (:undefined-behavior :debug)
           (:portability :debug)
           (:performance :debug)
           (T :info))
         (list :gl source)
         "~a" message))

(defmacro %handle (context event-type &rest args)
  `(when (handler ,context)
     (handle (make-event ,event-type ,@args) (handler ,context))))

(defmethod glfw:framebuffer-resized ((context context) w h)
  (let ((x-scale 1.0)
        (y-scale 1.0))
    #+darwin
    (destructuring-bind (x y) (glfw:content-scale context)
      (setf x-scale x y-scale y))
    (let ((w (round (* x-scale w)))
          (h (round (* y-scale h))))
      (when (and (< 0 w) (< 0 h)
                 (or (not (= w (width context))) (not (= h (height context)))))
        (v:info :trial.backend.glfw "Framebuffer resized to ~ax~a" w h)
        (setf (width context) w)
        (setf (height context) h)
        (%handle context 'resize :width w :height h)))))

(defmethod glfw:window-focused ((context context) focusedp)
  (v:info :trial.backend.glfw "Window has ~:[lost~;gained~] focus" focusedp)
  (when focusedp (refresh-window-size context))
  (if focusedp
      (%handle context 'gain-focus)
      (%handle context 'lose-focus)))

(defmethod glfw:window-iconified ((context context) iconifiedp)
  (v:info :trial.backend.glfw "Window has been ~:[restored~;iconified~]" iconifiedp)
  (setf (visible-p context) (and (not iconifiedp) (glfw:visible-p context)))
  (unless iconifiedp (refresh-window-size context))
  (if iconifiedp
      (%handle context 'window-hidden)
      (%handle context 'window-shown)))

(defmethod glfw:key-changed ((context context) key scancode action modifiers)
  (declare (ignore scancode))
  (unless (eql :unknown key)
    (case action
      (:press
       (v:trace :trial.input "Key pressed: ~a" key)
       (%handle context 'key-press :key (glfw-key->key key) :modifiers modifiers))
      (:repeat
       (v:trace :trial.input "Key repeated: ~a" key)
       (%handle context 'key-press :key (glfw-key->key key) :modifiers modifiers :repeat T))
      (:release
       (v:trace :trial.input "Key released: ~a" key)
       (%handle context 'key-release :key (glfw-key->key key) :modifiers modifiers)))))

(defmethod glfw:char-entered ((context context) char)
  (when (< char #x110000)
    (let ((char (code-char char)))
      (%handle context 'text-entered :text (string char)))))

(defmethod glfw:mouse-button-changed ((context context) button action modifiers)
  (declare (ignore modifiers))
  (let ((pos (mouse-pos context))
        (button (glfw-button->button button)))
    (case action
      (:press
       (v:trace :trial.input "Mouse pressed: ~a" button)
       (%handle context 'mouse-press :pos pos :button button))
      (:release
       (v:trace :trial.input "Mouse released: ~a" button)
       (%handle context 'mouse-release :pos pos :button button)
       (let* ((click (last-click context))
              (time (get-internal-real-time))
              (diff (/ (- time (last-click-time click)) internal-time-units-per-second)))
         (when (and (< diff 0.5) (eql (last-click-button click) button) (v= pos (last-click-pos click)))
           (v:trace :trial.input "Double click")
           (%handle context 'mouse-double-click :pos pos :button button))
         (setf (last-click-button click) button)
         (setf (last-click-time click) time)
         (v<- (last-click-pos click) pos))))))

(defmethod glfw:mouse-scrolled ((context context) x y)
  (v:trace :trial.input "Mouse wheel: ~a ~a" x y)
  (%handle context 'mouse-scroll :pos (mouse-pos context) :delta y))

(defmethod glfw:mouse-moved ((context context) x y)
  (let ((x-scale 1.0)
        (y-scale 1.0))
    (case (glfw:platform)
      ((:cocoa :wayland)
       (destructuring-bind (x y) (glfw:content-scale context)
         (setf x-scale x y-scale y))))
    (let ((current (vec (* x-scale x) (* y-scale (- (glfw:height context) y)))))
      (%handle context 'mouse-move :pos current :old-pos (mouse-pos context))
      (setf (mouse-pos context) current))))

(defmethod glfw:window-closed :after ((context context))
  (%handle context 'window-close))

(defmethod glfw:file-dropped ((context context) paths)
  (when paths
    (%handle context 'file-drop-event :paths paths :pos (mouse-pos context))))

(defun glfw-button->button (button)
  (case button
    ((:1 :left) :left)
    ((:2 :right) :right)
    (:3 :middle)
    (:4 :mouse-4)
    (:5 :mouse-5)
    (:6 :mouse-6)
    (:7 :mouse-7)
    (:8 :mouse-8)
    (T button)))

(defun glfw-key->key (key)
  (case key
    (:grave-accent :section)
    (T key)))

(defun key->glfw-key (key)
  (case key
    (:section :grave-accent)
    (T key)))

(defmethod local-key-string ((context context) (scancode integer))
  (%glfw:get-key-name :unknown scancode))

(defmethod local-key-string ((context context) (key symbol))
  (%glfw:get-key-name (key->glfw-key key) 0))

(defmethod current-monitor ((context context))
  (let ((monitor (glfw:monitor context)))
    (when monitor
      (unless (typep monitor 'monitor)
        (change-class (glfw:monitor context) 'monitor))
      monitor)))

(defmethod current-video-mode ((monitor monitor))
  (let ((mode (glfw:video-mode monitor)))
    (list (first mode) (second mode) (third mode) (name monitor))))

(defmethod list-monitors ((context context))
  (loop for monitor in (glfw:list-monitors)
        do (unless (typep monitor 'monitor)
             (change-class monitor 'monitor))
        collect monitor))

(defmethod list-video-modes ((context context))
  (let ((monitor (current-monitor context)))
    (when monitor (list-video-modes monitor))))

(defmethod list-video-modes ((monitor monitor))
  (loop for (w h r) in (glfw:video-modes monitor)
        collect (list w h r (name monitor))))

(defmethod width ((monitor monitor))
  (glfw:width monitor))

(defmethod height ((monitor monitor))
  (glfw:height monitor))
