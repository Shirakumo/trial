(defpackage #:org.shirakumo.fraf.trial.examples
  (:nicknames #:trial-examples)
  (:use #:cl+trial)
  (:shadow #:launch #:physics-scene)
  (:local-nicknames
   (#:sequences #:org.shirakumo.trivial-extensible-sequences)
   (#:alloy #:org.shirakumo.alloy)
   (#:trial-alloy #:org.shirakumo.fraf.trial.alloy)
   (#:assets #:org.shirakumo.fraf.trial.assets)
   (#:spaces #:org.shirakumo.fraf.trial.space)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:harmony #:org.shirakumo.fraf.harmony.user)
   (#:trial-harmony #:org.shirakumo.fraf.trial.harmony)
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:v #:org.shirakumo.verbose))
  (:export #:launch))

(in-package #:org.shirakumo.fraf.trial.examples)

(defvar *examples* ())

(define-pool examples)

(defclass example (trial:main)
  ((paused-p :initform NIL :accessor paused-p))
  (:default-initargs :context '(:vsync T :version (3 3))))

(defmethod initialize-instance ((main example) &key example)
  (when example
    (let ((scene (trial::mksym #.*package* example '-scene)))
      (if (and (find-class scene NIL) (subtypep scene 'scene))
          (setf (scene main) (make-instance scene))
          (v:error :trial.examples "No such example ~s" example))))
  (call-next-method))

(defmethod update ((main example) tt dt fc)
  (cond ((paused-p main)
         (handle (make-event 'tick :tt tt :dt dt :fc fc) (camera (scene main))))
        (T
         (issue (scene main) 'pre-tick :tt tt :dt dt :fc fc)
         (issue (scene main) 'tick :tt tt :dt dt :fc fc)
         (issue (scene main) 'post-tick :tt tt :dt dt :fc fc)))
  (process (scene main)))

(defun list-examples ()
  (sort (copy-list *examples*)
        #+sb-unicode #'sb-unicode:unicode<
        #-sb-unicode #'string<
        :key #'string))

(defgeneric setup-ui (scene panel))

(defclass example-scene (pipelined-scene)
  ((disable-ui :initform NIL :accessor disable-ui)
   (title :initform NIL :accessor title)
   (description :initform NIL :accessor description)))

(defgeneric make-scene (name))

(defmethod title ((name symbol))
  (if name (title (make-scene name)) "Trial Examples"))

(defmethod description ((name symbol))
  (if name (description (make-scene name))
      #.(format NIL "Welcome to the Trial Examples!

You are looking at Trial ~a on ~a ~d

Please select an example from the list on the left and press [Launch] below to start it. During all examples, the following key bindings can be used:

F1 - Return to this main menu
F10 - Switch between 4:3 and 16:9
F11 - Switch between fullscreen and not
F12 - Save a screenshot

In scenes with a camera, the following controls are available:

Ctrl - Hold and move the mouse to view
WASD - Move around
C - Ascend
Space - Descend" (version :trial) (lisp-implementation-type) (lisp-implementation-version))))

(defmethod available-p ((name symbol))
  (if name (available-p (make-scene name)) NIL))

(defmethod change-scene ((main example) (scene symbol) &key (old (scene main)))
  (change-scene main (make-scene scene) :old old))

(defmethod setup-scene :after ((main example) (scene example-scene))
  (let ((output (car (nodes scene)))
        (ui (make-instance 'ui))
        (combine (make-instance 'blend-pass :name 'blend-pass)))
    (unless (disable-ui scene)
      (connect (port output 'color) (port combine 'a-pass) scene)
      (connect (port ui 'color) (port combine 'b-pass) scene)
      (show (make-instance 'example-ui :scene scene) :ui ui))
    (setf (title *context*) (format NIL "Trial Examples (~a)" (title scene)))))

(defmethod setup-ui ((scene example-scene) panel)
  (alloy:finish-structure panel
                          (make-instance 'alloy:fixed-layout)
                          (make-instance 'alloy:focus-list)))

(define-handler (scene key-press :after) (key)
  (case key
    (:f1
     (change-scene +main+ (make-instance 'pipelined-scene))
     (discard-events scene))
    (:f10
     (cond ((and (= (width *context*) 800) (= (height *context*) 600))
            (resize *context* 1280 720))
           (T
            (resize *context* 800 600))))
    (:f11
     (if (and (= (width (current-monitor *context*)) (width *context*))
              (= (height (current-monitor *context*)) (height *context*)))
         (show *context* :fullscreen NIL :mode (list 1280 720))
         (show *context* :fullscreen T)))
    (:f12
     (let ((fbo (if (node 'blend-pass scene)
                    (flow:node (flow:left (first (flow:connections (port (node 'blend-pass scene) 'a-pass)))))
                    scene))
           (path (make-pathname :name (format-timestring :as :filename) :type "png" :defaults (user-homedir-pathname))))
       (save-image fbo path T)
       (v:info :trial.examples "Saved screenshot to ~a" path)))))

(defclass example-ui (trial-alloy:panel)
  ())

(defmethod initialize-instance :after ((panel example-ui) &key scene)
  (setup-ui scene panel))

(defmacro define-example (name &body body)
  (form-fiddle:with-body-options (body options title description (scene-class (trial::mksym #.*package* name '-scene)) slots superclasses (test T)) body
    (assert (null options))
    `(progn
       (pushnew ',name *examples*)
       
       (defclass ,scene-class (,@superclasses example-scene)
         (,@slots
          (title :initform ,(or title (string-downcase name)))
          (description :initform ,description)))

       (defmethod make-scene ((name (eql ',name)))
         (make-instance ',scene-class))

       (defmethod available-p ((scene ,scene-class))
         ,test)
       
       (defmethod setup-scene ((main example) (scene ,scene-class))
         ,@body
         scene)

       (when (and +main+ (slot-boundp +main+ 'scene) (typep (scene +main+) ',scene-class))
         (issue T 'reload-scene)))))

(define-shader-pass ui (trial-alloy:base-ui)
  ())

(defclass example-list (trial-alloy:menuing-panel) ())

(defmethod title ((place alloy:value-data)) (title (alloy:value place)))
(defmethod description ((place alloy:value-data)) (description (alloy:value place)))
(defmethod (setf title) (value (place alloy:value-data)))
(defmethod (setf description) (value (place alloy:value-data)))

(defmethod initialize-instance :after ((list example-list) &key)
  (let ((layout (make-instance 'alloy:grid-layout :row-sizes '(T) :col-sizes '(300 20 T)))
        (focus (make-instance 'alloy:horizontal-focus-list))
        (data (make-instance 'alloy:value-data :value NIL)))
    (let* ((clip (make-instance 'alloy:clip-view :limit :x))
           (list (make-instance 'alloy:vertical-linear-layout :layout-parent clip))
           (listf (make-instance 'alloy:vertical-focus-list :focus-parent focus))
           (scroll (alloy:represent-with 'alloy:y-scrollbar clip :focus-parent focus)))
      (alloy:enter clip layout :col 0 :row 0)
      (alloy:enter scroll layout :col 1 :row 0)
      (dolist (example (sort (list-examples) #'string< :key #'title))
        (make-instance 'alloy:button* :value (title example) :layout-parent list :focus-parent listf
                                      :on-activate (lambda () (setf (alloy:value data) example)
                                                     (alloy:refresh layout)))))
    (let ((side (make-instance 'alloy:grid-layout :col-sizes '(T) :row-sizes '(50 T 50))))
      (alloy:enter side layout :col 2 :row 0)
      (alloy:represent (title data) 'alloy:label :layout-parent side :style `((:label :size ,(alloy:un 30))))
      (alloy:represent (description data) 'alloy:label :layout-parent side :style `((:label :valign :top :wrap T)))
      (make-instance 'alloy:button* :value "Launch" :layout-parent side :focus-parent focus
                                    :on-activate (lambda () (when (available-p (alloy:value data))
                                                              (change-scene +main+ (alloy:value data))))))
    (alloy:finish-structure list layout focus)))

(defmethod setup-scene ((main example) (scene scene))
  (setf (title *context*) "Trial Examples")
  (enter (make-instance 'ui) scene)
  (trial-alloy:show-panel 'example-list))

(defun launch (&optional example &rest args)
  (apply #'trial:launch 'example :example example args))

(defun main ()
  (command-line-toplevel)
  (launch))

