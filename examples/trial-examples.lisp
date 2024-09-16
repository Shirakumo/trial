(defpackage #:org.shirakumo.fraf.trial.examples
  (:nicknames #:trial-examples)
  (:use #:cl+trial)
  (:shadow #:launch #:physics-scene)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:trial-alloy #:org.shirakumo.fraf.trial.alloy)
   (#:assets #:org.shirakumo.fraf.trial.assets)
   (#:spaces #:org.shirakumo.fraf.trial.space)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
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
  ((disable-ui :initform NIL :accessor disable-ui)))

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
     (let ((fbo (flow:node (flow:left (first (flow:connections (port (node 'blend-pass scene) 'a-pass))))))
           (path (make-pathname :name (format-timestring :as :filename) :type "png" :defaults (user-homedir-pathname))))
       (save-image fbo path T)
       (v:info :trial.examples "Saved screenshot to ~a" path)))))

(defclass example-ui (trial-alloy:panel)
  ())

(defmethod initialize-instance :after ((panel example-ui) &key scene)
  (setup-ui scene panel))

(defmacro define-example (name &body body)
  (form-fiddle:with-body-options (body options title (scene-class (trial::mksym #.*package* name '-scene)) slots superclasses (test T)) body
    (assert (null options))
    `(progn
       (pushnew ',name *examples*)
       
       (defclass ,scene-class (,@superclasses example-scene)
         ,slots)

       (defmethod title ((example (eql ',name)))
         ,(or title (string-downcase name)))

       (defmethod title ((scene ,scene-class))
         ,(or title (string-downcase name)))
       
       (defmethod setup-scene ((main example) (scene ,scene-class))
         ,@body
         scene)

       (defmethod available-p ((example (eql ',name)))
         ,test)

       (defmethod change-scene ((main example) (scene (eql ',name)) &key (old (scene main)))
         (change-scene main (make-instance ',scene-class) :old old))

       (when (and +main+ (slot-boundp +main+ 'scene) (typep (scene +main+) ',scene-class))
         (issue T 'reload-scene)))))

(define-shader-pass ui (trial-alloy:base-ui)
  ())

(defclass example-list (trial-alloy:menuing-panel) ())

(defmethod initialize-instance :after ((list example-list) &key)
  (let ((layout (make-instance 'alloy:vertical-linear-layout))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (dolist (example (sort (list-examples) #'string< :key #'title))
      (if (available-p example)
          (make-instance 'alloy:button* :value (title example) :layout-parent layout :focus-parent focus
                                        :on-activate (lambda () (change-scene +main+ example)))
          (v:info :trial.examples "The example ~a is not available on your system." example)))
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

#+linux
(trial::dont-deploy
 org.shirakumo.file-select.gtk::gmodule
 org.shirakumo.file-select.gtk::gio
 org.shirakumo.file-select.gtk::gtk
 org.shirakumo.file-select.gtk::glib)
#+darwin
(trial::dont-deploy
 org.shirakumo.file-select.macos::foundation
 org.shirakumo.file-select.macos::appkit
 org.shirakumo.file-select.macos::cocoa)
