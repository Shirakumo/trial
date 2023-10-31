(defpackage #:org.shirakumo.fraf.trial.examples
  (:nicknames #:trial-examples)
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:trial-alloy #:org.shirakumo.fraf.trial.alloy)
   (#:assets #:org.shirakumo.fraf.trial.assets)
   (#:spaces #:org.shirakumo.fraf.trial.space)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:v #:org.shirakumo.verbose))
  (:export #:launch))

(in-package #:org.shirakumo.fraf.trial.examples)

(defvar *examples* ())

(define-pool examples)

(defclass example (main)
  ((paused-p :initform NIL :accessor paused-p))
  (:default-initargs :clear-color (vec 0.13 0.15 0.1)
                     :context '(:vsync T :version (4 3))))

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
  ())

(defmethod setup-scene :after ((main example) (scene example-scene))
  (let ((output (car (last (nodes scene))))
        (ui (make-instance 'ui))
        (combine (make-instance 'blend-pass)))
    (connect (port output 'color) (port combine 'a-pass) scene)
    (connect (port ui 'color) (port combine 'b-pass) scene)
    (ignore-errors (show (make-instance 'example-ui :scene scene) :ui ui))))

(defclass example-ui (trial-alloy:panel)
  ())

(defmethod initialize-instance :after ((panel example-ui) &key scene)
  (setup-ui scene panel))

(defmacro define-example (name &body body)
  (form-fiddle:with-body-options (body options title (scene-class (trial::mksym #.*package* name '-scene)) slots superclasses) body
    (assert (null options))
    `(progn
       (pushnew ',name *examples*)

       (defmethod title ((example (eql ',name)))
         ,(or title (string-downcase name)))
       
       (defclass ,scene-class (,@superclasses example-scene)
         ,slots)
       
       (defmethod setup-scene ((main example) (scene ,scene-class))
         ,@body
         scene)

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
    (dolist (example (list-examples))
      (make-instance 'alloy:button* :value (title example) :layout-parent layout :focus-parent focus
                                    :on-activate (lambda () (change-scene +main+ example))))
    (alloy:finish-structure list layout focus)))

(defmethod setup-scene ((main example) (scene scene))
  (enter (make-instance 'ui) scene)
  (trial-alloy:show-panel 'example-list))

(defun launch (&optional example &rest args)
  (apply #'trial:launch 'example :example example args))
