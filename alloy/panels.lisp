(in-package #:org.shirakumo.fraf.trial.alloy)

(trial:define-shader-pass base-ui (ui-pass
                                   alloy:fixed-scaling-ui
                                   presentations:default-look-and-feel)
  ((panels :initform () :accessor panels)
   (alloy:target-resolution :initform (alloy:px-size 1280 720))
   (alloy:scales :initform '((3840 T 2.0)
                             (2800 T 1.5)
                             (1920 T 1.25)
                             (1280 T 1.0)
                             (1000 T 0.8)
                             (T T 0.5)))))

(defmethod initialize-instance :after ((ui base-ui) &key)
  (make-instance 'alloy:fullscreen-layout :layout-parent (alloy:layout-tree ui))
  (make-instance 'alloy:focus-list :focus-parent (alloy:focus-tree ui)))

(defmethod trial:clear :after ((ui base-ui))
  (alloy:clear (alloy:layout-tree ui))
  (alloy:clear (alloy:focus-tree ui))
  (setf (panels ui) ()))

(defmethod trial:handle :around ((ev trial:event) (ui base-ui))
  (unless (call-next-method)
    (dolist (panel (panels ui))
      (trial:handle ev panel))))

(defmethod trial:stage :after ((ui base-ui) (area trial:staging-area))
  (dolist (panel (panels ui))
    (trial:stage panel area)))

(defun find-panel (panel-type &optional (ui (ui)))
  (declare (optimize speed))
  (loop for panel in (panels ui)
        do (when (typep panel panel-type)
             (return panel))))

(define-compiler-macro find-panel (panel-type &optional (ui '(ui)))
  `(loop for panel in (panels ,ui)
         do (when (typep panel ,panel-type)
              (return panel))))

(defun toggle-panel (panel-type &rest initargs)
  (let ((panel (find-panel panel-type)))
    (if panel
        (trial:hide panel)
        (trial:show (apply #'make-instance panel-type initargs)))))

(defun show-panel (panel-type &rest initargs)
  (let ((panel (find-panel panel-type)))
    (or panel
        (trial:show (apply #'make-instance panel-type initargs)))))

(defun hide-panel (panel-type &optional (ui (ui)))
  (let ((panels (panels ui)))
    (loop for panel = (first panels)
          do (cond ((null panel)
                    (return))
                   ((typep panel panel-type)
                    (trial:hide panel)))
             (setf panels (cdr panels)))))

(defclass panel (alloy:structure)
  ((trial:active-p :initform NIL :accessor trial:active-p)))

(defmethod alloy:render :around ((ui ui) (panel panel))
  (unless (find-panel 'fullscreen-panel)
    (call-next-method)))

(defmethod trial:handle ((ev trial:event) (panel panel)))

(defmethod shown-p ((panel panel))
  (alloy:layout-tree (alloy:layout-element panel)))

(defmethod trial:visible-p ((panel panel))
  (alloy:layout-tree (alloy:layout-element panel)))

(defmethod trial:show ((panel panel) &key (ui (ui)))
  (when trial:+main+
    (trial:commit panel (trial:loader trial:+main+) :unload NIL))
  ;; Then attach to the UI
  (when (alloy:focus-element panel)
    (dolist (panel (panels ui))
      (when (trial:active-p panel)
        (setf (trial:active-p panel) NIL)
        (return))))
  (alloy:enter panel (alloy:root (alloy:layout-tree ui)))
  (alloy:register panel ui)
  (when (alloy:focus-element panel)
    (alloy:enter panel (alloy:root (alloy:focus-tree ui)))
    (setf (alloy:focus (alloy:focus-element panel)) :strong))
  (push panel (panels ui))
  (setf (trial:active-p panel) T)
  panel)

(defmethod trial:hide ((panel panel))
  (let ((ui (ui)))
    (when (slot-boundp (alloy:layout-element panel) 'alloy:layout-parent)
      (alloy:leave panel (alloy:root (alloy:layout-tree ui)))
      (alloy:leave panel (alloy:root (alloy:focus-tree ui)))
      (setf (panels ui) (remove panel (panels ui))))
    (when (trial:active-p panel)
      (setf (trial:active-p panel) NIL)
      (dolist (panel (panels ui))
        (when (alloy:focus-element panel)
          (setf (trial:active-p panel) T)
          (setf (alloy:focus (alloy:focus-element panel)) :strong)
          (return))))
    panel))

(defclass fullscreen-panel (panel)
  ())

(defclass action-set-change-panel (panel)
  ((prior-action-set :initform NIL :accessor prior-action-set)))

(defmethod trial:show :before ((panel action-set-change-panel) &key)
  (setf (prior-action-set panel) (trial:active-action-set)))

(defmethod trial:hide :after ((panel action-set-change-panel))
  (unless (eql (trial:action-set (prior-action-set panel))
               (trial:active-action-set))
    (trial:reset-retained (trial:scene trial:+main+)))
  (setf (trial:active-p (trial:action-set (prior-action-set panel))) T))

(defclass menuing-panel (action-set-change-panel fullscreen-panel)
  ())

(defmethod (setf trial:active-p) :after (value (panel menuing-panel))
  (when value
    (setf (trial:active-p (trial:action-set 'ui-actions)) T)))
