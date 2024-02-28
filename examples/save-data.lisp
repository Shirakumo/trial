(in-package #:org.shirakumo.fraf.trial.examples)

(define-example save-data
  :superclasses (alloy:observable-object)
  :title "Save Data"
  :slots ((data :initform :hello :accessor data))
  (enter (make-instance 'render-pass) scene))

(define-simple-save-file example-1 :current
  (:decode (depot data)
    (setf (data (scene +main+)) data))
  (:encode (depot)
    (list (data (scene +main+)))))

(defclass save-state-button (alloy:button)
  ())

(defmethod alloy:text ((button save-state-button))
  (format NIL "~a ~a ~a"
          (slot (alloy:value button))
          (format-timestring :timestamp (save-time (alloy:value button)))
          (username (alloy:value button))))

(defmethod alloy:activate ((button save-state-button))
  (load-save-data (alloy:value button) T)
  (v:info :trial.examples.save-data "Loaded ~a" (alloy:value button)))

(defmethod setup-ui ((scene save-data-scene) panel)
  (let* ((layout (make-instance 'alloy:grid-layout :col-sizes '(T) :row-sizes '(T 30 30)))
         (focus (make-instance 'alloy:vertical-focus-list))
         (clipper (make-instance 'alloy:clip-view :limit :x :layout-parent layout))
         (listf (make-instance 'alloy:vertical-focus-list :focus-parent focus))
         (list (make-instance 'alloy:vertical-linear-layout :min-size (alloy:size 100 50) :layout-parent clipper)))
    (flet ((populate-list ()
             (alloy:clear list)
             (alloy:clear listf)
             (dolist (save (list-save-files))
               (alloy:represent save 'save-state-button :layout-parent list :focus-parent focus))))
      (populate-list)
      (alloy:represent (slot-value scene 'data) 'alloy::printable :focus-parent focus :layout-parent layout)
      (let ((save (alloy:represent "New Save" 'alloy:button :focus-parent focus :layout-parent layout)))
        (alloy:on alloy:activate (save)
          (let ((save (store-save-data 0 T)))
            (v:info :trial.examples.save-data "Saved ~a" save))
          (populate-list))))
    (alloy:finish-structure panel layout focus)))
