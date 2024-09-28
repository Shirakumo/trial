(in-package #:org.shirakumo.fraf.trial.examples)

(define-example save-data
  :superclasses (alloy:observable-object)
  :title "Save Data"
  :description "Showcase of Trial's built-in save data system."
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
  (let* ((layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout))
         (focus (make-instance 'alloy:vertical-focus-list))
         (clipper (make-instance 'alloy:clip-view :limit :x))
         (listf (make-instance 'alloy:vertical-focus-list :focus-parent focus))
         (list (make-instance 'alloy:vertical-linear-layout :min-size (alloy:size 100 50) :layout-parent clipper))
         (save-slot 0))
    (alloy:enter clipper layout :constraints `((:top 0) (:fill :width) (:bottom 70)))
    (flet ((populate-list ()
             (alloy:clear list)
             (alloy:clear listf)
             (dolist (save (list-save-files))
               (alloy:represent save 'save-state-button :layout-parent list :focus-parent focus))))
      (populate-list)
      (let ((data (alloy:represent (slot-value scene 'data) 'alloy::printable :focus-parent focus))
            (slot (alloy:represent save-slot 'alloy:wheel :focus-parent focus))
            (save (alloy:represent "New Save" 'alloy:button :focus-parent focus)))
        (alloy:enter data layout :constraints `((:below ,clipper 5) (:fill :width) (:height 30)))
        (alloy:enter slot layout :constraints `((:bottom 0) (:left 0) (:width 100) (:height 30)))
        (alloy:enter save layout :constraints `((:chain :right ,slot) (:right 0)))
        (alloy:on alloy:activate (save)
          (let ((save (store-save-data save-slot T)))
            (v:info :trial.examples.save-data "Saved ~a" save))
          (populate-list))))
    (alloy:finish-structure panel layout focus)))
