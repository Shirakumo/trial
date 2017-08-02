#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget scene-graph (QDialog)
  ((scene :initarg :scene :accessor scene))
  (:default-initargs :scene (error "SCENE required.")))

(define-initializer (scene-graph setup)
  (setf (q+:window-title scene-graph) "Scene Graph")
  (q+:resize scene-graph 300 500)
  (refresh-instances scene-graph))

(define-subwidget (scene-graph tree)
    (q+:make-qtreewidget)
  (setf (q+:column-count tree) 2)
  (setf (q+:header-labels tree) '("Name" "Class")))

(define-subwidget (scene-graph pause)
    (q+:make-qpushbutton)
  (setf (q+:icon pause) (q+:standard-icon (q+:style pause)
                                          (q+:qstyle.sp_media-pause)))
  (setf (q+:tool-tip pause) "Un/Pause the scene."))

(define-subwidget (scene-graph enter)
    (q+:make-qpushbutton)
  (setf (q+:icon enter) (q+:standard-icon (q+:style enter)
                                          (q+:qstyle.sp_file-dialog-new-folder)))
  (setf (q+:tool-tip enter) "Enter a new unit into the scene."))

(define-subwidget (scene-graph leave)
    (q+:make-qpushbutton)
  (setf (q+:icon leave) (q+:standard-icon (q+:style leave)
                                          (q+:qstyle.sp_dialog-close-button)))
  (setf (q+:tool-tip leave) "Leave the selected unit from the scene."))

(define-subwidget (scene-graph reload)
    (q+:make-qpushbutton)
  (setf (q+:icon reload) (q+:standard-icon (q+:style reload)
                                           (q+:qstyle.sp_dialog-reset-button)))
  (setf (q+:tool-tip reload) "Reload the scene fully."))

(define-subwidget (scene-graph layout)
    (q+:make-qgridlayout scene-graph)
  (q+:add-widget layout tree 0 0 1 4)
  (q+:add-widget layout pause 1 0 1 1)
  (q+:add-widget layout enter 1 1 1 1)
  (q+:add-widget layout leave 1 2 1 1)
  (q+:add-widget layout reload 1 3 1 1)
  (setf (q+:spacing layout) 0))

(define-slot (scene-graph refresh refresh-instances) ()
  (q+:clear tree)
  (labels ((r (container parent)
             (flare-queue:do-queue (entity (flare:objects container))
               (let ((item (q+:make-qtreewidgetitem)))
                 (setf (q+:text item 0) (format NIL "~(~s~)" (flare:name entity)))
                 (setf (q+:text item 1) (format NIL "~(~s~)" (class-name (class-of entity))))
                 (q+:add-child parent item)))))
    (let ((item (q+:make-qtreewidgetitem))
          (scene (scene scene-graph)))
      (setf (q+:text item 0) (format NIL "~(~s~)" (flare:name scene)))
      (setf (q+:text item 1) (format NIL "~(~s~)" (class-name (class-of scene))))
      (r scene item)
      (q+:insert-top-level-item tree 0 item)))
  (q+:resize-column-to-contents tree 0)
  (q+:expand-all tree))
