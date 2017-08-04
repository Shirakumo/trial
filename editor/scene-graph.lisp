#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget scene-graph (QDialog)
  ((scene :initarg :scene :accessor scene)
   (entity->item-map :initform (make-hash-table :test 'eq) :accessor entity->item-map)
   (item->entity-map :initform (make-hash-table :test 'eq) :accessor item->entity-map))
  (:default-initargs :scene (error "SCENE required.")))

(define-initializer (scene-graph setup)
  (setf (q+:window-title scene-graph) "Scene Graph")
  (q+:resize scene-graph 300 500)
  (trial:add-handler scene-graph (scene scene-graph))
  (refresh-instances scene-graph))

(define-subwidget (scene-graph tree)
    (q+:make-qtreewidget)
  (setf (q+:column-count tree) 2)
  (setf (q+:header-labels tree) '("Name" "Class"))
  (setf (q+:stretch-last-section (q+:header tree)) NIL)
  (setf (q+:resize-mode (q+:header tree) 0) (q+:qheaderview.stretch)))

(define-subwidget (scene-graph pause)
    (q+:make-qpushbutton)
  (setf (q+:icon pause) (icon :pause))
  (setf (q+:tool-tip pause) "Un/Pause the scene."))

(define-subwidget (scene-graph enter)
    (q+:make-qpushbutton)
  (setf (q+:icon enter) (icon :add))
  (setf (q+:tool-tip enter) "Enter a new entity into the scene."))

(define-subwidget (scene-graph leave)
    (q+:make-qpushbutton)
  (setf (q+:icon leave) (icon :remove))
  (setf (q+:tool-tip leave) "Leave the selected entity from the scene."))

(define-subwidget (scene-graph reload)
    (q+:make-qpushbutton)
  (setf (q+:icon reload) (icon :refresh))
  (setf (q+:tool-tip reload) "Reload the scene fully."))

(define-subwidget (scene-graph layout)
    (q+:make-qgridlayout scene-graph)
  (q+:add-widget layout tree 0 0 1 4)
  (q+:add-widget layout pause 1 0 1 1)
  (q+:add-widget layout enter 1 1 1 1)
  (q+:add-widget layout leave 1 2 1 1)
  (q+:add-widget layout reload 1 3 1 1)
  (setf (q+:spacing layout) 0))

(defun make-scene-graph-item (entity scene-graph)
  (or (gethash entity (entity->item-map scene-graph))
      (let ((item (q+:make-qtreewidgetitem)))
        (setf (gethash entity (entity->item-map scene-graph)) item)
        (setf (gethash item (item->entity-map scene-graph)) entity)
        (setf (q+:text item 0) (format NIL "~(~s~)" (flare:name entity)))
        (setf (q+:text item 1) (format NIL "~(~s~)" (class-name (class-of entity))))
        (when (typep entity 'flare:container)
          (flare-queue:do-queue (child (flare:objects entity))
            (q+:add-child item (make-scene-graph-item child scene-graph))))
        item)))

(define-slot (scene-graph refresh refresh-instances) ()
  (clrhash (entity->item-map scene-graph))
  (clrhash (item->entity-map scene-graph))
  (q+:clear tree)
  (let ((item (q+:make-qtreewidgetitem))
        (scene (scene scene-graph)))
    (setf (q+:text item 0) (format NIL "~(~s~)" (flare:name scene)))
    (setf (q+:text item 1) (format NIL "~(~s~)" (class-name (class-of scene))))
    (flare-queue:do-queue (entity (flare:objects scene))
      (q+:add-child item (make-scene-graph-item entity scene-graph)))
    (q+:insert-top-level-item tree 0 item))
  (q+:resize-column-to-contents tree 0)
  (q+:expand-all tree))

(define-slot (scene-graph clicked) ((item "QTreeWidgetItem *") (column "int"))
  (declare (connected tree (item-double-clicked "QTreeWidgetItem *" "int")))
  (let ((entity (gethash item (item->entity-map scene-graph))))
    (case column
      (0 (inspect entity))
      (1 (q+:show (make-instance 'subject-chooser :instances-class (class-of entity)))))))

(define-slot (scene-graph pause) ()
  (declare (connected pause (clicked)))
  (if (flare:running (scene scene-graph))
      (flare:stop (scene scene-graph))
      (flare:start (scene scene-graph))))

(define-slot (scene-graph enter) ()
  (declare (connected enter (clicked)))
  )

(define-slot (scene-graph leave) ()
  (declare (connected leave (clicked)))
  (dolist (item (q+:selected-items tree))
    (trial:leave (gethash item (item->entity-map scene-graph)) (scene scene-graph))))

(define-slot (scene-graph reload) ()
  (declare (connected reload (clicked)))
  (trial:issue (scene scene-graph) 'trial:reload-scene))

(defmethod trial:handle (event (scene-graph scene-graph)))

(defmethod trial:handle ((enter trial:enter) (scene-graph scene-graph))
  (let* ((entity (trial:entity enter))
         (item (make-scene-graph-item entity scene-graph)))
    ;; Wrong! We get enter events for sub-containers too...
    (q+:add-child (q+:top-level-item (slot-value scene-graph 'tree) 0) item)))

(defmethod trial:handle ((leave trial:leave) (scene-graph scene-graph))
  (let* ((entity (trial:entity leave))
         (item (gethash entity (entity->item-map scene-graph))))
    (remhash entity (entity->item-map scene-graph))
    (remhash item (item->entity-map scene-graph))
    (when item
      (q+:remove-child (q+:parent item) item)
      (finalize item))))
