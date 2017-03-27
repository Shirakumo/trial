#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass pipeline ()
  ((connections :initform (make-hash-table :test 'eq) :accessor connections)
   (passes :initform () :accessor passes)
   (framebuffers :initform #() :accessor framebuffers)
   (pass-fbo-map :initform (make-hash-table :test 'eq) :accessor pass-fbo-map)))

(defmethod load progn ((pipeline pipeline))
  (map NIL #'load (framebuffers pipeline)))

(defmethod offload progn ((pipeline pipeline))
  (map NIL #'offload (framebuffers pipeline)))

(defmethod resize ((pipeline pipeline) width height)
  (loop for framebuffer across (framebuffers pipeline)
        do (resize framebuffer width height)))

(defmethod register ((pass shader-pass) (pipeline pipeline))
  (pushnew pass (passes pipeline)))

(defmethod deregister ((pass shader-pass) (pipeline pipeline))
  (setf (passes pipeline) (delete pass (passes pipeline)))
  (remhash pass (connections pipeline)))

;; FIXME: At some point we should probably allow doing more automated
;;        connections and nodes that use a previous node's FBOs as
;;        their own.
(defmethod connect-pass (source-pass target-pass target-input (pipeline pipeline))
  (unless (find target-input (pass-inputs (class-of target-pass)))
    (error "The pass input ~s does not exist on ~a."
           target-input target-pass))
  (unless (find source-pass (passes pipeline))
    (pushnew source-pass (passes pipeline)))
  (unless (find target-pass (passes pipeline))
    (pushnew target-pass (passes pipeline)))
  ;; FIXME: override on exists
  (push (list target-input source-pass)
        (gethash target-pass (connections pipeline))))

(defun connections->edges (connections)
  (let ((table (make-hash-table :test 'eq)))
    (loop for k being the hash-keys of connections
          for v being the hash-values of connections
          do (setf (gethash k table)
                   (mapcar #'second v)))
    table))

(defmethod pack-pipeline ((pipeline pipeline) target)
  (let* ((nodes (passes pipeline))
         (edges (connections->edges (connections pipeline)))
         (passes (flatten-dag nodes edges))
         (colors (color-graph nodes edges))
         (framebuffers (make-array (loop for color being the hash-values of colors
                                         maximize color))))
    ;; Allocate FBOs
    (loop for i from 0 below (length framebuffers)
          do (setf (aref framebuffers i)
                   (make-asset 'framebuffer-bundle-asset :depth-attachment
                               :width (width target)
                               :height (height target))))
    ;; Optimise color table
    (loop for pass being the hash-keys of colors
          for color being the hash-values of colors
          do (setf (gethash pass color) (aref framebuffers color)))
    ;; Commit
    (setf (passes pipeline) passes)
    (setf (framebuffers pipeline) framebuffers)
    (setf (pass-fbo-map pipeline) colors)))

(defun color-graph (nodes edges)
  ;; Greedy colouring
  (let ((result (make-hash-table :test 'eq))
        (available (make-array (length nodes) :initial-element T)))
    (setf (gethash (pop nodes) result) 0)
    (dolist (node nodes result)
      ;; Mark adjacent as unavailable
      (dolist (to (gethash node edges))
        (let ((color (gethash to result)))
          (when color (setf (aref available color) NIL))))
      ;; Assign available
      (setf (gethash node result)
            (loop for i from 0 below (length available)
                  do (when (aref available i)
                       (return i))))
      ;; Reset availability on adjacent
      (dolist (to (gethash node edges))
        (let ((color (gethash to result)))
          (when color (setf (aref available color) T)))))))

(defun flatten-dag (nodes edges)
  ;; Tarjan
  (let ((nodes* (make-hash-table :test 'eql))
        (sorted ()))
    (dolist (node nodes)
      (setf (gethash node nodes*) :unvisited))
    (labels ((visit (node)
               (case (gethash node nodes*)
                 (:temporary
                  (error "Detected loop in shader pass dependency graph."))
                 (:unvisited
                  (setf (gethash node nodes*) :temporary)
                  (dolist (target (gethash node edges))
                    (visit target))
                  (remhash node nodes*)
                  (push node sorted)))))
      (loop while (with-hash-table-iterator (iterator nodes)
                    (multiple-value-bind (found node) (iterator)
                      (when found (visit node) T)))))
    sorted))

(defmethod paint ((pipeline pipeline) target)
  (let ((pass-fbo-map (pass-fbo-map pipeline))
        (passes (passes pipeline)))
    (loop for pass in passes
          for fbo = (gethash pass pass-fbo-map)
          do (gl:bind-framebuffer :framebuffer (resource fbo))
             (paint pass target)
          finally (paint fbo target))))
