#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass pipeline ()
  ((connections :initform () :accessor connections)
   (passes :initform () :accessor passes)
   (framebuffers :initform #() :accessor framebuffers)
   (pass-fbo-map :initform (make-hash-table) :accessor pass-fbo-map)))

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
  (setf (passes pipeline) (delete pass (passes pipeline))))

(defmethod connect-pass (source-pass source-output target-pass target-input pipeline)
  (unless (or (find source-output '(:color :depth))
              (find source-output (pass-outputs source-pass)))
    (error "The pass output ~s does not exist on ~a."
           source-output source-pass))
  (unless (or (find target-input '(:color :depth))
              (find target-input (pass-inputs target-pass)))
    (error "The pass input ~s does not exist on ~a."
           target-input target-pass))
  (pushnew (list source-pass source-output target-pass target-input)
           (connections pipeline) :test #'equal))

(defmethod pack-pipeline ((pipeline pipeline) target)
  (let* ((nodes (passes pipeline))
         (edges (loop for connection in (connections pipeline)
                      collect (cons (first connection) (third connection))))
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
      (loop for (from to) in edges
            do (when (eql from node)
                 (let ((color (gethash to result)))
                   (when color (setf (aref available color) NIL)))))
      ;; Assign available
      (setf (gethash node result)
            (loop for i from 0 below (length available)
                  do (when (aref available i)
                       (return i))))
      ;; Reset availability on adjacent
      (loop for (from to) in edges
            do (when (eql from node)
                 (let ((color (gethash to result)))
                   (when color (setf (aref available color) T))))))))

(defun flatten-dag (nodes edges)
  ;; Tarjan
  (let ((edges* (make-hash-table :test 'eql))
        (nodes* (make-hash-table :test 'eql))
        (sorted ()))
    (dolist (node nodes)
      (setf (gethash node nodes*) :unvisited))
    (dolist (edge edges)
      (pushnew (cdr edge) (gethash (car edge) edges*)))
    (labels ((visit (node)
               (case (gethash node nodes*)
                 (:temporary
                  (error "Detected loop in shader pass dependency graph."))
                 (:unvisited
                  (setf (gethash node nodes*) :temporary)
                  (dolist (target (gethash node edges*))
                    (visit target))
                  (remhash node nodes*)
                  (push node sorted)))))
      (loop while (with-hash-table-iterator (iterator nodes)
                    (multiple-value-bind (found node) (iterator)
                      (when found (visit node) T)))))
    sorted))

(defmethod paint ((pipeline pipeline) target)
  (with-accessors ((pass-fbo-map pass-fbo-map) (passes passes)))
  (loop for pass in passes
        for fbo = (gethash pass pass-fbo-map)
        do () ;; FIXME: Resolve inputs to FBOs and set their uniforms
           (gl:bind-framebuffer :framebuffer (resource fbo))
           (paint pass target)
        finally (paint fbo target)))

(defmethod paint ((fbo framebuffer-bundle-asset) target)
  ;; FIXME: draw the FBO onto the general framebuffer.
  )
