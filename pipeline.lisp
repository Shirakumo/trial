#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass pipeline (entity event-loop)
  ((connections :initform (make-hash-table :test 'eq) :accessor connections)
   (passes :initform () :accessor passes)
   (framebuffers :initform #() :accessor framebuffers)
   (pass-fbo-map :initform (make-hash-table :test 'eq) :accessor pass-fbo-map)
   (copy-pass :initform (make-instance 'copy-pass) :accessor copy-pass)))

(defmethod load progn ((pipeline pipeline))
  (map NIL #'load (framebuffers pipeline))
  (map NIL #'load (passes pipeline))
  (load (copy-pass pipeline)))

(defmethod offload progn ((pipeline pipeline))
  (map NIL #'offload (framebuffers pipeline))
  (map NIL #'offload (passes pipeline))
  (offload (copy-pass pipeline)))

(defmethod resize ((pipeline pipeline) width height)
  (loop for framebuffer across (framebuffers pipeline)
        do (resize framebuffer width height)))

(defmethod register ((pass shader-pass) (pipeline pipeline))
  (pushnew pass (passes pipeline)))

(defmethod deregister ((pass shader-pass) (pipeline pipeline))
  (setf (passes pipeline) (delete pass (passes pipeline)))
  (remhash pass (connections pipeline))
  (loop for k being the hash-keys of (connections pipeline)
        for v being the hash-values of (connections pipeline)
        do (setf (gethash k (connections pipeline))
                 (remove pass v :key #'second))))

(defmethod clear ((pipeline pipeline))
  (offload pipeline)
  (clrhash (connections pipeline))
  (clrhash (pass-fbo-map pipeline))
  (setf (passes pipeline) ())
  (setf (framebuffers pipeline) #()))

;; FIXME: At some point we should probably allow doing more automated
;;        connections and nodes that use a previous node's FBOs as
;;        their own.
(defmethod connect-pass (source-pass target-pass target-input (pipeline pipeline))
  (let ((connections (connections pipeline)))
    (unless (find target-input (pass-inputs (class-of target-pass)) :test #'string=)
      (error "The pass input ~s does not exist on ~a."
             target-input target-pass))
    (unless (find source-pass (passes pipeline))
      (pushnew source-pass (passes pipeline)))
    (unless (find target-pass (passes pipeline))
      (pushnew target-pass (passes pipeline)))
    ;; Remove potential previous connection
    (setf (gethash target-pass connections)
          (remove target-input (gethash target-pass connections)
                  :key #'first :test #'string=))
    (push (list target-input source-pass)
          (gethash target-pass connections))))

(defun connections->edges (connections)
  (let ((edges))
    (loop for k being the hash-keys of connections
          for v being the hash-values of connections
          do (loop for connection in v
                   do (push (cons k connection) edges)))
    edges))

(defmethod check-consistent ((pipeline pipeline))
  (dolist (pass (passes pipeline))
    (dolist (input (pass-inputs (class-of pass)))
      (unless (find input (gethash pass (connections pipeline))
                    :test #'string= :key #'first)
        (error "Pipeline is not consistent.~%~
                Pass ~s is missing a connection to its input ~s."
               pass input)))))

(defmethod pack-pipeline ((pipeline pipeline) target)
  (check-consistent pipeline)
  (v:info :trial.pipeline "~a packing for ~a" pipeline target)
  (let* ((nodes (passes pipeline))
         (edges (connections->edges (connections pipeline)))
         (passes (flatten-graph nodes edges))
         (colors (color-graph nodes edges))
         (framebuffers (make-array (1+ (loop for color being the hash-values of colors
                                             maximize color)))))
    (v:info :trial.pipeline "~a pass order:   ~a" pipeline passes)
    (v:info :trial.pipeline "~a framebuffers: ~a" pipeline (length framebuffers))
    ;; Allocate FBOs
    (loop for i from 0 below (length framebuffers)
          do (setf (aref framebuffers i)
                   (make-asset 'framebuffer-bundle-asset (list :color :depth)
                               :width (width target)
                               :height (height target))))
    ;; Optimise color table
    (loop for pass being the hash-keys of colors
          for color being the hash-values of colors
          do (setf (gethash pass colors) (aref framebuffers color)))
    ;; Commit
    (dolist (pass passes)
      (setf (pass-inputs pass) ())
      (loop for (input source) in (gethash pass (connections pipeline))
            do (push (list input (gethash source colors))
                     (pass-inputs pass))))
    (setf (passes pipeline) passes)
    (setf (framebuffers pipeline) framebuffers)
    (setf (pass-fbo-map pipeline) colors)))

(defun color-graph (nodes edges)
  ;; Greedy colouring
  (let ((result (make-hash-table :test 'eq))
        (available (make-array (length nodes) :initial-element T)))
    (setf (gethash (pop nodes) result) 0)
    (flet ((mark-adjacent (node how)
             (loop for (from . to) in edges
                   do (cond ((eql node from)
                             (let ((color (gethash to result)))
                               (when color (setf (aref available color) how))))
                            ((eql node to)
                             (let ((color (gethash from result)))
                               (when color (setf (aref available color) how))))))))
      (dolist (node nodes result)
        ;; Mark adjacent as unavailable
        (mark-adjacent node NIL)
        ;; Assign available
        (setf (gethash node result)
              (loop for i from 0 below (length available)
                    do (when (aref available i)
                         (return i))))
        ;; Reset availability on adjacent
        (mark-adjacent node T)))
    result))

(defun flatten-graph (nodes edges)
  ;; Tarjan
  (let ((nodes* (make-hash-table :test 'eql))
        (edges* (make-hash-table :test 'eql))
        (sorted ()))
    (dolist (node nodes)
      (setf (gethash node nodes*) :unvisited))
    (dolist (edge edges)
      (push (cdr edge) (gethash (car edge) edges*)))
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
      (loop while (with-hash-table-iterator (iterator nodes*)
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
          finally (gl:bind-framebuffer :framebuffer 0)
                  (paint (copy-pass pipeline) target))))
