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
   (framebuffers :initform () :accessor framebuffers)))

(defmethod load progn ((pipeline pipeline))
  (mapc #'load (framebuffers pipeline)))

(defmethod offload progn ((pipeline pipeline))
  (mapc #'offload (framebuffers pipeline)))

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

(defmethod pack-pipeline ((pipeline pipeline))
  ;; Sort to flattened DAG.
  (setf (passes pipeline)
        (flatten-dag (loop for connection in (connections pipeline)
                           collect (cons (first connection) (third connection)))
                     (passes pipeline)))
  ;; Determine FBO allocation.

  ;; Compile down to efficient function.
  )

(defun flatten-dag (edges nodes)
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
  )
