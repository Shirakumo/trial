(in-package #:org.shirakumo.fraf.trial)

(defgeneric apply-transforms (object)
  (:method-combination progn :most-specific-last))

(defgeneric clear (container))
(defgeneric enter (thing container))
(defgeneric leave (thing container))
(defgeneric register (thing container))
(defgeneric deregister (thing container))
(defgeneric contains-p (thing container))

(declaim (inline map-scene-graph))
(defun map-scene-graph (function root)
  (declare (type function function))
  (declare (optimize speed))
  (labels ((process (node)
             (funcall function node)
             (when (typep node 'container)
               (sequences:dosequence (child node)
                 (process child)))))
    (process root)))

(defmacro do-scene-graph ((node graph &optional return) &body body)
  `(block NIL
     (map-scene-graph (lambda (,node) ,@body) ,graph)
     ,return))

(defclass scene-node ()
  ((container :initarg :container :initform NIL :accessor container)))

(defgeneric scene (node))

(defmethod scene ((node scene-node))
  (loop for parent = (container node)
        do (if parent
               (setf node parent)
               (return node))))

(defmethod leave ((node scene-node) (container (eql T)))
  (when (container node)
    (leave node (container node))))

(defmethod ancestor (name (node scene-node))
  (labels ((recurse (node)
             (cond ((equal name (name node))
                    node)
                   ((container node)
                    (recurse (container node))))))
    (recurse (container node))))

(defun ancestor-p (ancestor node)
  (labels ((recurse (node)
             (cond ((eq ancestor node)
                    ancestor)
                   ((container node)
                    (recurse (container node))))))
    (recurse (container node))))

(defclass container (scene-node sequences:sequence)
  ())

(defmethod apply-transforms progn ((container container)))

(defmethod contains-p ((node scene-node) (container container))
  (eq (container node) container))

(defmethod register ((node scene-node) (null null))
  node)

(defmethod deregister ((node scene-node) (null null))
  node)

(defmethod register ((node scene-node) (container container))
  (register node (container container)))

(defmethod deregister ((node scene-node) (container container))
  (deregister node (container container)))

(defmethod enter :after ((node scene-node) (container container))
  (register node container)
  (setf (container node) container))

(defmethod leave :after ((node scene-node) (container container))
  (deregister node container)
  (setf (container node) NIL))

(defmethod clear :before ((container container))
  (sequences:dosequence (child container)
    (deregister child container)))

(defmethod register :after ((child container) (parent container))
  (sequences:dosequence (node child)
    (register node parent)))

(defmethod deregister :before ((child container) (parent container))
  (sequences:dosequence (node child)
    (deregister node parent)))

(defmethod node (name (container container))
  (sequences:dosequence (child container)
    (when (equal name (name child))
      (return child))
    (when (typep child 'container)
      (let ((result (node name child)))
        (when result (return result))))))

#-elide-container-checks
(defmethod enter :before ((node scene-node) (container container))
  (when (container node)
    (error "The node~%  ~a~%cannot be entered into~%  ~a~%as it is already contained in~%  ~a"
           node container (container node))))

#-elide-container-checks
(defmethod leave :before ((node scene-node) (container container))
  (when (and (container node) (not (eq container (container node))))
    (error "The entity~%  ~a~%cannot be left from~%  ~a~%as it is contained in~%  ~a"
           node container (container node))))

(defmethod finalize ((container container))
  (for:for ((object over container))
    (finalize object))
  (clear container))

(defmethod describe-object :after ((container container) stream)
  (format stream "~&~%Entity Tree:~%")
  (3ds::describe-tree container #'identity stream))

(defmethod <- progn ((target container) (source container))
  (sequences:dosequence (entity source target)
    (enter (clone entity) target)))

(defclass entity (scene-node)
  ((name :initform NIL :initarg :name :accessor name)))

(defmethod print-object ((entity entity) stream)
  (if (name entity)
      (print-unreadable-object (entity stream :type T :identity NIL)
        (format stream "~s" (name entity)))
      (call-next-method)))

(defmethod descriptor ((entity entity))
  (if (name entity)
      (prin1-to-string (name entity))
      (call-next-method)))

(defmethod (setf name) :around (name (entity entity))
  (unless (eq name (name entity))
    (let ((scene (scene entity)))
      (cond ((eq entity scene)
             (call-next-method))
            (T
             (deregister entity scene)
             (call-next-method)
             (register entity scene))))))

(define-transfer entity name)

(defmethod apply-transforms progn ((entity entity))
  (when (container entity)
    (apply-transforms (container entity))))

(defmethod global-location ((entity entity) &optional (target (vec3)))
  (with-pushed-matrix ()
    (apply-transforms entity)
    (with-fast-matref (m (model-matrix))
      (vsetf target (m 0 3) (m 1 3) (m 2 3)))))
