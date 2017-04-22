(in-package #:org.shirakumo.fraf.trial)

(defvar *dialogue-trees* (make-hash-table))

(defclass dialogue-tree () ())
(defclass dialogue-line () ())

(defun ensure-dialogue-tree (tree)
  (cond
    ((typep tree 'dialogue-tree) (values tree T))
    ((or (typep tree 'keyword)
         (typep tree 'symbol))
     (alexandria:ensure-gethash (alexandria:ensure-symbol tree :sphere) *dialogue-trees*
                                (make-instance 'dialogue-tree)))
    (T (error (format NIL "Invalid type ~a for a tree name. Use symbol or keyword instead."
                      (type-of tree))))))

(defun dialogue-tree (name &optional lines)
  (let ((tree (ensure-dialogue-tree name)))
    (loop for line in lines
          do (add-line tree (car line) (cdr line)))))

(defclass dialogue-tree ()
  ((lines :initform (make-hash-table))))

(defmethod line ((tree dialogue-tree) line)
  (gethash (alexandria:ensure-symbol line :sphere) (slot-value tree 'lines)))

(defmethod add-line ((tree dialogue-tree) line-name line)
  (setf (gethash line-name (slot-value tree 'lines)) line))

(defmethod lines ((tree dialogue-tree))
  (sort (alexandria:hash-table-keys (slot-value tree 'lines))
        #'(lambda (key1 key2) (string< (format NIL "~:@(~a~)" key1)
                                       (format NIL "~:@(~a~)" key2)))))

(defclass dialogue-line ()
  ((text :initarg :text :accessor text)
   (choices :initform (make-hash-table))))

(defmethod choice ((line dialogue-line) choice)
  (gethash choice (slot-value line 'choices)))

(defmethod add-choice ((line dialogue-line) choice)
  (let ((name (car choice))
        (options (cdr choice)))
    (unless name (error "Please name the dialogue line choice."))
    (unless (getf options :text) (setf (getf options :text) ""))
    (setf (gethash (alexandria:ensure-symbol name :sphere) (slot-value line 'choices))
          options)))

(defmethod choices ((line dialogue-line))
  (sort (alexandria:hash-table-keys (slot-value line 'choices))
        #'(lambda (key1 key2) (string< (format NIL "~:@(~a~)" key1)
                                       (format NIL "~:@(~a~)" key2)))))

(defmethod print-object ((line dialogue-line) stream)
  (format stream "~a~%~{~a~%~}" (text line)
          (mapcar #'(lambda (key)
                      (let ((choice (choice line key)))
                        (format NIL "> ~a: ~a -> ~a"
                                key (getf choice :text)
                                (or (getf choice :jump) "[quit]"))))
                  (choices line))))

(defun define-dialogue (name options &rest choices)
  (let ((tree (ensure-dialogue-tree (getf options :tree)))
        (line (make-instance 'dialogue-line :text (getf options :text ""))))
    (loop for choice in choices do (add-choice line choice))
    (add-line tree name line)
    line))

(indent:define-indentation define-dialogue (2 4 (&whole 2 &rest 1)))
