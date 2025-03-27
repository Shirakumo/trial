(in-package #:org.shirakumo.fraf.trial)

(defclass list-container (container)
  ((%objects :initform () :accessor %objects)))

(defmethod clear ((container list-container))
  (let ((objects (%objects container)))
    (loop while objects
          do (setf (container (pop objects)) NIL))
    (setf (%objects container) objects))
  container)

(defmethod enter (thing (container list-container))
  (push thing (%objects container))
  thing)

(defmethod leave (thing (container list-container))
  (setf (%objects container) (delete thing (%objects container)))
  thing)

(defmethod finalize ((container list-container))
  (for:for ((object in (%objects container)))
           (finalize object)))

(defmethod sequences:elt ((container list-container) index)
  (nth index (%objects container)))

(defmethod (setf sequences:elt) (thing (container list-container) index)
  (setf (nth index (%objects container)) thing))

(defmethod sequences:make-sequence-like ((container list-container) length &rest args &key (initial-element NIL iep) (initial-contents NIL icp) &allow-other-keys)
  (declare (ignore initial-element initial-contents))
  (let ((sub (make-instance 'list-container)))
    (setf (%objects sub) (apply #'sequences:make-sequence-like (%objects container) length args))
    (unless (or icp iep)
      (replace (%objects sub) (%objects container)))
    sub))

(defmethod sequences:adjust-sequence ((container list-container) length &rest args &key &allow-other-keys)
  (setf (%objects container) (apply #'sequences:adjust-sequence (%objects container) length args))
  container)

(defmethod sequences:make-sequence-iterator ((container list-container) &key (start 0) end from-end)
  (let ((list (%objects container)))
    (multiple-value-bind (iterator limit from-end)
        (if from-end
            (let* ((termination (if (= start 0) #1='(NIL . NIL) (nthcdr (1- start) list)))
                   (init (if (<= (or end (length list)) start)
                             termination
                             (if end (last list (- (length list) (1- end))) (last list)))))
              (values init termination t))
            (cond
              ((not end) (values (nthcdr start list) nil nil))
              (t (let ((st (nthcdr start list)))
                   (values st (nthcdr (- end start) st) nil)))))
      (values iterator limit from-end
              (if from-end
                  (lambda (sequence iterator from-end)
                    (declare (ignore sequence from-end))
                    (if (eq iterator list)
                        #1#
                        (do* ((cdr list (cdr cdr)))
                             ((eq (cdr cdr) iterator) cdr))))
                  (lambda (sequence iterator from-end)
                    (declare (ignore sequence from-end))
                    (cdr iterator)))
              (lambda (sequence iterator limit from-end)
                (declare (ignore sequence from-end))
                (eq iterator limit))
              (lambda (sequence iterator)
                (declare (ignore sequence))
                (car iterator))
              (lambda (new-value sequence iterator)
                (declare (ignore sequence))
                (setf (car iterator) new-value))
              (lambda (sequence iterator)
                (declare (ignore sequence))
                (loop for cdr on list
                      for i from 0
                      when (eq cdr iterator)
                      return i))
              (lambda (sequence iterator)
                (declare (ignore sequence))
                iterator)))))

(defmethod for:make-iterator ((container list-container) &rest args)
  (apply #'for:make-iterator (%objects container) args))
