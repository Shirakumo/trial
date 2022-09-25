#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;;;; A lock-free single-reader multiple-writer queue that can resize.
;; Note: we spin in several locations, but this is preferable to locking,
;;       as we don't expect a lot of contention, and would vastly prefer
;;       the thread not be suspended at any time.

(defstruct (queue
            (:constructor %make-queue (elements reallocating)))
  (elements NIL :type simple-vector)
  (write-index 0 :type (unsigned-byte 32))
  (allocation-index 0 :type (unsigned-byte 32))
  (read-index 0 :type (unsigned-byte 32))
  (reallocating NIL :type simple-vector))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type T :identity T)
    (format stream "READ-INDEX: ~d WRITE-INDEX: ~d CAPACITY: ~d"
            (queue-read-index queue)
            (queue-write-index queue)
            (length (queue-elements queue)))))

(defun make-queue (&optional (initial-size 1024))
  (let ((elements (make-array initial-size :initial-element NIL)))
    (%make-queue elements elements)))

(defun queue-push (element queue)
  (declare (optimize speed (safety 1)))
  (let ((write (queue-allocation-index queue)))
    ;; 1. allocate the space in the elements vector
    (loop until (atomics:cas (queue-allocation-index queue) write (1+ write))
          do (setf write (queue-allocation-index queue)))
    ;; 2. check if we actually have space in the vector
    (let ((elements (queue-elements queue)))
      (when (<= (length elements) write)
        ;; 2.1 allocate a new vector and try to claim it in the back buffer
        (let ((new (make-array (* 2 (length elements)) :initial-element NIL)))
          (when (atomics:cas (queue-reallocating queue) elements new)
            ;; 2.2 keep copying over until the old buffer is full
            (loop for start = 0 then index
                  for index = (queue-write-index queue)
                  do (replace new elements :start2 start :end2 index)
                  until (= (length elements) index))
            ;; 2.3 publish the new queue vector
            (setf (queue-elements queue) new))
          (setf elements (queue-reallocating queue))))
      ;; 3. fill the element until it's actually public
      (loop
       (setf (aref elements write) element)
       (if (eq elements (queue-elements queue))
           (return)
           (setf elements (queue-reallocating queue))))
      ;; 4. commit the write by bumping the index
      (loop until (atomics:cas (queue-write-index queue) write (1+ write))))))

(defun queue-discard (queue)
  (declare (optimize speed (safety 1)))
  (let ((elements (queue-elements queue))
        (read (queue-read-index queue))
        (write (queue-write-index queue)))
    ;; We simply clear out the elements with NIL. We can't set them to be tentative
    ;; as that would freeze up the read loop. Note that this will also not reset the
    ;; read or write indices, a read loop needs to happen first for those to be reset.
    ;; We may also "miss out" on elements that are written to the queue concurrently,
    ;; but that shouldn't be a problem, as we only guarantee to clear out elements
    ;; set prior to the call of QUEUE-DISCARD.
    (loop for i from read below write
          do (setf (aref elements i) NIL))
    queue))

(defun map-queue (function queue)
  (declare (optimize speed (safety 1)))
  (let ((elements (queue-elements queue))
        (read (queue-read-index queue))
        (write (queue-write-index queue))
        (function (etypecase function
                    (function function)
                    (symbol (fdefinition function)))))
    (loop (cond ((<= write read)
                 ;; We reached beyond the write head. Try to reset the allocation head
                 ;; and exit processing
                 (cond ((or (< (queue-allocation-index queue) write)
                            (atomics:cas (queue-allocation-index queue) write 0))
                        (cond ((atomics:cas (queue-write-index queue) write 0)
                               (setf (queue-read-index queue) 0)
                               (return))
                              (T
                               (setf write (queue-write-index queue)))))
                       (T
                        ;; The write head changed, update it
                        (setf elements (queue-elements queue))
                        (setf write (queue-write-index queue)))))
                ((< read (length elements))
                 ;; We're within the bounds, read an element
                 (let ((element (aref elements read)))
                   (cond ((null element)
                          ;; The current element has been discarded, skip it
                          (incf read))
                         (T
                          (setf (queue-read-index queue) (1+ read))
                          ;; We got a proper element, increase the read index and null the element
                          (setf (aref elements read) NIL)
                          ;; Call the function with the element
                          (funcall function element)
                          ;; Reset the read index as we might have processed more within the function
                          (setf read (queue-read-index queue))
                          (when (= 0 read) (return))))))
                (T
                 (setf elements (queue-elements queue)))))
    queue))

(defun %queue-test-writers (&key (threads 10) (writes 100))
  (let ((queue (make-queue))
        (handles ())
        (start NIL))
    (unwind-protect
         (progn
           (dotimes (i threads)
             (let ((tid i))
               (push (bt:make-thread  (lambda ()
                                        (loop until start)
                                        (dotimes (i writes)
                                          (queue-push (cons tid i) queue))))
                     handles)))
           (setf start T)
           (dolist (handle handles)
             (bt:join-thread handle)))
      (dolist (handle handles)
        (ignore-errors (bt:destroy-thread handle))))
    (let ((found (make-hash-table :test 'eql)))
      (loop for (tid . i) across (queue-elements queue)
            do (when tid (push i (gethash tid found))))
      (format T "~&Checking ~a~%" queue)
      (dotimes (tid threads)
        (let ((entries (gethash tid found)))
          (dotimes (i writes)
            (assert (find i entries))))))))

(defun %queue-test-writers-reader (&key (threads 10) (writes 100))
  (let ((queue (make-queue))
        (handles ())
        (start NIL))
    (unwind-protect
         (progn
           (dotimes (i threads)
             (let ((tid i))
               (push (bt:make-thread  (lambda ()
                                        (loop until start)
                                        (dotimes (i writes)
                                          (queue-push (cons tid i) queue))))
                     handles)))
           (setf start T)
           (let ((found (make-hash-table :test 'eql)))
             (loop (map-queue (lambda (el)
                                (destructuring-bind (tid . i) el
                                  (when tid (push i (gethash tid found)))))
                              queue)
                   (when (loop for thread in handles never (bt:thread-alive-p thread))
                     (return)))
             (format T "~&Checking ~a~%" queue)
             (format T "~&Size overhead ~d" (- (length (queue-elements queue)) (* threads writes)))
             (dotimes (tid threads)
               (let ((entries (gethash tid found)))
                 (dotimes (i writes)
                   (assert (find i entries)))))))
      (dolist (handle handles)
        (bt:join-thread handle)))))
