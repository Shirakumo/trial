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
            (:constructor make-queue ()))
  (elements (make-array 1024 :initial-element :tentative) :type simple-vector)
  (write-index 0 :type (unsigned-byte 32))
  (allocation-index 0 :type (unsigned-byte 32))
  (read-index 0 :type (unsigned-byte 32))
  (reallocating 1024 :type (or (unsigned-byte 32) simple-vector)))

(defun queue-push (element queue)
  (declare (optimize debug))
  (let ((write (queue-allocation-index queue)))
    ;; First: allocate the space in the elements vector
    (loop until (atomics:cas (queue-allocation-index queue) write (1+ write))
          do (setf write (queue-allocation-index queue)))
    ;; Second: check if we actually have space in the vector
    (let ((elements (queue-elements queue)))
      (when (<= (length elements) write)
        (let ((new (make-array (* 2 (length elements)) :initial-element NIL)))
          (cond ((atomics:cas (queue-reallocating queue) (length elements) new)
                 ;; Copy over elements that have been committed so far
                 (replace new elements :end2 (1- (queue-write-index queue)))
                 (setf elements new)
                 (setf (queue-elements queue) new)
                 (setf (queue-reallocating queue) (length elements)))
                (T
                 ;; Someone else got it, write into the new array instead.
                 (setf elements (queue-reallocating queue))
                 (unless (vectorp elements)
                   (setf elements (queue-elements queue)))))))
      ;; Third: we now have a vector we can actually write to, so do that.
      (setf (aref elements write) element)
      ;; Fourth: now wait until we're done reallocating
      (loop while (vectorp (queue-reallocating queue)))
      ;; Fifth: now we can actually "commit" the write
      (loop until (atomics:cas (queue-write-index queue) write (1+ write))))))

(defun queue-discard (queue)
  (let ((elements (queue-elements queue))
        (read (queue-read-index queue))
        (write (queue-write-index queue)))
    ;; We simply clear out the elements with NIL. We can't set them to be tentative
    ;; as that would freeze up the read loop. Note that this will also not reset the
    ;; read or write indices, a read loop needs to happen first for those to be reset.
    ;; We may also "miss out" on elements that are written to the queue concurrently,
    ;; but that shouldn't be a problem, as we only guarantee to clear out elements
    ;; set prior to the call of QUEUE-DISCARD.
    (loop for i = read then (1+ i)
          while (< i write)
          do (setf (aref elements i) NIL))
    queue))

(defun map-queue (function queue)
  (declare (optimize debug))
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
      (map-queue (lambda (el)
                   (destructuring-bind (tid . i) el
                     (push i (gethash tid found))))
                 queue)
      (dotimes (tid threads)
        (let ((entries (gethash tid found)))
          (dotimes (i writes)
            (assert (find i entries))))))))

#++
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
           (let ((found (make-array (* threads writes) :fill-pointer 0)))
             (loop (map-queue (lambda (el) (vector-push el found)) queue)
                   (when (= (length found) (* threads writes))
                     (return)))))
      (dolist (handle handles)
        (bt:join-thread handle)))))
