#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.glfw)

(define-global +none+ (make-symbol "NONE"))

(defun make-event-queue ()
  (let ((queue (make-array 128)))
    (dotimes (i (length queue) queue)
      (setf (svref queue i) (cons +none+ +none+)))))

(defun handle-event-queue (queue handler)
  (declare (type (simple-vector 128) queue))
  (loop for i from 0 below 128
        for cell = (the cons (aref queue i))
        do (when (and (not (eq (car cell) +none+))
                      (not (eq (car cell) T))
                      (not (eq (cdr cell) +none+)))
             (setf (cdr cell) (funcall handler (car cell) (cdr cell)))
             (setf (car cell) T))))

(defun push-event-queue (queue request &optional arg)
  (declare (type (simple-vector 128) queue))
  (check-type request keyword)
  (loop for i = 0 then (mod (1+ i) 128)
        for cell = (the cons (svref queue i))
        do (when (eq (car cell) +none+)
             (when (atomics:cas (car cell) +none+ request)
               (setf (cdr cell) arg)
               (return i)))))

(defun await-event-queue (queue i)
  (declare (type (simple-vector 128) queue))
  (declare (type (integer 0 127) i))
  (loop for cell = (the cons (svref queue i))
        do (cond ((eq (car cell) +none+)
                  (return NIL))
                 ((eq (car cell) T)
                  (let ((result (shiftf (cdr cell) +none+)))
                    (setf (car cell) +none+)
                    (return result)))
                 (T
                  (sleep 0.0001)))))

(defun request-event-queue (queue request &optional arg)
  (await-event-queue queue (push-event-queue queue request arg)))
