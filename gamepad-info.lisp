#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *gamepad-info-table* (make-hash-table :test 'eql))

(defstruct gamepad-info
  (id 0 :type (unsigned-byte 32))
  (name NIL :type symbol)
  (axis-table (make-hash-table :test 'eql) :type hash-table)
  (button-table (make-hash-table :test 'eql) :type hash-table))

(defmethod print-object ((info gamepad-info) stream)
  (print-unreadable-object (info stream :type T)
    (format stream "~a" (gamepad-info-name info))))

(defun encode-gamepad-id (vendor product)
  (let ((i (ash vendor 16)))
    (setf (ldb (byte 16 0) i) product)
    i))

(defun decode-gamepad-id (id)
  (values (ash id -16)
          (logand id (1- (expt 2 16)))))

(defun ensure-gamepad-id (gamepad-ish)
  (etypecase gamepad-ish
    ((unsigned-byte 32)
     gamepad-ish)
    (cons
     (encode-gamepad-id (car gamepad-ish) (cdr gamepad-ish)))
    (cffi:foreign-pointer
     (encode-gamepad-id (cl-gamepad:vendor gamepad-ish)
                        (cl-gamepad:product gamepad-ish)))
    (gamepad-info
     (gamepad-info-id gamepad-ish))
    (symbol
     gamepad-ish)))

(defun gamepad-info (gamepad-ish)
  (gethash (ensure-gamepad-id gamepad-ish) *gamepad-info-table*))

(defun (setf gamepad-info) (gamepad-info gamepad-ish)
  (setf (gethash (ensure-gamepad-id gamepad-ish) *gamepad-info-table*)
        gamepad-info))

(defun remove-gamepad-info (gamepad-ish)
  (let ((gamepad-info (gamepad-info gamepad-ish)))
    (remhash (gamepad-info-name gamepad-info) *gamepad-info-table*)
    (remhash (ensure-gamepad-id gamepad-ish) *gamepad-info-table*)))

(defun gamepad-info-options (gamepad-ish)
  (let ((info (gamepad-info gamepad-ish)))
    `((:axes ,@(loop for id being the hash-keys of (gamepad-info-axis-table info)
                     for (name dir) being the hash-values of (gamepad-info-axis-table info)
                     collect (list id name dir)))
      (:buttons ,@(loop for id being the hash-keys of (gamepad-info-button-table info)
                        for name being the hash-values of (gamepad-info-button-table info)
                        collect (list id name))))))

(defun (setf gamepad-info-options) (options gamepad-ish)
  (let ((info (gamepad-info gamepad-ish)))
    (loop for (id name dir) in (cdr (assoc :axes options))
          do (setf (gethash id (gamepad-info-axis-table info)) (list name (or dir 1))))
    (loop for (id name) in (cdr (assoc :buttons options))
          do (setf (gethash id (gamepad-info-button-table info)) name))
    options))

(defmacro define-gamepad (name (manufacturer id &key inherit) &body options)
  (let ((info (gensym "INFO")))
    `(let ((,info (or (gamepad-info ',name)
                      (setf (gamepad-info ',name) (make-gamepad-info :name ',name)))))
       (setf (gamepad-info-id ,info) (encode-gamepad-id ,manufacturer ,id))
       (setf (gamepad-info (gamepad-info-id ,info)) ,info)
       ,(when inherit
          `(setf (gamepad-info-options ,info) (gamepad-info-options ',inherit)))
       (setf (gamepad-info-options ,info) ',options)
       ',name)))

(macrolet ((define-generic-controller (name (vendor id))
             `(define-gamepad ,name (,vendor ,id)
                (:axes ,@(loop for i from 0 to 255
                               collect `(,i ,(intern (format NIL "AXIS-~a" i) :keyword))))
                (:buttons ,@(loop for i from 0 to 255
                                  collect `(,i ,(intern (format NIL "BUTTON-~a" i) :keyword)))))))
  (define-generic-controller generic (0 0)))

(defun gamepad-button (device button-id)
  (let ((info (or (gamepad-info device)
                  (gamepad-info 'generic))))
    (gethash button-id (gamepad-info-button-table info) :unknown)))

(defun gamepad-axis (device axis-id)
  (let ((info (or (gamepad-info device)
                  (gamepad-info 'generic))))
    (gethash axis-id (gamepad-info-axis-table info) '(:unknown 1))))
