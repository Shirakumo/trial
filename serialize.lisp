#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-global +type-serialize-info+ (make-hash-table :test 'eql))

(defun serialize-info (type)
  (gethash type +type-serialize-info+))

(defstruct (serialize-info
            (:constructor make-serialize-info (type id reader writer)))
  (type NIL :type symbol)
  (id 0 :type (unsigned-byte 16))
  (reader NIL :type (function (stream) T))
  (writer NIL :type (function (T stream) T)))

(defmacro define-type-serializer (type reader writer)
  `(let ((struct (or (gethash ',type +type-serialize-info+))))
     (unless struct
       (setf (gethash ',type +type-serialize-info+)
             (make-serialize-info ',type (hash-table-count +type-serialize-info+)
                                  (lambda (s) (declare (ignore s)))
                                  (lambda (v s) (declare (ignore v s)))))
       (setf struct (gethash ',type +type-serialize-info+))
       (setf (gethash (serialize-info-id struct) +type-serialize-info+) struct))
     (setf (serialize-info-reader struct) ,reader)
     (setf (serialize-info-writer struct) ,writer)
     ',type))

(defmacro define-object-type-serializer (type constructor &body slots)
  `(define-type-serializer ,type
       (lambda (stream)
         (,constructor ,@(loop for (slot type) in slots
                               collect (kw slot)
                               collect `(funcall (load-time-value (serialize-info-reader (gethash ',type +type-serialize-info+))) stream))))
     (lambda (event stream)
       ,@(loop for (slot type) in slots
               collect `(funcall (load-time-value (serialize-info-writer (gethash ',type +type-serialize-info+))) (,slot event) stream)))))

(defun serialize-as (type value stream)
  (let ((struct (gethash type +type-serialize-info+)))
    (funcall (serialize-info-writer struct) value stream)))

(defun deserialize-as (type stream)
  (let ((struct (gethash type +type-serialize-info+)))
    (funcall (serialize-info-reader struct) stream)))

(define-type-serializer integer
  #'nibbles:read-sb64/le
  #'nibbles:write-sb64/le)

(define-type-serializer single-float
  #'nibbles:read-ieee-single/le
  #'nibbles:write-ieee-single/le)

(define-type-serializer double-float
  #'nibbles:read-ieee-double/le
  #'nibbles:write-ieee-double/le)

(define-type-serializer vec2
    (lambda (s) (vec2 (nibbles:read-ieee-single/le s)
                      (nibbles:read-ieee-single/le s)))
  (lambda (v s)
    (nibbles:write-ieee-single/le (vx2 v) s)
    (nibbles:write-ieee-single/le (vy2 v) s)))

(define-type-serializer vec3
    (lambda (s) (vec3 (nibbles:read-ieee-single/le s)
                      (nibbles:read-ieee-single/le s)
                      (nibbles:read-ieee-single/le s)))
  (lambda (v s)
    (nibbles:write-ieee-single/le (vx3 v) s)
    (nibbles:write-ieee-single/le (vy3 v) s)
    (nibbles:write-ieee-single/le (vz3 v) s)))

(define-type-serializer vec4
    (lambda (s) (vec4 (nibbles:read-ieee-single/le s)
                      (nibbles:read-ieee-single/le s)
                      (nibbles:read-ieee-single/le s)
                      (nibbles:read-ieee-single/le s)))
  (lambda (v s)
    (nibbles:write-ieee-single/le (vx4 v) s)
    (nibbles:write-ieee-single/le (vy4 v) s)
    (nibbles:write-ieee-single/le (vz4 v) s)
    (nibbles:write-ieee-single/le (vw4 v) s)))

(define-type-serializer null
    (lambda (s) (declare (ignore s)) NIL)
  (lambda (v s) (declare (ignore v s)) NIL))

(define-type-serializer gamepad:device
    ;; FIXME: Try to restore a more accurate representation of the used device.
    (lambda (s)
      (declare (ignore s))
      (or (block NIL (gamepad:call-with-devices (lambda (d) (return d))))
          (load-time-value (allocate-instance (find-class 'gamepad:device)))))
  (lambda (v s)
    (declare (ignore v s))
    NIL))

(define-type-serializer symbol
    (lambda (s)
      (let ((pkg (make-string (read-byte s) :element-type 'base-char))
            (sym (make-string (read-byte s) :element-type 'base-char)))
        (dotimes (i (length pkg))
          (setf (char pkg i) (code-char (read-byte s))))
        (dotimes (i (length sym))
          (setf (char sym i) (code-char (read-byte s))))
        (intern sym pkg)))
  (lambda (v s)
    (let ((pkg (package-name (symbol-package v)))
          (sym (symbol-name v)))
      (write-byte (length pkg) s)
      (write-byte (length sym) s)
      (loop for char across pkg do (write-byte (char-code char) s))
      (loop for char across sym do (write-byte (char-code char) s)))))

(define-type-serializer keyword
    (lambda (s)
      (let ((sym (make-string (read-byte s) :element-type 'base-char)))
        (dotimes (i (length sym))
          (setf (char sym i) (code-char (read-byte s))))
        (intern sym "KEYWORD")))
  (lambda (v s)
    (let ((sym (symbol-name v)))
      (write-byte (length sym) s)
      (loop for char across sym do (write-byte (char-code char) s)))))

(define-type-serializer boolean
    (lambda (s)
      (< 0 (read-byte s)))
  (lambda (v s)
    (write-byte (if v 1 0) s)))

(define-type-serializer string
    (lambda (s)
      (let ((string (make-string (nibbles:read-ub32/le s))))
        (dotimes (i (length string) string)
          (setf (char string i) (code-char (nibbles:read-ub32/le s))))))
  (lambda (v s)
    (nibbles:write-ub32/le (length v) s)
    (loop for char across v do (nibbles:write-ub32/le (char-code char) s))))

(define-type-serializer T
    (lambda (s)
      (funcall (serialize-info-reader (gethash (read-byte s) +type-serialize-info+)) s))
  (lambda (v s)
    (let ((struct (gethash (type-of v) +type-serialize-info+)))
      (write-byte (serialize-info-id struct) s)
      (funcall (serialize-info-writer struct) v s))))
