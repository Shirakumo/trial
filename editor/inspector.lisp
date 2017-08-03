#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(defun inspect (object)
  (typecase object
    ((or (eql NIL) (eql T)))
    (hash-table
     (q+:show (make-instance 'hash-table-inspector :object object)))
    (array
     (q+:show (make-instance 'array-inspector :object object)))
    (symbol
     (q+:show (make-instance 'symbol-inspector :object object)))
    (package
     (q+:show (make-instance 'package-inspector :object object)))
    (cons
     (q+:show
      (if (or (consp (cdr object)) (null (cdr object)))
          (make-instance 'list-inspector :object object)
          (make-instance 'cons-inspector :object object))))
    ((or standard-object structure-object condition)
     (q+:show (make-instance 'object-inspector :object object)))))

(defun safe-princ (value)
  (handler-case (let ((string (princ-to-string value)))
                  (if (< 50 (length string))
                      (with-output-to-string (out)
                        (write-sequence string out :end 50)
                        (write-string "..." out))
                      string))
    (error (err)
      (declare (ignore err))
      (format NIL "<ERROR DURING PRINTING>"))))

(defun safe-input-value (&optional parent)
  (let ((new (q+:qinputdialog-get-text
              parent (format NIL "Set a new value")
              "Enter a new value. What you enter will be evaluated.")))
    (block NIL
      (when (string= new "")
        (return (values NIL NIL)))
      (let ((expr (handler-case (read-from-string new)
                    (error (err)
                      (q+:qmessagebox-critical
                       parent "Error during reading of new value"
                       (princ-to-string err))
                      (return (values NIL NIL))))))
        (handler-case (values (eval expr) T)
          (error (err)
            (q+:qmessagebox-critical
             parent "Error during evaluation of new value"
             (princ-to-string err))
            (return (values NIL NIL))))))))
