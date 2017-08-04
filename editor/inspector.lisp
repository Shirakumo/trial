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
    (pathname
     (q+:show (make-instance 'pathname-inspector :object object)))
    (function
     (q+:show (make-instance 'function-inspector :object object)))
    (cons
     (q+:show
      (if (or (consp (cdr object)) (null (cdr object)))
          (make-instance 'list-inspector :object object)
          (make-instance 'cons-inspector :object object))))
    ((or standard-object structure-object condition)
     (q+:show (make-instance 'object-inspector :object object)))))

(defun %r (thing search replace)
  (cl-ppcre:regex-replace-all search thing replace))

(defun safe-princ (value &optional (length 50))
  (handler-case (let ((string (princ-to-string value)))
                  (%r (if (and length (< length (length string)))
                          (with-output-to-string (out)
                            (write-sequence string out :end 50)
                            (write-string "..." out))
                          string)
                      "&" "&&"))
    (error (err)
      (declare (ignore err))
      (format NIL "<ERROR DURING PRINTING>"))))

(defun safe-prin1 (value &optional (length 50))
  (handler-case (let ((string (prin1-to-string value)))
                  (%r (if (and length (< length (length string)))
                          (with-output-to-string (out)
                            (write-sequence string out :end 50)
                            (write-string "..." out))
                          string)
                      "&" "&&"))
    (error (err)
      (declare (ignore err))
      (format NIL "<ERROR DURING PRINTING>"))))

(defun safe-input-value (&optional parent)
  (let ((new (q+:qinputdialog-get-text
              parent "Set a new value"
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

(define-widget inspector (QWidget)
  ((object :initarg :object :accessor object)
   (refresh-thread :initform NIL :accessor refresh-thread))
  (:default-initargs :object (error "OBJECT required.")))

(define-object signal-carrier (QObject)
  ((object :initarg :object :accessor object)))

(define-signal (inspector add-item) (qobject))

(defmethod refresh-background :around ((inspector inspector))
  (let ((thread (refresh-thread inspector)))
    (when (and thread (bt:thread-alive-p thread))
      (bt:interrupt-thread thread (lambda () (abort))))
    (setf (refresh-thread inspector)
          (bt:make-thread (lambda ()
                            (with-simple-restart (abort "Exit refreshing.")
                              (call-next-method)))))))

(defmethod add-item (item (inspector inspector))
  (signal! inspector (add-item qobject)
           (make-instance 'signal-carrier :object item)))
