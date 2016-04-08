#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:trial-editor-console
  (:nicknames #:org.shirakumo.fraf.trial.editor.console)
  (:use #:cl+qt #:trivial-gray-streams)
  (:export #:console))
(in-package #:org.shirakumo.fraf.trial.editor.console)
(in-readtable :qtools)

(define-widget console (QTextEdit)
  ((main :initarg :main :accessor main)
   (input-begin :initform 0 :accessor input-begin)
   (output-stream :initarg :output-stream :accessor output-stream)
   (error-stream :initarg :error-stream :accessor error-stream)
   (package :initarg :package :accessor current-package))
  (:default-initargs
    :main (error "MAIN required.")
    :package (find-package '#:trial)))

(define-initializer (console setup)
  (unless (slot-boundp console 'output-stream)
    (setf output-stream (make-instance 'console-output-stream :console console :color "orange")))
  (unless (slot-boundp console 'error-stream)
    (setf error-stream (make-instance 'console-output-stream :console console :color "red")))
  (output-prefix console))

(define-subwidget (console font) (q+:make-qfont "Monospace" 10)
  (setf (q+:style-hint font) (q+:qfont.type-writer))
  (setf (q+:font console) font))

(define-subwidget (console palette) (q+:make-qpalette)
  (let ((background (q+:color palette (q+:qpalette.window)))
        (foreground (q+:color palette (q+:qpalette.base))))
    (setf (q+:color palette (q+:qpalette.base)) background)
    (setf (q+:color palette (q+:qpalette.text)) foreground)
    (setf (q+:palette console) palette)))

(define-signal (console return-pressed) ())

(define-override (console key-press-event) (ev)
  (cond ;; Signal return pressed.
    ((or (= (q+:key ev) (q+:qt.key_enter))
         (= (q+:key ev) (q+:qt.key_return)))
     (call-next-qmethod)
     (signal! console (return-pressed)))
    ;; Catch escape to forbid removing text before input.
    ((= (q+:key ev) (q+:qt.key_backspace))
     (when (< (input-begin console) (cursor console))
       (call-next-qmethod)))
    ;; Delegate standard.
    (T
     (call-next-qmethod))))

;; FIXME: handle + ++ +++ / // /// * ** ***
(define-slot (console eval) ()
  (declare (connected console (return-pressed)))
  (let ((*terminal-io* (output-stream console))
        (*standard-input* *standard-input*)
        (*standard-output* (output-stream console))
        (*error-output* (error-stream console))
        (*query-io* (output-stream console))
        (*trace-output* (output-stream console))
        (*debug-io* (output-stream console))
        (*package* (current-package console)))
    (handler-case
        (let* ((form (read-from-string (input console)))
               (result (trial::eval-in-scene (trial::scene main) form)))
          (output-result console (multiple-value-list result)))
      (error (err)
        (output-line console)
        (output-error console err))))
  (output-prefix console))

;; Helper functions to i/o text.
(defun cursor (console)
  (q+:position (q+:text-cursor console)))

(defun output (console format-string &rest args)
  (q+:move-cursor console (q+:qtextcursor.end))
  (q+:insert-html console (apply #'format NIL format-string args))
  (q+:move-cursor console (q+:qtextcursor.end)))

(defun escape (text)
  (flet ((r (text find replace)
           (cl-ppcre:regex-replace-all find text replace)))
    (r (r (r text "&" "&amp;") "<" "&lt;") ">" "&gt;")))

(defun output-line (console)
  (output console "<br />"))

(defun output-form (console form)
  (output console "~a<br />" (escape (write-to-string form))))

(defun output-colored (console color format-string &rest args)
  (output console "<span style=\"color:~a;\">~a</span>" color (apply #'format NIL format-string args)))

(defun output-prefix (console)
  (output-colored console "red" "~a&gt;" (escape (package-name (current-package console))))
  (output console "&nbsp;")
  (setf (input-begin console) (cursor console)))

(defun output-comment (console format-string &rest args)
  (output-colored console "gray" "; ~a<br />" (apply #'format NIL format-string args)))

(defun output-result (console values)
  (if values
      (dolist (value values)
        (output-colored console "cyan" "~a<br />" (escape (write-to-string value))))
      (output-comment console "No values.")))

(defun output-error (console error)
  (output-comment console "<span style=\"color:red;\">Error:</span> ~a" (escape (princ-to-string error)))
  (output-comment console "[Condition of type ~a]" (escape (princ-to-string (type-of error)))))

(defun text (console)
  (q+:to-plain-text console))

(defun input (console)
  (assert (< (input-begin console) (cursor console))
          () "No input at this point.")
  (subseq (text console) (input-begin console) (cursor console)))

(defclass console-output-stream (fundamental-character-output-stream trivial-gray-stream-mixin)
  ((console :initarg :console :accessor console)
   (buffer :initform (make-string-output-stream) :accessor buffer)
   (color :initarg :color :accessor color))
  (:default-initargs
   :console (error "CONSOLE required.")
   :color "orange"))

(defmethod stream-clear-output ((stream console-output-stream))
  (setf (buffer stream) (make-string-output-stream)))

(defmethod stream-finish-output ((stream console-output-stream))
  (let ((string (get-output-stream-string (buffer stream))))
    (output-colored (console stream) (color stream) "~a" (cl-ppcre:regex-replace-all "\\n" (escape string) "<br />")))
  (clear-output stream))

(defmethod stream-force-output ((stream console-output-stream))
  (stream-finish-output stream))

(defmethod stream-write-string ((stream console-output-stream) string &optional (start 0) end)
  (write-string string (buffer stream) :start start :end end)
  (stream-finish-output stream))

(defmethod stream-write-char ((stream console-output-stream) char)
  (write-string (string char) stream))

(defmethod stream-terpri ((stream console-output-stream))
  (write-char #\Newline stream))
