#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(trial:define-pool icons
  :base '(:trial-editor "icons"))

(defvar *icons* (make-hash-table :test 'eql))

(defun icon (name)
  (let ((icon (gethash name *icons*)))
    (etypecase icon
      (qobject
       icon)
      ((or pathname string)
       (setf (icon name) (q+:make-qicon (uiop:native-namestring (trial:pool-path 'icons icon)))))
      (null
       (error "No icon ~s available." name)))))

(defun (setf icon) (icon name)
  (setf (gethash name *icons*) icon))

(defun remove-icon (name)
  (remhash name *icons*))

(defmacro define-icon (name path)
  (unless (probe-file (trial:pool-path 'icons path))
    (warn "Icon could not be found."))
  `(setf (icon ',name) ,path))

(define-widget inline-button (QPushButton)
  ())

(defmethod initialize-instance :after ((button inline-button) &key icon tooltip)
  (setf (q+:size-policy button) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.maximum)))
  (setf (q+:fixed-size button) (q+:make-qsize 30 30))
  (setf (q+:icon-size button) (q+:make-qsize 16 16))
  (setf (q+:icon button) (icon icon))
  (when tooltip (setf (q+:tool-tip button) tooltip)))

(define-icon :add "plus 1512.svg")
(define-icon :adjust "arrow-outside 262.svg")
(define-icon :refresh "arrow-repeat 235.svg")
(define-icon :set "arrow-in-left 387.svg")
(define-icon :remove "delete 1487.svg")
(define-icon :compile "settings 1491.svg")
(define-icon :clear "directory-missing 1628.svg")
(define-icon :reinitialize "arrow-repeat 237.svg")
(define-icon :finalize "file-missing 1689.svg")
(define-icon :rename "edit-text-bar 1372.svg")
(define-icon :use "arrow-in-down 345.svg")
(define-icon :export "arrow-right 268.svg")
(define-icon :pause "pause 1006.svg")
(define-icon :play "play 1000.svg")
(define-icon :inspect "search-right 1507.svg")
(define-icon :list "list 1496.svg")
(define-icon :close "close 1511.svg")
