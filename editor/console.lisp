#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:trial-editor-console
  (:nicknames #:org.shirakumo.fraf.trial.editor.console)
  (:use #:cl+qt)
  (:export #:console))
(in-package #:org.shirakumo.fraf.trial.editor.console)
(in-readtable :qtools)

(define-widget console (QTextEdit qui:repl)
  ((main :initarg :main :accessor main))
  (:default-initargs
    :main (error "MAIN required.")))

(defmethod qui::repl-eval-inner ((console console) form)
  (eval-in-scene (scene (main console))
                 form `((*terminal-io* ,*terminal-io*)
                        (*standard-output* ,*standard-output*)
                        (*error-output* ,*error-output*)
                        (*query-io* ,*query-io*)
                        (*trace-output* ,*trace-output*)
                        (*debug-io* ,*debug-io*)
                        (*package* ,*package*)
                        (+ ,+) (++ ,++) (+++ ,+++)
                        (/ ,/) (// ,//) (/// ,///)
                        (* ,*) (** ,**) (*** ,***))))
