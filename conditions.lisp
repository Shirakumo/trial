(in-package #:org.shirakumo.fraf.trial)

(defmacro define-simple-condition (name supers slots &optional format &rest args)
  `(define-condition ,name ,supers
     ,(loop for slot in slots
            collect (if (symbolp slot)
                        `(,slot :initarg ,(intern (string slot) "KEYWORD") :initform NIL)
                        slot))
     ,@(when format
         `((:report (lambda (c s) (format s ,format ,@(loop for arg in args
                                                            collect (if (symbolp arg)
                                                                        `(slot-value c ',arg)
                                                                        arg)))))))))

(define-simple-condition trial-error (error) ())
(define-simple-condition trial-warning (warning) ())

(define-simple-condition thread-did-not-exit (trial-error)
  (thread timeout) "Thread~%  ~a~%did not exit after ~ds." thread timeout)

(define-simple-condition resource-not-allocated (trial-error)
  (resource) "The resource~%  ~s~%is required to be allocated, but was not yet." resource)

(define-simple-condition context-creation-error (trial-error)
  (message context) "Failed to create an OpenGL context~@[:~%~%  ~a~]" message)

(define-simple-condition resource-depended-on (trial-error)
  (resource dependents) "The resource~%  ~a~%cannot be unstaged as it is depended on by~{~%  ~a~}" resource dependents)

(define-simple-condition shader-compilation-error (trial-error)
  (shader log) "Failed to compile ~a:~%  ~a~%~a" shader log (format-with-line-numbers (shader-source (slot-value c 'shader))))

(define-simple-condition initarg-not-supplied (trial-error)
  (initarg) "The initarg~%  ~s~%is required but was not supplied." initarg)

(defun arg! (argument)
  (error 'initarg-not-supplied :initarg argument))

(define-simple-condition asset-input-file-missing (trial-warning)
  (asset file) "Input file~%  ~a~%for asset~%  ~s~%does not exist."
  (pathname-utils:native-namestring (pathname-utils:normalize-pathname (slot-value c 'file) :up-as-back T)) asset)

(define-simple-condition not-implemented (trial-error)
  (file index) "This function has not been implemented yet.

Please help the development of Trial by implementing it ~@[in

  ~a~@[:~a~]

~]and submitting a patch to

   https://shirakumo.org/projects/trial

Thanks!" file index)

(defmacro implement! ()
  `(error 'not-implemented :file ,(or *compile-file-truename* *load-truename*)))
