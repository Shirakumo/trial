#+quicklisp (ql:quickload '(:swank :cffi) :silent T)
#+(and asdf (not quicklisp)) (asdf:load-system '(:swank :cffi))

#+(and unix sbcl)
(trace cffi:load-foreign-library :condition-after (cffi:foreign-funcall "restore_sbcl_signals" :void))

(swank:create-server :dont-close T)
(loop until swank::*connections* do (sleep 0.1))
(loop while swank::*connections* do (sleep 1.0))
