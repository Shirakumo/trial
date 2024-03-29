(defpackage #:org.shirakumo.fraf.trial.gog
  (:use #:cl)
  (:export
   #:main
   #:gog-required-p)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:gog #:org.shirakumo.fraf.gog-galaxy)
   (#:gog* #:org.shirakumo.fraf.gog-galaxy.cffi)
   (#:v #:org.shirakumo.verbose)))
(in-package #:org.shirakumo.fraf.trial.gog)

(setf trial::*open-in-browser-hook*
      (lambda (url)
        (when (and (gog:initialized-p)
                   (eql :initialized (gog:overlay-state T)))
          (gog:show-webpage T url)
          T)))

(defclass main (trial:main)
  ((gog-required-p :initform NIL :initarg :require-gog :accessor gog-required-p)))

(defmethod initialize-instance ((main main) &key gog-client-id gog-client-secret)
  (call-next-method)
  (handler-bind ((error
                   (lambda (e)
                     (v:severe :trial.steam "Failed to initialise GOG Galaxy API: ~a" e)
                     (v:debug :trial.steam e)
                     (when (deploy:deployed-p)
                       (cond ((gog-required-p main))
                             (T (invoke-restart 'ignore)))))))
    (when (or (gog-required-p main)
              (deploy:deployed-p))
      (with-simple-restart (ignore "Ignore the GOG failure.")
        (v:info :trial.steam "Initialising GOG Galaxy~@[ for ~a ~a~]" gog-client-id gog-client-secret)
        (or (gog:init gog-client-id gog-client-secret)
            (error "Failed to initialise"))))))

(defmethod trial:finalize :after ((main main))
  (gog:shutdown))

(defmethod trial:username ((main main))
  (or (when (and (gog:initialized-p) (gog:signed-in-p T))
        (ignore-errors (gog:persona-name T)))
      (call-next-method)))

(defmethod trial:poll-input :after ((main main))
  (when (gog:initialized-p)
    (gog:process-data)))

#-(or windows darwin)
(trial::dont-deploy
 org.shirakumo.fraf.gog-galaxy.cffi:galaxy
 org.shirakumo.fraf.gog-galaxy.cffi:galaxy-c)
