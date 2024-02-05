(in-package #:org.shirakumo.fraf.trial.gog)

(defclass achievement-api (trial:achievement-api)
  ())

(defmethod trial:load-achievement-data ((api achievement-api))
  (unless (gog:initialized-p)
    (error "Not connected to GOG.")))

(trial:define-handler (achievement-api trial:achievement-unlocked :after) ((achievement trial:achievement))
  (setf (gog:achieved-p (string-downcase (trial:name achievement))) T))

(trial:define-handler (achievement-api trial:achievement-relocked :after) ((achievement trial:achievement))
  (setf (gog:achieved-p (string-downcase (trial:name achievement))) NIL))

(defmethod trial:notifications-display-p ((api achievement-api))
  (gog:initialized-p))

(defmethod (setf trial:notifications-display-p) (value (api achievement-api))
  NIL)

(pushnew (make-instance 'achievement-api) trial:*achievement-apis*)

