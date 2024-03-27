(in-package #:org.shirakumo.fraf.trial.gog)

(defclass achievement-api (trial:achievement-api)
  ())

(defmethod trial:load-achievement-data ((api achievement-api))
  (unless (and (gog:initialized-p) (gog:signed-in-p T))
    (error "Not connected to GOG.")))

(trial:define-handler (achievement-api trial:achievement-unlocked :after) ((achievement trial:achievement))
  (setf (gog:achieved-p (string-downcase (trial:name achievement))) T)
  (gog:store (gog:interface 'gog:stats)))

(trial:define-handler (achievement-api trial:achievement-relocked :after) ((achievement trial:achievement))
  (setf (gog:achieved-p (string-downcase (trial:name achievement))) NIL)
  (gog:store (gog:interface 'gog:stats)))

(defmethod trial:notifications-display-p ((api achievement-api))
  (and (gog:initialized-p)
       (gog:signed-in-p T)))

(defmethod (setf trial:notifications-display-p) (value (api achievement-api))
  NIL)

(pushnew (make-instance 'achievement-api) trial:*achievement-apis*)

