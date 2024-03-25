(in-package #:org.shirakumo.fraf.trial.steam)

(defclass achievement-api (trial:achievement-api)
  ())

(defmethod trial:load-achievement-data ((api achievement-api))
  (unless steam::*steamworks*
    (error "Not connected to steam."))
  (dolist (achievement (steam:list-achievements (steam:interface 'steam:steamuserstats T)))
    (handler-case
        (setf (slot-value (trial:achievement (steam:handle achievement)) 'trial:active-p) (steam:achieved-p achievement))
      (error ()
        (v:warn :trial.achievements "Steam achievement ~s not present locally!" (steam:handle achievement))))))

(trial:define-handler (achievement-api trial:achievement-unlocked :after) ((achievement trial:achievement))
  (setf (steam:achieved-p (string-downcase (trial:name achievement)) :sync T) T))

(trial:define-handler (achievement-api trial:achievement-relocked :after) ((achievement trial:achievement))
  (setf (steam:achieved-p (string-downcase (trial:name achievement)) :sync T) NIL))

(defmethod trial:notifications-display-p ((api achievement-api))
  steam::*steamworks*)

(defmethod (setf trial:notifications-display-p) (value (api achievement-api))
  NIL)

(pushnew (make-instance 'achievement-api) trial:*achievement-apis*)
