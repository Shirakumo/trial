#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.steam)

(defmethod trial:list-achievements ((main main))
  (if steam::*steamworks*
      (steam:list-achievements (steam:interface 'steam:steamuserstats T))
      (call-next-method)))

(defmethod (setf trial:achievement-state) :before (value (achievement trial:achievement) (main main))
  (when steam::*steamworks*
    (setf (steam:achieved-p (trial:api-name achievement)) value)))

(defmethod trial:achievement-state ((achievement trial:achievement) (main main))
  (if steam::*steamworks*
      (steam:achieved-p (trial:api-name achievement))
      (call-next-method)))

(defmethod trial:name ((achievement steam:achievement))
  (trial:symbol->c-name (steam:handle achievement)))

(defmethod trial:api-name ((achievement steam:achievement))
  (steam:handle achievement))

(defmethod trial:title ((achievement steam:achievement))
  (steam:display-name achievement))

(defmethod trial:description ((achievement steam:achievement))
  (steam:description achievement))

(defmethod trial:unlocked-p ((achievement steam:achievement))
  (steam:achieved-p achievement))

(defmethod (setf trial:unlocked-p) (value (achievement steam:achievement))
  (setf (steam:achieved-p achievement) value))
