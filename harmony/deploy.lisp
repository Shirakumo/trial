(in-package #:org.shirakumo.fraf.trial.harmony)

(defmethod trial:report-on-error ((error mixed:mixed-error))
  (case (mixed:error-code error)
    ((:out-of-memory 1)
     (trial:emessage "Failed to allocate memory. Are you sure you have enough RAM free to run this game?"))
    (T
     (call-next-method))))

