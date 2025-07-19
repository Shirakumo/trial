(in-package #:org.shirakumo.fraf.trial.particle-studio)

(defmethod serialize ((emitter particle-emitter) (stream stream))
  ())

(defmethod serialize ((fields particle-force-fields) (stream stream))
  (dotimes (i (trial::particle-force-field-count fields))
    (serialize (aref (particle-force-fields fields) i) stream)))

(defmethod serialize ((field particle-force-field) (stream stream))
  (trial::with-gl-slots (particle-force-field type position strength trial::range trial::inv-range normal) field
    (prin1
     (case type
       (0
        `(:type :null))
       (1
        `(:type :point :srength ,strength :position ,position :inv-range ,trial::inv-range))
       (2
        `(:type :direction :srength ,strength :normal ,normal))
       (3
        `(:type :plane :strength ,strength :position ,position :normal ,normal))
       (4
        `(:type :vortex :strength ,strength :position ,position :normal ,normal :inv-range ,trial::inv-range))
       (5
        `(:type :sphere :strength ,strength :position ,position :range ,trial::range))
       (6
        `(:type :planet :strength ,strength :position ,position :range ,trial::range))
       (7
        `(:type :brake :strength ,strength))
       (T
        `(:type ,type :srength ,strength :position ,position :normal ,normal :range ,trial::range :inv-range ,trial::inv-range)))
     stream)))
