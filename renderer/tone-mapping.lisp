(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass tone-mapping-pass (post-effect-pass)
  ((previous-pass :port-type input :texspec (:internal-format :rgba16f) :accessor previous-pass)
   (color :port-type output :attachment :color-attachment0 :accessor color)
   (inv-gamma :initarg :inv-gamma :initform (/ 2.2) :accessor inv-gamma :uniform T))
  (:shader-file (trial "tone-map/hdr-tone-mapping.glsl")))

(defmethod (setf gamma) (gamma (pass tone-mapping-pass))
  (setf (inv-gamma pass) (/ gamma)))

(defmethod gamma ((pass tone-mapping-pass))
  (/ (inv-gamma pass)))

(define-shader-pass hable (tone-mapping-pass)
  ((shoulder-strength :initarg :shoulder-strength :initform 0.15 :accessor shoulder-strength :uniform "a")
   (linear-strength :initarg :linear-strength :initform 0.5 :accessor linear-strength :uniform "b")
   (linear-angle :initarg :linear-angle :initform 0.1 :accessor linear-angle :uniform "c")
   (toe-strength :initarg :toe-strength :initform 0.2 :accessor toe-strength :uniform "d")
   (toe-numerator :initarg :toe-numerator :initform 0.02 :accessor toe-numerator :uniform "e")
   (toe-denominator :initarg :toe-denominator :initform 0.3 :accessor toe-denominator :uniform "f")
   (linear-white-point :initarg :linear-white-point :initform 11.2 :accessor linear-white-point :uniform "w")
   (exposure-bias :initarg :exposure-bias :initform 2.0 :accessor exposure-bias :uniform "exposure_bias"))
  (:shader-file (trial "tone-map/hable.glsl")))

(define-shader-pass hill-aces (tone-mapping-pass) ()
  (:shader-file (trial "tone-map/hill-aces.glsl")))

(define-shader-pass narkowicz-aces (tone-mapping-pass) ()
  (:shader-file (trial "tone-map/narkowicz-aces.glsl")))

(define-shader-pass reinhard (tone-mapping-pass) ()
  (:shader-file (trial "tone-map/reinhard.glsl")))

(define-shader-pass reinhard-extended (tone-mapping-pass) 
  ((c-white :initarg :c-white :initform 1.0 :accessor c-white :uniform "c_white"))
  (:shader-file (trial "tone-map/reinhard-extended.glsl")))

(define-shader-pass schlick (tone-mapping-pass) 
  ((p :initarg :p :initform 1.0 :accessor p :uniform "p")
   (hi-val :initarg :hi-val :initform 5.0 :accessor hi-val :uniform "hi_val"))
  (:shader-file (trial "tone-map/schlick.glsl")))

(define-shader-pass tumblin-rushmeier (tone-mapping-pass)
  ((luminance-map :port-type input :texspec (:internal-format :r16))
   (ld-max :initarg :ld-max :initform 150.0 :accessor ld-max :uniform "ld_max")
   (c-max :initarg :c-max :initform 100.0 :accessor c-max :uniform "c_max"))
  (:shader-file (trial "tone-map/tumblin-rushmeier.glsl")))

(define-shader-pass uchimura (tone-mapping-pass)
  ((max-brightness :initarg :max-brightness :initform 1.0 :accessor max-brightness :uniform "m")
   (contrast :initarg :contrast :initform 0.0 :accessor contrast :uniform "a")
   (linear-start :initarg :linear-start :initform 0.0 :accessor linear-start :uniform "m")
   (linear-length :initarg :linear-length :initform 0.01 :accessor linear-length :uniform "l")
   (black-tightness-shape :initarg :black-tightness-shape :initform 1.0 :accessor black-tightness-shape :uniform "c")
   (black-tightness-offset :initarg :black-tightness-offset :initform 0.0 :accessor black-tightness-offset :uniform "b"))
  (:shader-file (trial "tone-map/uchimura.glsl")))

(define-shader-pass ward (tone-mapping-pass)
  ((ld-max :initarg :ld-max :initform 1.0 :accessor ld-max :uniform "ld_max"))
  (:shader-file (trial "tone-map/ward.glsl")))
