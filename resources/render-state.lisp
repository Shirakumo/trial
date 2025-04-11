(in-package #:org.shirakumo.fraf.trial)

(defclass render-state (resource)
  ((blend-mode :initarg :blend-mode :initform :source-over :accessor blend-mode)
   (depth-bias :initarg :depth-bias :initform NIL :accessor depth-bias)
   (write-depth-p :initarg :write-depth-p :initform T :accessor write-depth-p)
   (write-stencil-p :initarg :write-stencil-p :initform T :accessor write-stencil-p)
   (clamp-depth-p :initarg :clamp-depth-p :initform T :accessor clamp-depth-p)
   (depth-test :initarg :depth-test :initform '<= :accessor depth-test)
   (stencil-test :initarg :stencil-test :initform 'T :accessor stencil-test)
   (stencil-value :initarg :stencil-value :initform 0 :accessor stencil-value)
   (on-stencil-fail :initarg :on-stencil-fail :initform :keep :accessor on-stencil-fail)
   (on-stencil-depth-fail :initarg :on-stencil-depth-fail :initform :keep :accessor on-stencil-depth-fail)
   (on-stencil-pass :initarg :on-stencil-pass :initform :replace :accessor on-stencil-pass)
   (front-face :initarg :front-face :initform :ccw :accessor front-face)
   (cull-face :initarg :cull-face :initform :back :accessor cull-face)
   (polygon-mode :initarg :polygon-mode :initform :fill :accessor polygon-mode)
   (framebuffer :initarg :framebuffer :initform NIL :accessor framebuffer)
   (shader-program :initarg :shader-program :initform NIL :accessor shader-program)))

(defmethod allocated-p ((state render-state)) T)
(defmethod allocate ((state render-state)))
(defmethod deallocate ((state render-state)))

(defmethod activate ((state render-state))
  (activate (framebuffer state))
  (activate (shader-program state)))

(defmethod (setf blend-mode) :before (mode (state render-state))
  (ecase mode
    ((:source-over :normal)
     (gl:blend-func-separate :src-alpha :one-minus-src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-over
     (gl:blend-func-separate :one-minus-dst-alpha :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:clear
     (gl:blend-func-separate :zero :zero :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:source
     (gl:blend-func-separate :one :zero :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination
     (gl:blend-func-separate :zero :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:source-in
     (gl:blend-func-separate :dst-alpha :zero :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-in
     (gl:blend-func-separate :zero :src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:source-out
     (gl:blend-func-separate :one-minus-dst-alpha :zero :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-out
     (gl:blend-func-separate :zero :one-minus-src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:destination-atop
     (gl:blend-func-separate :one-minus-dst-alpha :src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:xor
     (gl:blend-func-separate :one-minus-dst-alpha :one-minus-src-alpha :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:add
     (gl:blend-func-separate :src-alpha :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (:one
     (gl:blend-func-separate :one :one :one :one)
     (gl:blend-equation :func-add))
    (:multiply
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :multiply-khr))
       (T
        (gl:blend-func-separate :zero :src-color :one :one-minus-src-alpha)
        (gl:blend-equation :func-add))))
    (:screen
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :screen-khr))
       (T
        (gl:blend-func-separate :one :one-minus-src-color :one :one-minus-src-alpha)
        (gl:blend-equation :func-add))))
    (:overlay
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :overlay-khr))))
    (:darken
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :darken-khr))
       (T
        (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
        (gl:blend-equation :max))))
    (:lighten
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :lighten-khr))))
    (:dodge
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :colordodge-khr))))
    (:burn
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :colorburn-khr))))
    (:hard-light
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :hardlight-khr))))
    (:soft-light
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :softlight-khr))))
    (:difference
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :difference-khr))
       (T
        (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
        (gl:blend-equation :func-subtract))))
    (:exclusion
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :exclusion-khr))))
    (:hue
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :hsl-hue-khr))))
    (:saturation
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :hsl-saturation-khr))))
    (:color
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :hsl-color-khr))))
    (:luminosity
     (gl-extension-case
       (:GL-KHR-BLEND-EQUATION-ADVANCED
        (gl:blend-equation :hsl-luminosity-khr))))
    (:invert
     (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-reverse-subtract))))

(defmethod (setf depth-bias) :before (value (state render-state))
  (implement!))

(defmethod (setf write-depth-p) :before (value (state render-state))
  (gl:depth-mask value))

(defmethod (setf write-stencil-p) :before (value (state render-state))
  (gl:stencil-mask (if value #xFF #x00)))

(defmethod (setf clamp-depth-p) :before (value (state render-state))
  (when-gl-extension :gl-arb-depth-clamp
    (if value
        (enable-feature :depth-clamp)
        (disable-feature :depth-clamp))))

(defun %to-test-op (value)
  (ecase value
    ((NIL) :never)
    (< :less)
    (<= :lequal)
    (= :equal)
    (/= :nequal)
    (> :greater)
    (>= :gequal)
    ((T) :always)))

(defmethod (setf depth-test) :before (value (state render-state))
  (gl:depth-func (%to-test-op value)))

(defmethod (setf stencil-test) :before (value (state render-state))
  (gl:stencil-func (%to-test-op value)
                   (stencil-value state)
                   #xFF))

(defmethod (setf stencil-value) :before (value (state render-state))
  (gl:stencil-func (stencil-test state)
                   value
                   #xFF))

(defun %to-stencil-op (value)
  (ecase value
    (:keep :keep)
    (:zero :zero)
    (:replace :replace)
    (:invert :invert)
    (:incf :incr)
    (:decf :decr)
    (:incf-wrap :incr-wrap)
    (:decf-wrap :decr-wrap)))

(defmethod (setf on-stencil-fail) :before (value (state render-state))
  (gl:stencil-op (%to-stencil-op value)
                 (%to-stencil-op (on-stencil-depth-fail state))
                 (%to-stencil-op (on-stencil-pass state))))

(defmethod (setf on-stencil-depth-fail) :before (value (state render-state))
  (gl:stencil-op (%to-stencil-op (on-stencil-pass state))
                 (%to-stencil-op value)
                 (%to-stencil-op (on-stencil-pass state))))

(defmethod (setf on-stencil-pass) :before (value (state render-state))
  (gl:stencil-op (%to-stencil-op (on-stencil-fail state))
                 (%to-stencil-op (on-stencil-depth-fail state))
                 (%to-stencil-op value)))

(defmethod (setf front-face) :before (value (state render-state))
  (gl:front-face value))

(defmethod (setf cull-face) :before (value (state render-state))
  (cond (value
         (enable-feature :cull-face)
         (gl:cull-face value))
        (T
         (disable-feature :cull-face))))

(defmethod (setf polygon-mode) :before (value (state render-state))
  (gl:polygon-mode :front-and-back value))

(defmacro with-render-state ((&rest state &key &allow-other-keys) &body body)
  (let ((state (loop for (k v) on state by #'cddr
                     for slot = (c2mop:slot-definition-name (initarg-slot 'render-state k T))
                     collect (list (gensym (string slot)) slot v)))
        (instance (gensym "INSTANCE")))
    `(let* ((,instance (render-state *context*))
            ,@(loop for (g s) in state
                    collect `((,g (,s ,instance)))))
       ,@(loop for (g s v) in state
               collect `(setf (,s ,instance) ,v))
       (unwind-protect
            (progn
              ,@body)
         ,@(loop for (g s) in state
                 collect `(setf (,s ,instance) ,g))))))
