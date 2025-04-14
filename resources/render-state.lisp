(in-package #:org.shirakumo.fraf.trial)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
     (shader-program :initarg :shader-program :initform NIL :accessor shader-program))))

(defmethod allocated-p ((state render-state)) T)
(defmethod allocate ((state render-state)))
(defmethod deallocate ((state render-state)))

(declaim (inline active-state-diff-p))
(defun active-state-diff-p (state field value)
  (and (eq state (render-state *context*))
       (not (eql value (slot-value state field)))))

(defmacro define-render-state-update (field (value &optional (state 'state)) &body body)
  (let ((fun (mksym *package* '%set- field)))
    `(progn
       (declaim (inline ,fun))
       (defun ,fun (,value ,state)
         (declare (ignorable ,state))
         ,@body)

       (defmethod (setf ,field) :before (,value (,state render-state))
         (when (active-state-diff-p state ',field ,value)
           (,fun ,value ,state))))))

(define-render-state-update blend-mode (value)
  (ecase value
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

(define-render-state-update depth-bias (value)
  (implement!))

(define-render-state-update write-depth-p (value)
  (gl:depth-mask value))

(define-render-state-update write-stencil-p (value)
  (gl:stencil-mask (if value #xFF #x00)))

(define-render-state-update clamp-depth-p (value)
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

(define-render-state-update depth-test (value)
  (gl:depth-func (%to-test-op value)))

(define-render-state-update stencil-test (value state)
  (gl:stencil-func (%to-test-op value)
                   (stencil-value state)
                   #xFF))

(define-render-state-update stencil-value (value state)
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

(define-render-state-update on-stencil-fail (value state)
  (gl:stencil-op (%to-stencil-op value)
                 (%to-stencil-op (on-stencil-depth-fail state))
                 (%to-stencil-op (on-stencil-pass state))))

(define-render-state-update on-stencil-depth-fail (value state)
  (gl:stencil-op (%to-stencil-op (on-stencil-pass state))
                 (%to-stencil-op value)
                 (%to-stencil-op (on-stencil-pass state))))

(define-render-state-update on-stencil-pass (value state)
  (gl:stencil-op (%to-stencil-op (on-stencil-fail state))
                 (%to-stencil-op (on-stencil-depth-fail state))
                 (%to-stencil-op value)))

(define-render-state-update front-face (value)
  (gl:front-face value))

(define-render-state-update cull-face (value)
  (cond (value
         (enable-feature :cull-face)
         (gl:cull-face value))
        (T
         (disable-feature :cull-face))))

(define-render-state-update polygon-mode (value)
  (gl:polygon-mode :front-and-back value))

(define-render-state-update framebuffer (value)
  (activate value))

(define-render-state-update shader-program (value)
  (activate value))

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

(defmethod activate ((state render-state))
  (let ((current (render-state *context*)))
    (unless (eq state current)
      (macrolet ((maybe-update (field)
                   `(let ((new-value (slot-value state ',field)))
                      (unless (eql new-value (slot-value state ',field))
                        (,(mksym *package* '%set- field) new-value state))))
                 (maybe-update-all ()
                   `(progn ,@(loop for slot in (c2mop:class-direct-slots (find-class 'render-state))
                                   collect `(maybe-update ,(c2mop:slot-definition-name slot))))))
        (maybe-update-all)))))

(defmethod activate :after ((state render-state))
  (setf (slot-value *context* 'render-state) state))
