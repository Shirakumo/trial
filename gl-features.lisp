(in-package #:org.shirakumo.fraf.trial)

(defvar *gl-features* #(:blend
                        :clip-distance0
                        :clip-distance1
                        :color-logic-op
                        :cull-face
                        :debug-output
                        :debug-output-synchronous
                        :depth-clamp
                        :depth-test
                        :dither
                        :framebuffer-srgb
                        :line-smooth
                        :multisample
                        :polygon-offset-fill
                        :polygon-offset-line
                        :polygon-offset-point
                        :polygon-smooth
                        :primitive-restart
                        :primitive-restart-fixed-index
                        :rasterizer-discard
                        :sample-alpha-to-coverage
                        :sample-alpha-to-one
                        :sample-coverage
                        :sample-shading
                        :sample-mask
                        :scissor-test
                        :stencil-test
                        :texture-cube-map-seamless
                        :program-point-size))

(defvar *default-enabled-gl-features* #(:dither :multisample))

(defun make-feature-table (&optional parent)
  (let ((table (make-hash-table :test 'eq :size (length *gl-features*))))
    (if parent
        (loop for k being the hash-keys of parent
              for v being the hash-values of parent
              do (setf (gethash k table) v))
        (reset-features table))
    table))

(defun reset-features (&optional (table (feature-table)))
  (loop for k across *gl-features*
        do (if (find k *default-enabled-gl-features*)
               (setf (gethash k table) T)
               (setf (gethash k table) NIL))))

(define-global +feature-table-stack-ptr+ 0)
(define-global +feature-table-stack+ #())

(setf +feature-table-stack+
      (let ((array (make-array 32)))
        (dotimes (i (length array) array)
          (setf (aref array i) (make-feature-table)))))

(defun feature-table ()
  (aref +feature-table-stack+ +feature-table-stack-ptr+))

(defun feature-enabled-p (feature)
  (gethash feature (feature-table)))

(defun enable-feature (&rest features)
  (let ((table (feature-table)))
    (dolist (feature features)
      (unless (gethash feature table)
        (gl:enable feature)
        (setf (gethash feature table) T)))))

(define-compiler-macro enable-feature (&whole whole &environment env &rest features)
  (let ((constants) (variants) (table (gensym "TABLE")))
    (dolist (feature features)
      (if (constantp feature env)
          (push feature constants)
          (push feature variants)))
    (cond ((and (null constants) (null variants))
           ())
          ((null constants)
           whole)
          (T
           `(let ((,table (feature-table)))
              (enable-feature ,@variants)
              ,@(loop for constant in constants
                      for feature = `(load-time-value ,constant)
                      collect `(unless (gethash ,feature ,table)
                                 (gl:enable ,feature)
                                 (setf (gethash ,feature ,table) T))))))))

(defun disable-feature (&rest features)
  (let ((table (feature-table)))
    (dolist (feature features)
      (when (gethash feature table)
        (gl:disable feature)
        (setf (gethash feature table) NIL)))))

(define-compiler-macro disable-feature (&whole whole &environment env &rest features)
  (let ((constants) (variants) (table (gensym "TABLE")))
    (dolist (feature features)
      (if (constantp feature env)
          (push feature constants)
          (push feature variants)))
    (cond ((and (null constants) (null variants))
           ())
          ((null constants)
           whole)
          (T
           `(let ((,table (feature-table)))
              (disable-feature ,@variants)
              ,@(loop for constant in constants
                      for feature = `(load-time-value ,constant)
                      collect `(when (gethash ,feature ,table)
                                 (gl:disable ,feature)
                                 (setf (gethash ,feature ,table) NIL))))))))

(defun push-features ()
  (let ((prev (aref +feature-table-stack+ +feature-table-stack-ptr+))
        (cur (aref +feature-table-stack+ (1+ +feature-table-stack-ptr+))))
    (loop for k being the hash-keys of prev using (hash-value v)
          do (setf (gethash k cur) v))
    (incf +feature-table-stack-ptr+)))

(defun pop-features ()
  (let ((prev (aref +feature-table-stack+ +feature-table-stack-ptr+))
        (cur (aref +feature-table-stack+ (1- +feature-table-stack-ptr+))))
    (loop for k being the hash-keys of prev using (hash-value v)
          do (cond ((and v (not (gethash k cur)))
                    (gl:disable k))
                   ((and (not v) (gethash k cur))
                    (gl:enable k))))
    (decf +feature-table-stack-ptr+)))

(defmacro with-pushed-features (&body body)
  `(progn
     (push-features)
     (unwind-protect
          (progn ,@body)
       (pop-features))))

(defun set-blend-mode (mode)
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

;; KLUDGE: this sucks.
(defvar *depth-mask* T)
(defmacro with-depth-mask (mode &body body)
  (let ((old-mode (gensym "OLD-MODE"))
        (new-mode (gensym "NEW-MODE"))
        (thunk (gensym "THUNK")))
    `(let ((,old-mode *depth-mask*)
           (,new-mode ,mode))
       (flet ((,thunk ()
                ,@body))
         (if (eq ,new-mode ,old-mode)
             (,thunk)
             (let ((*depth-mask* ,new-mode))
               (gl:depth-mask ,new-mode)
               (multiple-value-prog1 (,thunk)
                 (gl:depth-mask ,old-mode))))))))
