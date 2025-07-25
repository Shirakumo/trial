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

(declaim (inline set-blend-mode))
(defun set-blend-mode (mode &key (advanced-blend-equations NIL))
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
    (:subtract
     (gl:blend-func-separate :src-alpha :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-reverse-subtract))
    (:one
     (gl:blend-func-separate :one :one :one :one)
     (gl:blend-equation :func-add))
    (:invert
     (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
     (gl:blend-equation :func-reverse-subtract))
    (:multiply
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :multiply-khr))
           (T
            (gl:blend-func-separate :zero :src-color :one :one-minus-src-alpha)
            (gl:blend-equation :func-add))))
    (:screen
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :screen-khr))
           (T
            (gl:blend-func-separate :one :one-minus-src-color :one :one-minus-src-alpha)
            (gl:blend-equation :func-add))))
    (:darken
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :darken-khr))
           (T
            (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
            (gl:blend-equation :max))))
    (:difference
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :difference-khr))
           (T
            (gl:blend-func-separate :one :one :one :one-minus-src-alpha)
            (gl:blend-equation :func-subtract))))
    (:overlay
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :overlay-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:lighten
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :lighten-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:dodge
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :colordodge-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:burn
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :colorburn-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:hard-light
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :hardlight-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:soft-light
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :softlight-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:exclusion
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :exclusion-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:hue
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :hsl-hue-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:saturation
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :hsl-saturation-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:color
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :hsl-color-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))
    (:luminosity
     (cond ((and advanced-blend-equations
                 (gl-extension-p :GL-KHR-BLEND-EQUATION-ADVANCED))
            (gl:blend-equation :hsl-luminosity-khr))
           (T
            (error "Blend mode ~a is not supported!" mode))))))

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

(defvar *stencil-mask* T)
(defmacro with-stencil-mask (mode &body body)
  (let ((old-mode (gensym "OLD-MODE"))
        (new-mode (gensym "NEW-MODE"))
        (thunk (gensym "THUNK")))
    `(let ((,old-mode *stencil-mask*)
           (,new-mode ,mode))
       (flet ((,thunk ()
                ,@body))
         (if (eq ,new-mode ,old-mode)
             (,thunk)
             (let ((*stencil-mask* ,new-mode))
               (gl:stencil-mask ,(if new-mode #xFF #x00))
               (multiple-value-prog1 (,thunk)
                 (gl:stencil-mask ,(if old-mode #xFF #x00)))))))))
