(in-package #:org.shirakumo.fraf.trial)

(defclass texture (gl-resource)
  ((width :initarg :width :writer (setf width))
   (height :initarg :height :writer (setf height))
   (depth :initarg :depth :writer (setf depth))
   (target :initarg :target :writer (setf target))
   (levels :initarg :levels :writer (setf levels))
   (samples :initarg :samples :writer (setf samples))
   (internal-format :initarg :internal-format :writer (setf internal-format))
   (sources :initarg :sources :initform () :accessor sources)
   (mag-filter :initarg :mag-filter :writer (setf mag-filter))
   (min-filter :initarg :min-filter :writer (setf min-filter))
   (mipmap-levels :initarg :mipmap-levels :writer (setf mipmap-levels))
   (mipmap-lod :initarg :mipmap-lod :writer (setf mipmap-lod))
   (anisotropy :initarg :anisotropy :writer (setf anisotropy))
   (wrapping :initarg :wrapping :writer (setf wrapping))
   (border-color :initarg :border-color :writer (setf border-color))
   (swizzle :initarg :swizzle :writer (setf swizzle))
   (storage :initarg :storage)))

(define-unbound-reader texture width (first (texture-sources->texture-size (sources texture))))
(define-unbound-reader texture height (second (texture-sources->texture-size (sources texture))))
(define-unbound-reader texture depth (third (texture-sources->texture-size (sources texture))))
(define-unbound-reader texture target :texture-2d)
(define-unbound-reader texture levels 1)
(define-unbound-reader texture samples 1)
(define-unbound-reader texture internal-format :rgba)
(define-unbound-reader texture mag-filter
  (case (setting :display :texture :filter)
    (:nearest :nearest)
    (T :linear)))
(define-unbound-reader texture min-filter
  (case (setting :display :texture :filter)
    (:nearest :nearest)
    ((:linear :bilinear) :linear)
    (T :linear-mipmap-linear)))
(define-unbound-reader texture mipmap-levels (list 0 10))
(define-unbound-reader texture mipmap-lod (list -1000 1000 0.0))
(define-unbound-reader texture anisotropy
  (setting :display :texture :anisotropy))
(define-unbound-reader texture wrapping '(:clamp-to-edge :clamp-to-edge :clamp-to-edge))
(define-unbound-reader texture border-color (vec 0 0 0 0))
(define-unbound-reader texture swizzle '(:r :g :b :a))
(define-unbound-reader texture storage :dynamic)

(defmacro define-texture-property-slots (&rest props)
  `(progn
     ,@(loop for prop in props
             collect `(defmethod (setf ,prop) :after (value (texture texture))
                        (when (allocated-p texture)
                          (gl:bind-texture (target texture) (gl-name texture))
                          (update-texture-properties texture)
                          (gl:bind-texture (target texture) 0))))))

(define-texture-property-slots mag-filter min-filter mipmap-levels mipmap-lod anisotropy wrapping border-collor swizzle)

(defmethod (setf sources) :after ((sources cons) (texture texture))
  (when (allocated-p texture)
    #-elide-context-current-checks
    (check-context-current)
    (gl:bind-texture (target texture) (gl-name texture))
    (destructuring-bind (w h d) (texture-sources->texture-size sources)
      (when (or (and w (/= w (width texture)))
                (and h (/= h (height texture)))
                (and d (/= d (depth texture))))
        (setf (width texture) w)
        (setf (height texture) h)
        (setf (depth texture) d)
        (allocate-texture-storage texture)))
    (dolist (source sources)
      (upload-texture-source source texture))
    (gl:bind-texture (target texture) 0)))

(defmethod pixel-data ((texture texture))
  (when (sources texture)
    (pixel-data (first (sources texture)))))

(defmethod pixel-type ((texture texture)) 
  (if (sources texture)
      (pixel-type (first (sources texture)))
      (internal-format-pixel-type (internal-format texture))))

(defmethod pixel-format ((texture texture))
  (if (sources texture)
      (pixel-format (first (sources texture)))
      (internal-format-pixel-format (internal-format texture))))

(defmethod (setf pixel-data) (value (texture texture))
  (when (sources texture)
    (setf (pixel-data (first (sources texture))) value)))

(defmethod (setf pixel-type) (value (texture texture))
  (when (sources texture)
    (setf (pixel-type (first (sources texture))) value)))

(defmethod (setf pixel-format) (value (texture texture)) 
  (when (sources texture)
    (setf (pixel-format (first (sources texture))) value)))

(defun texture-texspec (texture)
  (loop for slot in '(width height depth target levels samples internal-format
                      mag-filter min-filter mipmap-levels mipmap-lod
                      anisotropy wrapping border-color storage)
        when (slot-boundp texture slot)
        collect (kw slot)
        and collect (slot-value texture slot)))

(defmethod shared-initialize :around ((texture texture) slots &rest args)
  (when (getf args :wrapping)
    (setf (getf args :wrapping) (enlist (getf args :wrapping)
                                        (getf args :wrapping)
                                        (getf args :wrapping))))
  (apply #'call-next-method texture slots args))

(defmethod shared-initialize :before ((texture texture) slots &rest initargs)
  (declare (ignore slots))
  ;; (assert (< 0 width (gl:get* :max-texture-size)))
  ;; (assert (< 0 height (gl:get* :max-texture-size)))
  ;; (assert (< 0 depth (gl:get* :max-texture-size)))
  (flet ((test (func prop &optional (mod #'identity))
           (let* ((temp '#.(make-symbol "NO-VALUE"))
                  (value (getf initargs prop temp)))
             (unless (eq value temp)
               (funcall func (funcall mod (getf initargs prop)))))))
    (test (lambda (x) (check-type x vec4)) :border-color)
    (test #'integerp :width)
    (test #'integerp :height)
    (test #'integerp :depth)
    (test #'check-texture-target :target)
    (test #'check-texture-internal-format :internal-format)
    (test #'check-texture-mag-filter :mag-filter)
    (test #'check-texture-min-filter :min-filter)
    (test #'check-texture-wrapping :wrapping #'first)
    (test #'check-texture-wrapping :wrapping #'second)
    (test #'check-texture-wrapping :wrapping #'third)))

(defmethod shared-initialize :after ((texture texture) slots &key pixel-format pixel-type pixel-data)
  (when (and (null (sources texture)) (or pixel-data pixel-type pixel-format))
    (setf (sources texture) (list (make-texture-source :pixel-format (internal-format-pixel-format (internal-format texture))
                                                       :pixel-type (internal-format-pixel-type (internal-format texture))))))
  (when pixel-data (setf (pixel-data texture) pixel-data))
  (when pixel-type (setf (pixel-type texture) pixel-type))
  (when pixel-format (setf (pixel-format texture) pixel-format)))

(defmethod update-buffer-data ((buffer texture) (data (eql T)) &rest args)
  (apply #'update-buffer-data buffer (pixel-data buffer) args))

(defmethod update-buffer-data ((buffer texture) data &key (x 0) (y 0) (z 0) (level 0) (width (width buffer)) (height (height buffer)) (depth (depth buffer))
                                                                   (pixel-format (pixel-format buffer)) (pixel-type (pixel-type buffer)))
  #-elide-context-current-checks
  (check-context-current)
  (mem:with-memory-region (region data)
    (gl:bind-texture (target buffer) (gl-name buffer))
    (ecase (target buffer)
      (:texture-1d
       (%gl:tex-sub-image-1d :texture-1d level x width pixel-format pixel-type (mem:memory-region-pointer region)))
      ((:texture-2d :texture-1d-array)
       (%gl:tex-sub-image-2d (target buffer) level x y width height pixel-format pixel-type (mem:memory-region-pointer region)))
      ((:texture-3d :texture-2d-array)
       (%gl:tex-sub-image-3d (target buffer) level x y z width height depth pixel-format pixel-type (mem:memory-region-pointer region))))))

(defmethod resize-buffer-data ((buffer texture) (data (eql T)) &rest args)
  (apply #'resize-buffer-data buffer (pixel-data buffer) args))

(defmethod resize-buffer-data ((buffer texture) data &key (level 0) (width (width buffer)) (height (height buffer)) (depth (depth buffer))
                                                                   (pixel-format (pixel-format buffer)) (pixel-type (pixel-type buffer)))
  #-elide-context-current-checks
  (check-context-current)
  (let ((internal-format (cffi:foreign-enum-value '%gl:enum (internal-format buffer))))
    (mem:with-memory-region (region data)
      (gl:bind-texture (target buffer) (gl-name buffer))
      (ecase (target buffer)
        (:texture-1d
         (%gl:tex-image-1d (target buffer) level internal-format width 0 pixel-format pixel-type (mem:memory-region-pointer region)))
        ((:texture-2d :texture-1d-array)
         (%gl:tex-image-2d (target buffer) level internal-format width height 0 pixel-format pixel-type (mem:memory-region-pointer region)))
        ((:texture-3d :texture-2d-array)
         (%gl:tex-image-3d (target buffer) level internal-format width height depth 0 pixel-format pixel-type (mem:memory-region-pointer region)))))))

(defmethod print-object ((texture texture) stream)
  (print-unreadable-object (texture stream :type T :identity T)
    (case (target texture)
      (:texture-1d
       (format stream "~a" (width texture)))
      ((:texture-2d :texture-1d-array :texture-cube-map :texture-2d-multisample)
       (format stream "~ax~a" (width texture) (height texture)))
      ((:texture-3d :texture-2d-array :texture-2d-multisample-array)
       (format stream "~ax~ax~a" (width texture) (height texture) (depth texture))))
    (format stream " ~a~:[~; ALLOCATED~]" (internal-format texture) (allocated-p texture))))

(defun allocate-texture-storage (texture)
  (with-accessor-values (target storage levels internal-format width height depth samples) texture
    (let ((pixel-type (internal-format-pixel-type internal-format))
          (pixel-format (internal-format-pixel-format internal-format))
          (internal-format (cffi:foreign-enum-value '%gl:enum internal-format)))
      (ecase storage
        (:dynamic
         (ecase target
           ((:texture-1d)
            (%gl:tex-image-1d target 0 internal-format width 0 pixel-format pixel-type (cffi:null-pointer)))
           ((:texture-2d :texture-1d-array)
            (%gl:tex-image-2d target 0 internal-format width height 0 pixel-format pixel-type (cffi:null-pointer)))
           ((:texture-cube-map)
            (dolist (target '(:texture-cube-map-positive-x :texture-cube-map-negative-x
                              :texture-cube-map-positive-y :texture-cube-map-negative-y
                              :texture-cube-map-positive-z :texture-cube-map-negative-z))
              (%gl:tex-image-2d target 0 internal-format width height 0 pixel-format pixel-type (cffi:null-pointer))))
           ((:texture-3d :texture-2d-array)
            (%gl:tex-image-3d target 0 internal-format width height depth 0 pixel-format pixel-type (cffi:null-pointer)))))
        ;; TODO: make this only use gl-tex-storage-* if the feature is available, fall back to tex-image instead otherwise.
        (:static
         (ecase target
           ((:texture-1d)
            (%gl:tex-storage-1d target levels internal-format width))
           ((:texture-2d :texture-1d-array)
            (%gl:tex-storage-2d target levels internal-format width height))
           ((:texture-cube-map)
            (%gl:tex-storage-2d target levels internal-format width height))
           ((:texture-3d :texture-2d-array)
            (%gl:tex-storage-3d target levels internal-format width height depth))
           ((:texture-2d-multisample)
            (%gl:tex-storage-2d-multisample target samples internal-format width height 1))
           ((:texture-2d-multisample-array)
            (%gl:tex-storage-3d-multisample target samples internal-format width height depth 1))))))))

(defun update-texture-properties (texture)
  (with-accessors* (width height depth target samples internal-format pixel-format pixel-type pixel-data swizzle
                          mag-filter min-filter mipmap-levels mipmap-lod anisotropy wrapping border-color storage)
      texture
    (gl:tex-parameter target :texture-wrap-s (first wrapping))
    (unless (find target '(:texture-1d-array :texture-1d))
      (gl:tex-parameter target :texture-wrap-t (second wrapping)))
    (when (eql target :texture-cube-map)
      (gl:tex-parameter target :texture-wrap-r (third wrapping)))
    (when (find :clamp-to-border wrapping)
      (gl:tex-parameter target :texture-border-color
                        (list (vx border-color) (vy border-color) (vz border-color) (vw border-color))))
    (cffi:with-foreign-object (params :int 4)
      (loop for c in swizzle
            for i from 0
            do (setf (cffi:mem-aref params :int i)
                     (ecase c
                       ((:r :red) (cffi:foreign-enum-value '%gl:enum :red))
                       ((:g :green) (cffi:foreign-enum-value '%gl:enum :green))
                       ((:b :blue) (cffi:foreign-enum-value '%gl:enum :blue))
                       ((:a :alpha) (cffi:foreign-enum-value '%gl:enum :alpha))
                       ((0 :zero) (cffi:foreign-enum-value '%gl:enum :zero))
                       ((1 :one) (cffi:foreign-enum-value '%gl:enum :one)))))
      (%gl:tex-parameter-iv target :texture-swizzle-rgba params))
    (gl:tex-parameter target :texture-min-filter min-filter)
    (gl:tex-parameter target :texture-mag-filter mag-filter)
    (unless (or (eql target :texture-2d-multisample)
                (find internal-format '(:depth-component :depth-stencil)))
      (when (find min-filter '(:linear-mipmap-linear :linear-mipmap-nearest
                               :nearest-mipmap-linear :nearest-mipmap-nearest))
        (destructuring-bind (&optional (base 0) (max 1000)) mipmap-levels
          (gl:tex-parameter target :texture-base-level base)
          (gl:tex-parameter target :texture-max-level max))
        (destructuring-bind (&optional (min -1000) (max 1000) (bias 0.0)) mipmap-lod
          (gl:tex-parameter target :texture-min-lod min)
          (gl:tex-parameter target :texture-max-lod max)
          (gl:tex-parameter target :texture-lod-bias bias))
        (gl:generate-mipmap target))
      (when (and anisotropy (not (eq min-filter :nearest)))
        (gl:tex-parameter target :texture-max-anisotropy-ext anisotropy)))))

(defmethod allocate ((texture texture))
  (let ((tex (gl:gen-texture)))
    (with-cleanup-on-failure (gl:delete-textures (list tex))
      (gl:bind-texture (target texture) tex)
      (allocate-texture-storage texture)
      (dolist (source (sources texture))
        (upload-texture-source source texture))
      (update-texture-properties texture)
      (gl:bind-texture (target texture) 0)
      (setf (data-pointer texture) tex))))

(defmethod deallocate ((texture texture))
  (gl:delete-textures (list (gl-name texture))))

(defmethod unload ((texture texture))
  (loop for source in (sources texture)
        do (finalize (texture-source-pixel-data source))))

(defmethod resize ((texture texture) width height)
  (when (or (/= width (width texture))
            (/= height (height texture)))
    (setf (width texture) width)
    (setf (height texture) height)
    (when (allocated-p texture)
      (assert (eql :dynamic (storage texture)))
      #-elide-context-current-checks
      (check-context-current)
      (gl:bind-texture (target texture) (gl-name texture))
      (allocate-texture-storage texture)
      (when (find (min-filter texture) '(:linear-mipmap-linear :linear-mipmap-nearest
                                         :nearest-mipmap-linear :nearest-mipmap-nearest))
        (gl:generate-mipmap (target texture)))
      (gl:bind-texture (target texture) 0))))

(defmethod size ((texture texture))
  (let ((type (internal-format-pixel-type (internal-format texture)))
        (format (internal-format-pixel-format (internal-format texture))))
    (* (pixel-data-stride type format)
       (or (width texture) 1)
       (or (height texture) 1)
       (or (depth texture) 1))))

(defmethod save-image ((source texture) target image-type &rest args &key (level 0))
  (mem:with-memory-region (region (size source))
    #-elide-context-current-checks
    (check-context-current)
    (gl:bind-texture (target source) (gl-name source))
    (let ((type (internal-format-pixel-type (internal-format source)))
          (format (internal-format-pixel-format (internal-format source))))
      (%gl:get-tex-image (target source) level format type (memory-region-pointer region))
      (apply #'save-image region target image-type :width (width source) :height (height source) :pixel-type type :pixel-format format args))))

(defmethod bind ((source texture) point)
  (gl:active-texture point)
  (gl:bind-texture (target source) (gl-name source)))

(defmethod activate ((source texture))
  (gl:bind-texture (target source) (gl-name source)))

(defmethod deactivate ((source texture))
  (gl:bind-texture (target source) 0))

(defun clear-texture (texture &optional (pixel 0))
  #-elide-context-current-checks
  (check-context-current)
  (gl-extension-case
    (:gl-arb-clear-texture
     (let ((size (pixel-data-stride (pixel-type texture) (pixel-format texture))))
       (cffi:with-foreign-object (fill :uint8 size)
         (static-vectors:fill-foreign-memory fill size pixel)
         (%gl:clear-tex-image (gl-name texture) 0
                              (pixel-format texture)
                              (pixel-type texture)
                              fill))))
    (T
     (let ((size (* (width texture) (or (height texture) 1) (or (depth texture) 1)
                    (pixel-data-stride (pixel-type texture) (pixel-format texture)))))
       (cffi:with-foreign-object (fill :uint8 size)
         (static-vectors:fill-foreign-memory fill size pixel)
         (gl:bind-texture (target texture) (gl-name texture))
         (ecase (target texture)
           (:texture-1d
            (%gl:tex-sub-image-1d :texture-1d 0 0 (width texture) (pixel-format texture) (pixel-type texture) fill))
           ((:texture-2d :texture-1d-array)
            (%gl:tex-sub-image-2d (target texture) 0 0 0 (width texture) (height texture) (pixel-format texture) (pixel-type texture) fill))
           ((:texture-3d :texture-2d-array)
            (%gl:tex-sub-image-3d (target texture) 0 0 0 0 (width texture) (height texture) (depth texture) (pixel-format texture) (pixel-type texture) fill))))))))

(defmethod clear ((texture texture))
  (clear-texture texture))

;;;; Texture spec wrangling
;; The idea of this is that, in order to maximise sharing of texture resources
;; between independent parts, we need to join (in the lattice sense) two texture
;; specs together to determine a common closest supertype that can be used by
;; both. Some texture attributes are not joinable and in such a case the join will
;; simply return NIL. Others are easily joinable by maxing. Yet others are a huge
;; pain in the ass, such as the internal format. This implementation is a best-
;; effort that in theory is capable of perfectly handling every combination.
;; however, due to limits in my ability to give a shit it is currently only
;; feasible for common formats. The primary place that still needs work is the
;; restructure-texture-format, the rest should be generic enough to handle every-
;; thing, at least by my estimation.

(defun destructure-texture-format (format)
  (cl-ppcre:register-groups-bind (compression signed super r r-size r-type g g-size g-type b b-size b-type rg rg-size rg-type rgb rgb-size rgb-type rgba rgba-size rgba-type a a-size a-type e e-size d d-size d-type s s-size s-type rgtc bptc floatage snorm unorm) ("^(compressed-)?(signed-)?(s)?((?:red|r)(\\d+)?(ui|i|f)?)?(-g(\\d+)?(ui|i|f)?)?(-b(\\d+)?(ui|i|f)?)?(rg(\\d+)?(ui|i|f)?)?(rgb(\\d+)?(ui|i|f)?)?(rgba(\\d+)?(ui|i|f)?)?(-(?:a|alpha)(\\d+)?(ui|i|f)?)?(-e(\\d+)?)?(depth(?:-component-?)?(\\d+)?(f)?)?(-stencil(\\d+)?(ui|i|f)?)?(-rgtc\\d)?(-bptc)?(-signed-float|-unsigned-float)?(-snorm)?(-unorm)?$" (string-downcase format))
    (macrolet ((parse-part (part)
                 (let* ((*print-case* (readtable-case *readtable*))
                        (type (mksym *package* part "-" 'type))
                        (size (mksym *package* part "-" 'size)))
                   `(when ,part
                      (list (when ,size (parse-integer ,size))
                            (cond ((or (equalp ,type "f") floatage) :float)
                                  ((or (equalp ,type "i") (equalp ,type "ui")) :integer))
                            (cond ((or (equalp ,type "i") signed (equalp ,type "-signed-float")) :signed)
                                  ((or (equalp ,type "ui") (equalp ,type "-unsigned-float")) :unsigned)))))))
      (let ((r (parse-part r))
            (g (parse-part g))
            (b (parse-part b))
            (a (parse-part a))
            (d (parse-part d))
            (s (parse-part s))
            (rg (parse-part rg))
            (rgb (parse-part rgb))
            (rgba (parse-part rgba)))
        (list :r (or r rg rgb rgba)
              :g (or g rg rgb rgba)
              :b (or b rgb rgba)
              :a (or a rgba)
              :depth d
              :stencil s
              :shared (cond (e-size (parse-integer e-size))
                            (e T))
              :features (loop for i in (list compression super rgtc bptc snorm unorm)
                              for f in (list :compression :super :rgtc :bptc :snorm :unorm)
                              when i collect f))))))

(defun restructure-texture-format (format)
  (destructuring-bind (&key r g b a depth stencil shared features) format
    ;; FIXME: This is a primitive approach to restructuring. Some of the formats
    ;;        are SERIOUSLY WEIRD, but I also expect them to not ever get used, so
    ;;        this should be "good enough" for now.
    (let ((rgba (cond ((and r g b a) "rgba")
                      ((and r g b) "rgb")
                      ((and r g) "rg")
                      ((and r) "r"))))
      (flet ((format-type (type)
               (format NIL "~@[~a~]~@[~a~]"
                       (first type) (case (second type)
                                      (:float "f")
                                      (:integer (case (third type)
                                                  (:signed "i")
                                                  (:unsigned "ui")))))))
        (values
         (find-symbol (string-upcase
                       (cond ((find :compression features)
                              (format NIL "compressed-~a~@[-rgtc~a~]~@[-~a~]~@[-~a~]"
                                      rgba (when (find :rgtc features) (length rgba))
                                      (find :bptc features) (find :unorm features)))
                             ((and depth stencil)
                              (format NIL "depth~@[~a~]-stencil~@[~a~]"
                                      (format-type depth) (format-type stencil)))
                             (depth
                              (format NIL "depth-component~@[~a~]"
                                      (format-type depth)))
                             (stencil
                              (format NIL "stencil-index~@[~a~]"
                                      (format-type stencil)))
                             (T
                              ;; FIXME: Doesn't handle different types for each component
                              (format NIL "~a~a~@[-e~a~]" rgba (format-type r) shared))))
                      "KEYWORD"))))))

(defun join-texture-format-typespec (a b)
  (flet ((max* (a b)
           (cond ((and a b) (max a b))
                 (a a)
                 (b b))))
    (cond ((null b) (values a T))
          ((null a) (values b T))
          ((equal a b) (values a T))
          (T
           (destructuring-bind (a-b a-t a-s) a
             (destructuring-bind (b-b b-t b-s) b
               (cond ((and (member a-t '(:float NIL))
                           (member b-t '(:float NIL)))
                      (values (list (max* a-b b-b)
                                    (or a-t b-t)
                                    (or a-s b-s))
                              T))
                     ((and (eq a-t b-t) (eq a-s b-s))
                      (values (list (max* a-b b-b)
                                    a-t
                                    a-s)
                              T)))))))))

(defun join-texture-format (a b)
  (let ((a (destructure-texture-format a))
        (b (destructure-texture-format b)))
    (flet ((same (field)
             (equal (getf a field) (getf b field)))
           (join-texture-format-typespec* (f)
             (multiple-value-bind (spec joinable) (join-texture-format-typespec (getf a f) (getf b f))
               (if joinable
                   spec
                   (return-from join-texture-format NIL)))))
      (when (and (same :features)
                 (same :shared))
        (cond ((and (getf a :depth) (getf b :depth))
               (restructure-texture-format
                (list :depth (join-texture-format-typespec* :depth)
                      :stencil (join-texture-format-typespec* :stencil)
                      :features (getf a :features))))
              ((and (getf a :stencil) (getf b :stencil))
               (restructure-texture-format
                (list :stencil (join-texture-format-typespec* :stencil)
                      :features (getf a :features))))
              ((not (or (getf a :depth) (getf b :depth)))
               (restructure-texture-format
                (list :r (join-texture-format-typespec* :r)
                      :g (join-texture-format-typespec* :g)
                      :b (join-texture-format-typespec* :b)
                      :a (join-texture-format-typespec* :a)
                      :features (getf a :features)))))))))

(defun join-texspec (a b)
  (flet ((same (field)
           (equal (getf a field) (getf b field)))
         (max* (a b)
           (cond ((and a b) (max a b)) (a a) (b b))))
    (when (and (same :target)
               (same :width)
               (same :height)
               (same :pixel-type)
               (same :pixel-data)
               (same :mag-filter)
               (same :min-filter)
               (same :wrapping))
      ;; FIXME: width and height handling
      (let ((texspec (copy-list a)))
        (setf (getf texspec :samples)
              (max* (getf a :samples)
                    (getf b :samples)))
        (setf (getf texspec :anisotropy)
              (max* (getf a :anisotropy)
                    (getf b :anisotropy)))
        (setf (getf texspec :internal-format)
              (join-texture-format
               (getf a :internal-format)
               (getf b :internal-format)))
        (when (getf texspec :internal-format)
          texspec)))))

(defun join-texspecs (texspecs)
  (let (spec (collected ()))
    (tagbody
     next
       (setf spec (pop texspecs))
       (unless spec (go end))
     loop
       (dolist (other texspecs)
         (let ((merged (join-texspec spec other)))
           (when merged
             (setf texspecs (remove other texspecs :test #'eq))
             (setf spec merged)
             (go loop))))
       (push spec collected)
       (go next)
     end)
    collected))
