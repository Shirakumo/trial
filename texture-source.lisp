(in-package #:org.shirakumo.fraf.trial)

(defstruct (texture-source
            (:constructor make-texture-source (&key pixel-data pixel-type pixel-format level target src dst))
            (:predicate NIL)
            (:copier NIL))
  (pixel-data (cffi:null-pointer))
  (pixel-type :unsigned-byte)
  (pixel-format :rgba)
  (target NIL)
  (level NIL)
  ;;         X Y Z W H D
  (src (list NIL NIL NIL NIL NIL NIL))
  (dst (list NIL NIL NIL NIL NIL NIL)))

(defmethod print-object ((source texture-source) stream)
  (print-unreadable-object (source stream :type T)
    (format stream "~a ~a"
            (pixel-type source) (pixel-format source))
    (destructuring-bind (x y z w h d) (texture-source-src source)
      (when (or x y z w h d)
        (format stream " SRC: ~@[~a~]~@[,~a~]~@[,~a~]~@[:~a~]~@[x~a~]~@[x~a~]"
                x y z w h d)))
    (destructuring-bind (x y z w h d) (texture-source-dst source)
      (format stream " DST: ~@[~a~]~@[,~a~]~@[,~a~]~@[:~a~]~@[x~a~]~@[x~a~]"
              x y z w h d)
      (format stream "~@[ ~a~]~@[ (~a)~]" (target source) (level source)))))

(define-accessor-delegate-methods pixel-data (texture-source-pixel-data texture-source))
(define-accessor-delegate-methods pixel-type (texture-source-pixel-type texture-source))
(define-accessor-delegate-methods pixel-format (texture-source-pixel-format texture-source))
(define-accessor-delegate-methods level (texture-source-level texture-source))
(define-accessor-delegate-methods target (texture-source-target texture-source))

(defmethod width ((source texture-source))
  (or (nth 3 (texture-source-src source))
      (nth 3 (texture-source-dst source))))

(defmethod height ((source texture-source))
  (or (nth 4 (texture-source-src source))
      (nth 4 (texture-source-dst source))))

(defmethod depth ((source texture-source))
  (or (nth 5 (texture-source-src source))
      (nth 5 (texture-source-dst source))))

(defun texture-sources->texture-size (sources)
  (let ((max-x 0)
        (max-y 0)
        (max-z 0))
    (dolist (source sources)
      (destructuring-bind (x y z w h d) (texture-source-src source)
        (setf max-x (max max-x (+ (or x 0) (or w 0))))
        (setf max-y (max max-y (+ (or y 0) (or h 0))))
        (setf max-z (max max-z (+ (or z 0) (or d 0))))))
    (list (unless (= 0 max-x) max-x) 
          (unless (= 0 max-y) max-y)
          (unless (= 0 max-z) max-z))))

(defun texture-sources->target (sources)
  (let ((target (loop for source in sources thereis (target source))))
    (case target
      ((:texture-cube-map-positive-x :texture-cube-map-negative-x
        :texture-cube-map-positive-y :texture-cube-map-negative-y
        :texture-cube-map-positive-z :texture-cube-map-negative-z)
       :texture-cube-map)
      ((:red :green :blue :alpha :rg :gb :ba :rgb :gba :rgba)
       :texture-2d)
      ((NIL)
       (destructuring-bind (x y z) (texture-sources->texture-size sources)
         (cond (z :texture-3d)
               (y :texture-2d)
               (x :texture-1d)
               (T :texture-2d))))
      (T target))))

(defun normalize-texture-sources (sources &optional target)
  (let ((channels ())
        (targets (delete-duplicates (loop for source in sources collect (target source))))
        (target (or target (texture-sources->target sources))))
    (dolist (target targets)
      (case target
        ((:r :g :b :a) (push target channels))
        (:red (push :r channels))
        (:green (push :g channels))
        (:blue (push :b channels))
        (:alpha (push :a channels))
        (:rg (push :r channels) (push :g channels))
        (:ra (push :r channels) (push :a channels))
        (:gb (push :g channels) (push :b channels))
        (:ba (push :b channels) (push :a channels))
        (:rgb (push :r channels) (push :g channels) (push :b channels))
        (:gba (push :g channels) (push :b channels) (push :a channels))
        (:rgba (push :r channels) (push :g channels) (push :b channels) (push :a channels))))
    (setf channels (delete-duplicates channels))
    (cond ((eql :texture-cube-map target)
           ;; We're coercing the cube map sources to their proper targets.
           (loop for source in sources
                 for target in '(:texture-cube-map-positive-x :texture-cube-map-negative-x
                                 :texture-cube-map-positive-y :texture-cube-map-negative-y
                                 :texture-cube-map-positive-z :texture-cube-map-negative-z)
                 do (unless (target source) (setf (target source) target)))
           sources)
          ((and (null channels) (null (rest targets)))
           ;; No channels and only one target means we're just fine uploading as usual.
           sources)
          ((null (rest targets))
           ;; Singular target but some channels mean we can just coerce the textures to the defaults
           ;; and let swizzling handle the problem.
           (let ((pixel-format (ecase (length channels) (4 :rgba) (3 :rgb) (2 :rg) (1 :red))))
             (dolist (source sources (values sources (infer-swizzle-channels channels)))
               (setf (target source) NIL) ; Turn this into auto-detect
               (setf (pixel-format source) pixel-format))))
          ((rest targets)
           ;; Multiple targets and channels mean we have an actual composition of different channels.
           ;; We merge the sources on CPU-side into a single source.
           (v:warn :trial "Performing expensive texture source merge. You may want to pre-compose these sources instead.")
           (let ((pixel-format (ecase (length channels) (4 :rgba) (3 :rgb) (2 :rg) (1 :red))))
             (destructuring-bind (wd hd dd) (texture-sources->texture-size sources)
               (let* ((cd (length channels))
                      (data (make-array (* wd (or hd 1) (or dd 1) cd) :element-type '(unsigned-byte 8))))
                 (dolist (source sources)
                   (assert (eq :unsigned-byte (pixel-type source)))
                   (destructuring-bind (xds yds zds w h d) (texture-source-dst source)
                     (destructuring-bind (xss yss zss ws hs ds) (texture-source-src source)
                       (mem:with-memory-region (region (texture-source-pixel-data source))
                         (let ((cs (ecase (target source) ((:r :g :b :a :red :green :blue :alpha) 1) ((:rg :gb :ba :ra) 2) ((:rgb :gba) 3) (:rgba 4)))
                               (ids (ecase (target source) ((:red :r :rg :ra :rgb :rgba) 0) ((:green :g :gb :gba) 1) ((:blue :b :ba) 2) ((:alpha :a) 3)))
                               (src (memory-region-pointer region))
                               (xds (or xds 0)) (yds (or yds 0)) (zds (or zds 0)) (xss (or xss 0)) (yss (or yss 0)) (zss (or zss 0)))
                           (dotimes (z (max 1 (or d ds)))
                             (dotimes (y (max 1 (or h hs)))
                               (dotimes (x (or w ws))
                                 (dotimes (i cs)
                                   (setf (aref data (+ ids i (* cd (+ x xds (* wd (+ y yds (* hd (+ z zds))))))))
                                         (cffi:mem-aref src :uint8 (+ i (* cs (+ x xss (* ws (+ y yss (* hs (+ z zss))))))))))))))))))
                 (values (list (make-texture-source :src (list NIL NIL NIL wd hd dd)
                                                    :pixel-data data
                                                    :pixel-format pixel-format
                                                    :pixel-type :unsigned-byte))
                         (infer-swizzle-channels channels)))))))))

(defun make-image-source (data width height pixel-type pixel-format &key defaults (depth 0) level target)
  (merge-texture-sources (make-texture-source :src (list NIL NIL NIL width height depth)
                                              :dst (list NIL NIL NIL NIL NIL NIL)
                                              :pixel-data data
                                              :pixel-type pixel-type
                                              :pixel-format pixel-format
                                              :level level
                                              :target target)
                         defaults))

(defun merge-texture-sources (source &optional defaults)
  (when defaults
    (flet ((merge-rects (rect defaults)
             (loop for r-cons on rect
                   for d-cons on defaults
                   do (when (null (car r-cons))
                        (setf (car r-cons) (car d-cons))))))
      (merge-rects (texture-source-src source) (texture-source-src defaults))
      (merge-rects (texture-source-dst source) (texture-source-dst defaults))
      (unless (pixel-data source) (setf (pixel-data source) (pixel-data defaults)))
      (unless (pixel-type source) (setf (pixel-type source) (pixel-type defaults)))
      (unless (pixel-format source) (setf (pixel-format source) (pixel-format defaults)))
      (unless (level source) (setf (level source) (level defaults)))
      (unless (target source) (setf (target source) (target defaults)))))
  source)

(defun upload-texture-source (source texture)
  (let* ((target (target texture))
         (level (or (texture-source-level source) 0))
         (format (texture-source-pixel-format source))
         (type (texture-source-pixel-type source))
         (data (texture-source-pixel-data source))
         (stride (pixel-data-stride type format)))
    (mem:with-memory-region (region data)
      (destructuring-bind (src-x src-y src-z src-w src-h src-d) (texture-source-src source)
        (destructuring-bind (dst-x dst-y dst-z dst-w dst-h dst-d) (texture-source-dst source)
          (unless dst-w (setf dst-w (width texture)))
          (unless dst-h (setf dst-h (height texture)))
          (unless dst-d (setf dst-d (depth texture)))
          (unless src-w (setf src-w (width texture)))
          (unless src-h (setf src-h (height texture)))
          (unless src-d (setf src-d (depth texture)))
          (unless src-x (setf src-x 0))
          (unless src-y (setf src-y 0))
          (unless src-z (setf src-z 0))
          (unless dst-x (setf dst-x src-x))
          (unless dst-y (setf dst-y src-y))
          (unless dst-z (setf dst-z src-z))
          (let* ((off (* stride (+ src-x (* src-y (+ src-w (* src-z src-h))))))
                 (len (* stride (+ dst-w (* (1- (or dst-h 1)) src-w) (* (1- (or dst-d 1)) src-w src-h))))
                 (ptr (cffi:inc-pointer (memory-region-pointer region) off)))
            #-:elide-buffer-access-checks
            (when (< (memory-region-size region) (+ off len))
              (error "Trying to upload a region bigger than there is data in the image.~%Region is:
  ~a bytes,
  ~d , ~d , ~d
  ~d x ~d x ~d
  ~a ~a
image is
  ~a bytes
  ~d , ~d , ~d
  ~d x ~d x ~d
  ~a ~a"
                     (+ off len) src-x src-y src-z src-w src-h src-d format type
                     (memory-region-size region) dst-x dst-y dst-z dst-w dst-h dst-d (pixel-format texture) (pixel-type texture)))
            (ecase target
              (:texture-1d
               (%gl:tex-sub-image-1d target level dst-x dst-w format type ptr))
              ((:texture-2d :texture-1d-array)
               (gl:pixel-store :unpack-row-length src-w)
               (%gl:tex-sub-image-2d target level dst-x dst-y dst-w dst-h format type ptr))
              ((:texture-cube-map)
               (gl:pixel-store :unpack-row-length src-w)
               (cond ((target source)
                      (%gl:tex-sub-image-2d (target source) level dst-x dst-y dst-w dst-h format type ptr))
                     ;; Assume the texture is vertically stacked, x+ x- y+ y- z+ z-
                     ((gl-extension-p :arb-direct-state-access)
                      (%gl:tex-sub-image-3d :texture-cube-map level dst-x dst-y dst-z dst-w dst-h dst-d format type ptr))
                     (T
                      (loop with len = (* stride (+ dst-w (* (1- (or dst-h 1)) src-w)))
                            for target from (+ (cffi:foreign-enum-value '%gl::enum :texture-cube-map-positive-x) dst-z)
                            repeat dst-d
                            do (%gl:tex-sub-image-2d target level dst-x dst-y dst-w dst-h format type ptr)
                               (cffi:incf-pointer ptr len)))))
              ((:texture-3d :texture-2d-array)
               (gl:pixel-store :unpack-row-length src-w)
               (gl:pixel-store :unpack-image-height src-h)
               (%gl:tex-sub-image-3d target level dst-x dst-y dst-z dst-w dst-h dst-d format type ptr))))
          (gl:pixel-store :unpack-row-length 0)
          (gl:pixel-store :unpack-image-height 0))))))

(defun merge-textures (textures)
  (multiple-value-bind (sources swizzle) (normalize-texture-sources (loop for tex in textures
                                                                          append (sources (ensure-generated tex))))
    (destructuring-bind (w h d) (texture-sources->texture-size sources)
      (make-instance 'texture
                     :width w :height h :depth d
                     :sources sources
                     :swizzle swizzle
                     :internal-format (infer-internal-format (pixel-type (first sources)) (pixel-format (first sources)))
                     :target (texture-sources->target sources)
                     :samples (samples (first textures))
                     :min-filter (min-filter (first textures))
                     :mag-filter (mag-filter (first textures))
                     :wrapping (wrapping (first textures))
                     :mipmap-levels (mipmap-levels (first textures))
                     :mipmap-lod (mipmap-lod (first textures))
                     :anisotropy (anisotropy (first textures))
                     :border-color (border-color (first textures))
                     :storage (storage (first textures))))))

(defmethod finalize ((source texture-source))
  (when (texture-source-pixel-data source)
    (finalize (texture-source-pixel-data source))
    (setf (texture-source-pixel-data source) NIL))
  source)

(defmethod mem:call-with-memory-region ((function function) (source texture-source) &rest args)
  (apply #'mem:call-with-memory-region function (texture-source-pixel-data source) args))
