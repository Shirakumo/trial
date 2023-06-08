#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defstruct (texture-source
            (:constructor make-texture-source (&key pixel-data pixel-type pixel-format level src dst))
            (:predicate NIL)
            (:copier NIL))
  (pixel-data (cffi:null-pointer))
  (pixel-type :unsigned-byte)
  (pixel-format :rgba)
  (level NIL)
  ;;         X Y Z W H D
  (src (list NIL NIL NIL NIL NIL NIL))
  (dst (list NIL NIL NIL NIL NIL NIL)))

(define-accessor-delegate-methods pixel-data (texture-source-pixel-data texture-source))
(define-accessor-delegate-methods pixel-type (texture-source-pixel-type texture-source))
(define-accessor-delegate-methods pixel-format (texture-source-pixel-format texture-source))
(define-accessor-delegate-methods level (texture-source-level texture-source))

(defun texture-sources->texture-size (sources)
  (let ((max-x 0)
        (max-y 0)
        (max-z 0))
    (dolist (source sources (list max-x max-y max-z))
      (destructuring-bind (x y z w h d) (texture-source-src source)
        (setf max-x (max max-x (+ (or x 0) (or w 0))))
        (setf max-y (max max-y (+ (or y 0) (or h 0))))
        (setf max-z (max max-z (+ (or z 0) (or d 0))))))))

(defun make-image-source (data width height pixel-type pixel-format &key defaults (depth 0) level)
  (merge-texture-sources (make-texture-source :src (list NIL NIL NIL width height depth)
                                              :dst (list NIL NIL NIL NIL NIL NIL)
                                              :pixel-data data
                                              :pixel-type pixel-type
                                              :pixel-format pixel-format
                                              :level level)
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
      (unless (level source) (setf (level source) (level defaults)))))
  source)

(defun upload-texture-source (texture source)
  (let ((target (target texture))
        (level (or (texture-source-level source) 0))
        (format (texture-source-pixel-format source))
        (type (texture-source-pixel-type source))
        (data (texture-source-pixel-data source)))
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
        (ecase target
          (:texture-1d
           (gl:pixel-store :pack-skip-pixels src-x)
           (%gl:tex-sub-image-1d target level dst-x dst-w format type data))
          (:texture-2d :texture-1d-array
           (gl:pixel-store :pack-skip-pixels src-x)
           (gl:pixel-store :pack-skip-rows src-y)
           (gl:pixel-store :pack-row-length src-w)
           (%gl:tex-sub-image-2d target level dst-x dst-y dst-w dst-h format type data))
          (:texture-3d :texture-2d-array
           (gl:pixel-store :pack-skip-pixels src-x)
           (gl:pixel-store :pack-skip-rows src-y)
           (gl:pixel-store :pack-skip-images src-z)
           (gl:pixel-store :pack-row-length src-w)
           (gl:pixel-store :pack-image-height src-h)
           (%gl:tex-sub-image-3d target level dst-x dst-y dst-z dst-w dst-h dst-d format type data)))))))

(defclass texture (gl-resource)
  ((width :initarg :width :writer (setf width))
   (height :initarg :height :writer (setf height))
   (depth :initarg :depth :writer (setf depth))
   (target :initarg :target :writer (setf target))
   (levels :initarg :levels :writer (setf levels))
   (samples :initarg :samples :writer (setf samples))
   (internal-format :initarg :internal-format :writer (setf internal-format))
   (sources :initarg :sources :initform (list (make-texture-source NIL)) :accessor sources)
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
(define-unbound-reader texture mag-filter :linear)
(define-unbound-reader texture min-filter :linear-mipmap-linear)
(define-unbound-reader texture mipmap-levels (list 0 1000))
(define-unbound-reader texture mipmap-lod (list -1000 1000 0.0))
(define-unbound-reader texture anisotropy NIL)
(define-unbound-reader texture wrapping '(:clamp-to-edge :clamp-to-edge :clamp-to-edge))
(define-unbound-reader texture border-color (vec 0 0 0 0))
(define-unbound-reader texture swizzle '(:r :g :b :a))
(define-unbound-reader texture storage :dynamic)

(defmethod pixel-data ((texture texture)) (pixel-data (first (sources texture))))
(defmethod pixel-type ((texture texture)) (pixel-type (first (sources texture))))
(defmethod pixel-format ((texture texture)) (pixel-format (first (sources texture))))

(defmethod (setf pixel-data) (value (texture texture)) (setf (pixel-data (first (sources texture))) value))
(defmethod (setf pixel-type) (value (texture texture)) (setf (pixel-type (first (sources texture))) value))
(defmethod (setf pixel-format) (value (texture texture)) (setf (pixel-format (first (sources texture))) value))

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
  (when (and (getf args :internal-format)
             (not (getf args :pixel-format)))
    (setf (getf args :pixel-format) (texture-internal-format->pixel-format (getf args :internal-format))))
  (when (and (getf args :internal-format)
             (not (getf args :pixel-type)))
    (setf (getf args :pixel-type) (texture-internal-format->pixel-type (getf args :internal-format))))
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
    (test #'check-texture-target :target)
    (test #'check-texture-internal-format :internal-format)
    (test #'check-texture-pixel-format :pixel-format)
    (test #'check-texture-pixel-type :pixel-type)
    (test #'check-texture-mag-filter :mag-filter)
    (test #'check-texture-min-filter :min-filter)
    (test #'check-texture-wrapping :wrapping #'first)
    (test #'check-texture-wrapping :wrapping #'second)
    (test #'check-texture-wrapping :wrapping #'third)))

(defmethod shared-initialize :after ((texture texture) slots &key pixel-format pixel-type pixel-data)
  (when pixel-data (setf (pixel-data texture) pixel-data))
  (when pixel-type (setf (pixel-type texture) pixel-type))
  (when pixel-format (setf (pixel-format texture) pixel-format)))

(defmethod update-buffer-data ((buffer texture) (data (eql T)) &rest args)
  (apply #'update-buffer-data buffer (pixel-data buffer) args))

(defmethod update-buffer-data ((buffer texture) (data vector) &key (x 0) (y 0) (z 0) (level 0) (width (width buffer)) (height (height buffer)) (depth (depth buffer))
                                                                   (pixel-format (pixel-format buffer)) (pixel-type (pixel-type buffer)))
  (cffi:with-pointer-to-vector-data (ptr data)
    (gl:bind-texture (target buffer) (gl-name buffer))
    (ecase (target buffer)
      (:texture-1d
       (%gl:tex-sub-image-1d :texture-1d level x width pixel-format pixel-type ptr))
      ((:texture-2d :texture-1d-array)
       (%gl:tex-sub-image-2d (target buffer) level x y width height pixel-format pixel-type ptr))
      ((:texture-3d :texture-2d-array)
       (%gl:tex-sub-image-3d (target buffer) level x y z width height depth pixel-format pixel-type ptr)))))

(defmethod resize-buffer-data ((buffer texture) (data (eql T)) &rest args)
  (apply #'resize-buffer-data buffer (pixel-data buffer) args))

(defmethod resize-buffer-data ((buffer texture) (data vector) &key (level 0) (width (width buffer)) (height (height buffer)) (depth (depth buffer))
                                                                   (pixel-format (pixel-format buffer)) (pixel-type (pixel-type buffer)))
  (cffi:with-pointer-to-vector-data (ptr data)
    (gl:bind-texture (target buffer) (gl-name buffer))
    (ecase (target buffer)
      (:texture-1d
       (%gl:tex-image-1d (target buffer) level (internal-format buffer) width 0 pixel-format pixel-type ptr))
      ((:texture-2d :texture-1d-array)
       (%gl:tex-image-2d (target buffer) level (internal-format buffer) width height 0 pixel-format pixel-type ptr))
      ((:texture-3d :texture-2d-array)
       (%gl:tex-image-3d (target buffer) level (internal-format buffer) width height depth 0 pixel-format pixel-type ptr)))))

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

(defmacro with-pixel-data-pointer ((ptr data) &body body)
  (let ((datag (gensym "DATA")))
    `(let ((,datag ,data))
       (flet ((thunk (,ptr) () ,@body))
         (etypecase ,datag
           (null
            (thunk (cffi:null-pointer)))
           (cffi:foreign-pointer
            (thunk ,datag))
           (vector
            (if (static-vector-p ,datag)
                (thunk (static-vectors:static-vector-pointer ,datag))
                (cffi:with-pointer-to-vector-data (,ptr ,datag)
                  (thunk ,ptr)))))))))

(defun allocate-texture-storage (texture)
  (with-accessor-values (target storage levels internal-format width height depth
                                samples pixel-format pixel-type pixel-data) texture
    (let ((internal-format (cffi:foreign-enum-value '%gl:enum internal-format))
          (pixel-element-type (pixel-type->cl-type pixel-type)))
      ;; FIXME: Handle array cases better, factor this out into an update routine.
      (case target
        ((:texture-1d)
         (with-pixel-data-pointer (ptr pixel-data)
           (ecase storage
             (:dynamic (%gl:tex-image-1d target 0 internal-format width 0 pixel-format pixel-type ptr))
             (:static (%gl:tex-storage-1d target levels internal-format width)))))
        ((:texture-2d :texture-1d-array)
         (with-pixel-data-pointer (ptr pixel-data)
           (ecase storage
             (:dynamic (%gl:tex-image-2d target 0 internal-format width height 0 pixel-format pixel-type ptr))
             (:static (%gl:tex-storage-2d target levels internal-format width height)))))
        ((:texture-cube-map)
         (loop for target in '(:texture-cube-map-positive-x :texture-cube-map-negative-x
                               :texture-cube-map-positive-y :texture-cube-map-negative-y
                               :texture-cube-map-positive-z :texture-cube-map-negative-z)
               for data in (if (consp pixel-data)
                               pixel-data
                               (let ((c (cons pixel-data NIL)))
                                 (setf (cdr c) c)))
               do (with-pixel-data-pointer (ptr data)
                    (ecase storage
                      (:dynamic (%gl:tex-image-2d target 0 internal-format width height 0 pixel-format pixel-type ptr))
                      (:static (%gl:tex-storage-2d target levels internal-format width height))))))
        ((:texture-3d :texture-2d-array)
         (ecase storage
           (:dynamic (%gl:tex-image-3d target 0 internal-format width height depth 0 pixel-format pixel-type (cffi:null-pointer)))
           (:static (%gl:tex-storage-3d target levels internal-format width height depth)))
         (typecase pixel-data
           (list
            (loop for z from 0
                  for data in pixel-data
                  do (with-pixel-data-pointer (ptr data)
                       (%gl:tex-sub-image-3d target 0 0 0 z width height 1 pixel-format pixel-type ptr))))
           ((not null)
            (with-pixel-data-pointer (ptr pixel-data)
              (%gl:tex-sub-image-3d target 0 0 0 0 width height depth pixel-format pixel-type ptr)))))
        ((:texture-2d-multisample)
         (%gl:tex-storage-2d-multisample target samples internal-format width height 1))
        ((:texture-2d-multisample-array)
         (%gl:tex-storage-3d-multisample target samples internal-format width height depth 1))))))

(defmethod allocate ((texture texture))
  (with-accessors* (width height depth target samples internal-format pixel-format pixel-type pixel-data swizzle
                          mag-filter min-filter mipmap-levels mipmap-lod anisotropy wrapping border-color storage)
      texture
    (let ((tex (gl:gen-texture)))
      (with-cleanup-on-failure (gl:delete-textures (list tex))
        (gl:bind-texture target tex)
        (allocate-texture-storage texture)
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
          (when anisotropy
            (gl:tex-parameter target :texture-max-anisotropy-ext anisotropy)))
        (gl:bind-texture target 0)
        (setf (data-pointer texture) tex)))))

(defmethod deallocate ((texture texture))
  (gl:delete-textures (list (gl-name texture))))

(defmethod unload ((texture texture))
  (maybe-free-static-vector (pixel-data texture))
  (setf (pixel-data texture) NIL))

(defmethod resize ((texture texture) width height)
  (when (or (/= width (width texture))
            (/= height (height texture)))
    (setf (width texture) width)
    (setf (height texture) height)
    (when (allocated-p texture)
      (assert (eql :dynamic (storage texture)))
      (gl:bind-texture (target texture) (gl-name texture))
      (allocate-texture-storage texture)
      (when (find (min-filter texture) '(:linear-mipmap-linear :linear-mipmap-nearest
                                         :nearest-mipmap-linear :nearest-mipmap-nearest))
        (gl:generate-mipmap (target texture)))
      (gl:bind-texture (target texture) 0))))

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
