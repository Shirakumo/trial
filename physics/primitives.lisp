(in-package #:org.shirakumo.fraf.trial)

(defgeneric intersects-p (a b))
(defgeneric distance (a b))
(defgeneric detect-hits (a b contacts start end))

(defmethod distance ((a vec3) (b vec3))
  (vdistance a b))

(defmethod distance ((a vec2) (b vec2))
  (vdistance a b))

(defun detect-hit (a b &optional (hit (make-hit)))
  (let ((array (make-array 1)))
    (declare (dynamic-extent array))
    (setf (aref array 0) hit)
    (let ((count (detect-hits a b array 0 1)))
      (when (< 0 count)
        hit))))

(defmethod intersects-p (a b)
  (let ((hit (make-hit)))
    (declare (dynamic-extent hit))
    (detect-hit a b hit)))

(defmacro define-distance ((a b) &body body)
  (let ((av (intern "A")) (bv (intern "B"))
        (block (gensym "BLOCK")))
    `(progn
       (defmethod distance ((,av ,a) (,bv ,b))
         (block ,block
           ,@body))
       ,@(unless (eql a b)
           `((defmethod distance ((b ,b) (a ,a))
               (block ,block
                 ,@body)))))))

(defmacro define-intersection-test ((a b) &body body)
  (let ((av (intern "A")) (bv (intern "B"))
        (block (gensym "BLOCK")))
    `(progn
       (defmethod intersects-p ((,av ,a) (,bv ,b))
         (block ,block
           ,@body))
       ,@(unless (eql a b)
           `((defmethod intersects-p ((b ,b) (a ,a))
               (block ,block
                 ,@body)))))))

(defun finish-hit (hit a b)
  (declare (type hit hit))
  (declare (type primitive a))
  (declare (optimize speed))
  #-trial-release (when (v= 0 (hit-normal hit)) (error "Hit normal not set correctly."))
  (setf (hit-a hit) (primitive-entity a))
  (setf (hit-b hit) (if (typep b 'primitive) (primitive-entity b) b))
  (multiple-value-bind (static-friction dynamic-friction restitution)
      (material-interaction-properties
       (primitive-material a) (if (typep b 'primitive) (primitive-material b) NIL))
    (setf (hit-static-friction hit) static-friction)
    (setf (hit-dynamic-friction hit) dynamic-friction)
    (setf (hit-restitution hit) restitution)
    hit))

(defmacro define-hit-detector ((a b) &body body)
  (let ((av (intern "A")) (bv (intern "B"))
        (block (gensym "BLOCK")))
    `(progn
       (defmethod detect-hits ((,av ,a) (,bv ,b) hits start end)
         (declare (type (unsigned-byte 32) start end))
         (declare (type simple-vector hits))
         (when (<= end start)
           (return-from detect-hits start))
         (let ((hit (aref hits start)))
           (block ,block
             (flet ((finish-hit ()
                      (finish-hit hit ,av ,bv)
                      (incf start)
                      (if (< start end)
                          (setf hit (aref hits start))
                          (return-from ,block)))
                    (detect-hits (,av ,bv)
                      (setf start (detect-hits ,av ,bv hits start end))))
               (declare (ignorable #'finish-hit #'detect-hits))
               ,@body))
           start))
       ,@(unless (eql a b)
           `((defmethod detect-hits ((a ,b) (b ,a) hits start end)
               (let ((nstart (detect-hits b a hits start end)))
                 ;; Reverse the information to ensure consistency with hit-a/hit-b
                 (loop for i from start below nstart
                       for hit = (aref hits i)
                       do (nv- (hit-normal hit))
                          (rotatef (hit-a hit) (hit-b hit)))
                 nstart)))))))

(defstruct primitive
  (entity NIL :type T)
  (material NIL :type T)
  (local-transform (meye 4) :type mat4)
  (transform (mat4) :type mat4)
  ;; Cached global location and global bsize, Invalidated by setting
  ;; the x component of GLOBAL-BSIZE to -1.0.
  (cached-global-location (vec3) :type vec3)
  (cached-global-bsize (vec3 -1.0) :type vec3))

(defmethod global-location ((primitive primitive) &optional (target (vec3)))
  (mcol3 (primitive-transform primitive) 3 target))

(defmethod global-orientation ((primitive primitive) &optional (quat (quat)))
  (!qfrom-mat quat (primitive-transform primitive)))

(defmethod location ((primitive primitive))
  (mcol3 (primitive-local-transform primitive) 3))

(defmethod (setf location) ((vec vec3) (primitive primitive))
  (with-fast-matref (m (primitive-local-transform primitive))
    (setf (m 0 3) (vx3 vec))
    (setf (m 1 3) (vy3 vec))
    (setf (m 2 3) (vz3 vec))
    vec))

(defmethod orientation ((primitive primitive))
  (qfrom-mat (primitive-local-transform primitive)))

(defmethod (setf orientation) ((quat quat) (primitive primitive))
  (let ((src (mat3))
        (dst (primitive-local-transform primitive)))
    (declare (dynamic-extent src))
    (qmat quat src)
    (with-fast-matref (s src)
      (with-fast-matref (d dst)
        (setf (d 0 0) (s 0 0) (d 0 1) (s 0 1) (d 0 2) (s 0 2))
        (setf (d 1 0) (s 1 0) (d 1 1) (s 1 1) (d 1 2) (s 1 2))
        (setf (d 2 0) (s 2 0) (d 2 1) (s 2 1) (d 2 2) (s 2 2))))
    quat))

(declaim (inline primitive-update-transform))
(defun primitive-update-transform (primitive transform-matrix)
  (!m* (primitive-transform primitive)
       transform-matrix
       (primitive-local-transform primitive))
  (setf (vx (primitive-cached-global-bsize primitive)) -1.0f0))

(defmethod 3ds:location ((primitive primitive))
  (global-location primitive))

(defmethod 3ds:bsize ((primitive primitive))
  (global-bsize primitive))

(define-accessor-delegate-methods entity (primitive-entity primitive))
(define-accessor-delegate-methods material (primitive-material primitive))
(define-accessor-delegate-methods transform-matrix (primitive-transform primitive))

(defun make-primitive-like (primitive constructor &rest args)
  (apply constructor :entity (primitive-entity primitive)
                     :material (primitive-material primitive)
                     :local-transform (primitive-local-transform primitive)
                     :transform (primitive-transform primitive)
                     args))

(defmacro define-primitive-type (name &body slots)
  (destructuring-bind (name &optional (super 'primitive)) (enlist name)
    (let ((int-constructor (mksym *package* '%make- name))
          (constructor (mksym *package* 'make- name)))
      `(progn
         (declaim (inline ,constructor))
         (defstruct (,name (:constructor ,int-constructor)
                           (:include ,super))
           ,@slots)

         (defun ,constructor (&rest args &key location orientation &allow-other-keys)
           (let ((primitive (apply #',int-constructor (remf* args :location :orientation))))
             (when location (setf (location primitive) location))
             (when orientation (setf (orientation primitive) orientation))
             primitive))

         ,@(loop for (slot) in slots
                 collect `(define-accessor-delegate-methods ,slot (,(mksym *package* name '- slot) ,name)))))))

(define-primitive-type sphere
  (radius 1.0 :type single-float))

(defmethod print-object ((primitive sphere) stream)
  (print-unreadable-object (primitive stream :type T :identity T)
    (format stream "~f" (radius primitive))))

(defmethod global-bsize ((primitive sphere) &optional (target (vec3)))
  (v<- target (sphere-radius primitive)))

(defmethod 3ds:radius ((primitive sphere))
  (sphere-radius primitive))

(define-primitive-type plane
  (normal (vec3 0 1 0) :type vec3)
  (offset 0.0 :type single-float))

(defmethod print-object ((primitive plane) stream)
  (print-unreadable-object (primitive stream :type T :identity T)
    (format stream "~a ~f" (normal primitive) (offset primitive))))

(defmethod global-bsize ((primitive plane) &optional (target (vec3)))
  (cond ((v= +vx3+ (vabs (plane-normal primitive)))
         (vsetf target 1.0 most-positive-single-float most-positive-single-float))
        ((v= +vy3+ (vabs (plane-normal primitive)))
         (vsetf target most-positive-single-float 1.0 most-positive-single-float))
        ((v= +vz3+ (vabs (plane-normal primitive)))
         (vsetf target most-positive-single-float most-positive-single-float 1.0))
        (T ;; The plane is slightly tilted, so its bsize is infinite.
         (vsetf target most-positive-single-float most-positive-single-float most-positive-single-float))))

(defmethod 3ds:radius ((primitive plane))
  most-positive-single-float)

(define-primitive-type (half-space plane))

(defmethod global-bsize ((primitive half-space) &optional (target (vec3)))
  (v<- target most-positive-single-float))

;; NOTE: the box is centred at 0,0,0 and the bsize is the half-size along each axis.
(define-primitive-type box
  (bsize (vec3 1 1 1) :type vec3))

(defmethod print-object ((primitive box) stream)
  (print-unreadable-object (primitive stream :type T :identity T)
    (format stream "~a" (bsize primitive))))

(defmethod global-bsize ((primitive box) &optional (target (vec3)))
  (v<- target (box-bsize primitive))
  (n*m4/3 (mapply (primitive-transform primitive) #'abs) target))

(defmethod 3ds:radius ((primitive box))
  (let ((bsize (box-bsize primitive)))
    (max (vx bsize) (vy bsize) (vz bsize))))

;; Frustums are just boxes skewed by a linear transform. We provide these shorthands
;; here to allow easier construction of frustum testing primitives.
(defun make-frustum-box (left right bottom top near far)
  (let ((transform (mfrustum left right bottom top near far)))
    (make-box :local-transform transform
              :transform (mcopy transform)
              :bsize (vec (* 0.5 (abs (- right left)))
                          (* 0.5 (abs (- top bottom)))
                          (* 0.5 (abs (- far near)))))))

(defun make-perspective-box (fovy aspect near far)
  (let* ((fh (* (the single-float (tan (* (/ fovy 360.0) F-PI))) near))
         (fw (* fh aspect)))
    (make-frustum-box (- fw) fw (- fh) fh near far)))

;; NOTE: the cylinder is centred at 0,0,0 and points Y-up. the "height" is the half-height.
(define-primitive-type cylinder
  (radius 1.0 :type single-float)
  (height 1.0 :type single-float))

(defmethod print-object ((primitive cylinder) stream)
  (print-unreadable-object (primitive stream :type T :identity T)
    (format stream "~f ~f" (radius primitive) (height primitive))))

(defmethod global-bsize ((primitive cylinder) &optional (target (vec3)))
  (let* ((h   (cylinder-height primitive))
         (r   (cylinder-radius primitive))
         (dir (n*m4/3 (primitive-transform primitive) (vec 0 (* 2 h) 0)))
         (dot (v. dir dir))
         (ex  (* r (sqrt (- 1 (if (plusp dot) (* (vx dir) (vx dir) (/ dot)) 0)))))
         (ey  (* r (sqrt (- 1 (if (plusp dot) (* (vy dir) (vy dir) (/ dot)) 0)))))
         (ez  (* r (sqrt (- 1 (if (plusp dot) (* (vz dir) (vz dir) (/ dot)) 0))))))
    (vsetf target
           (* .5 (- (max (+ 0           ex)
                         (+ (vx dir)    ex))
                    (min (+ 0        (- ex))
                         (+ (vx dir) (- ex)))))
           (* .5 (- (max (+ 0           ey)
                         (+ (vy dir)    ey))
                    (min (+ 0        (- ey))
                         (+ (vy dir) (- ey)))))
           (* .5 (- (max (+ 0           ez)
                         (+ (vz dir)    ez))
                    (min (+ 0        (- ez))
                         (+ (vz dir) (- ez))))))))

(defmethod 3ds:radius ((primitive cylinder))
  (sqrt (+ (expt (cylinder-radius primitive) 2)
           (expt (cylinder-height primitive) 2))))

;; NOTE: the pill is centred at 0,0,0, and points Y-up. the "height" is the half-height
;;       and does not include the caps, meaning the total height of the pill is 2r+2h.
(define-primitive-type pill
  (radius 1.0 :type single-float)
  (height 1.0 :type single-float))

(defmethod print-object ((primitive pill) stream)
  (print-unreadable-object (primitive stream :type T :identity T)
    (format stream "~f ~f" (radius primitive) (height primitive))))

(defmethod global-bsize ((primitive pill) &optional (target (vec3)))
  (let* ((h (pill-height primitive))
         (r (pill-radius primitive))
         (dir (n*m4/3 (primitive-transform primitive) (vec 0 h 0))))
    ;; FIXME: this assumes that the primitive transform is length- and
    ;; angle-preserving (that is, it does not include scaling or
    ;; worse). Just scaling could be easily taken into account if
    ;; isotropic.
    (vsetf target
           (+ r (abs (vx dir))) (+ r (abs (vy dir))) (+ r (abs (vz dir))))))

(defmethod 3ds:radius ((primitive pill))
  (+ (pill-height primitive) (pill-radius primitive)))

(define-primitive-type triangle
  (a (vec3 -1 0 -1) :type vec3)
  (b (vec3 +1 0 -1) :type vec3)
  (c (vec3 +0 0 +1) :type vec3))

(defmethod print-object ((primitive triangle) stream)
  (print-unreadable-object (primitive stream :type T :identity T)
    (format stream "~a ~a ~a" (a primitive) (b primitive) (c primitive))))

(defmethod global-bsize ((primitive triangle) &optional (target (vec3)))
  (let ((vmin (vec3)) (vmax (vec3)) (tmp (vec3)))
    (declare (dynamic-extent vmin tmp))
    (flet ((test (vec)
             (!m* tmp (primitive-transform primitive) vec)
             (vmin vmin tmp)
             (vmax vmax tmp)))
      (test (triangle-a primitive))
      (test (triangle-b primitive))
      (test (triangle-c primitive))
      (nv* (!v- target vmax vmin) 0.5))))

(defmethod 3ds:radius ((primitive triangle))
  (sqrt (max (vsqrlength (triangle-a primitive))
             (vsqrlength (triangle-b primitive))
             (vsqrlength (triangle-c primitive)))))

(define-primitive-type general-mesh
  ;; NOTE: Packed vertex positions as X Y Z triplets
  ;; [ X0 Y0 Z0 X1 Y1 Z1 X2 Y2 Z2 X3 Y3 Z3 ... ]
  (vertices #() :type (simple-array single-float (*)))
  ;; NOTE: Vertex indices pointing into the vertex array / 3
  ;; [ 0 1 2 2 3 0 ... ]
  (faces #() :type (simple-array (unsigned-byte 32) (*))))

(defmethod print-object ((primitive general-mesh) stream)
  (print-unreadable-object (primitive stream :type T :identity T)
    (format stream "~d tris" (truncate (length (faces primitive)) 3))))

(declaim (ftype (function (general-mesh) (values vec3 vec3 &optional nil))
                general-mesh-update-global-location-and-bsize))
(defun general-mesh-update-global-location-and-bsize (primitive)
  ;; (declare (optimize speed))
  (let ((vertices (general-mesh-vertices primitive))
        (transform (primitive-transform primitive))
        (local-transform (primitive-local-transform primitive))
        (global-location (general-mesh-cached-global-location primitive))
        (global-bsize (general-mesh-cached-global-bsize primitive))
        (vmin (vec3 #1=most-positive-single-float #1# #1#))
        (vmax (vec3 #2=most-negative-single-float #2# #2#))
        (tmp (vec3)))
    (declare (dynamic-extent vmin tmp))
    (loop for i from 0 below (length vertices) by 3
          do (vsetf tmp
                    (aref vertices (+ i 0))
                    (aref vertices (+ i 1))
                    (aref vertices (+ i 2)))
             (n*m transform tmp)
             (nvmin vmin tmp)
             (nvmax vmax tmp))
    (let ((diff/2 (nv* (!v- tmp vmax vmin) 0.5f0)))
      (values (!v+ global-location (mcol3 local-transform 3) vmin diff/2)
              (v<- global-bsize diff/2)))))

(defmethod global-location ((primitive general-mesh) &optional (target (vec3)))
  (v<- target (if (= -1.0f0 (vx (primitive-cached-global-bsize primitive)))
                  (nth-value 0 (general-mesh-update-global-location-and-bsize primitive))
                  (primitive-cached-global-location primitive))))

(defmethod global-bsize ((primitive general-mesh) &optional (target (vec3)))
  (let ((cached-bsize (primitive-cached-global-bsize primitive)))
    (v<- target (if (= -1.0f0 (vx cached-bsize))
                    (nth-value 1 (general-mesh-update-global-location-and-bsize primitive))
                    (primitive-cached-global-bsize primitive)))))

(defmethod 3ds:radius ((primitive general-mesh))
  ;; NOTE: because we cannot move the location of the fitting sphere to be
  ;;       different from the location of the primitive, this radius is not
  ;;       necessarily the ideal radius.
  (sqrt (loop with vertices = (general-mesh-vertices primitive)
              for i from 0 below (length vertices) by 3
              maximize (+ (expt (aref vertices (+ i 0)) 2)
                          (expt (aref vertices (+ i 1)) 2)
                          (expt (aref vertices (+ i 2)) 2)))))

(define-primitive-type (convex-mesh general-mesh))

(defmacro with-mesh-construction ((constructor &optional (finalizer 'finalize)) &body body)
  (let ((vertices (gensym "VERTICES"))
        (faces (gensym "FACES"))
        (face-table (gensym "FACE-TABLE")))
    `(let ((,vertices (make-array 0 :element-type 'single-float :adjustable T))
           (,faces (make-array 0 :element-type '(unsigned-byte 32) :adjustable T :fill-pointer T))
           (,face-table (make-hash-table :test 'equal))
           (i 0))
       (flet ((,constructor (x y z)
                (let* ((c (cons (cons x y) z))
                       (e (gethash c ,face-table)))
                  (cond (e
                         (vector-push-extend e ,faces))
                        (T
                         (vector-push-extend i ,faces)
                         (let ((j (* 3 i)))
                           (when (< (length ,vertices) (+ j 3))
                             (adjust-array ,vertices (+ j 3)))
                           (setf (aref ,vertices (+ j 0)) (float x 0f0))
                           (setf (aref ,vertices (+ j 1)) (float y 0f0))
                           (setf (aref ,vertices (+ j 2)) (float z 0f0)))
                         (setf (gethash c ,face-table) i)
                         (incf i)))))
              (,finalizer ()
                (values (make-array (length ,vertices) :element-type 'single-float :initial-contents ,vertices)
                        (make-array (length ,faces) :element-type '(unsigned-byte 32) :initial-contents ,faces))))
         ,@body))))

(defmethod coerce-object ((primitive primitive) (type (eql 'general-mesh)) &rest args &key &allow-other-keys)
  (apply #'coerce-object primitive 'convex-mesh args))

(defmethod coerce-object ((primitive sphere) (type (eql 'convex-mesh)) &key (segments 32))
  (with-mesh-construction (v)
    (let ((lat (float segments 0f0))
          (lng (float segments 0f0))
          (size (sphere-radius primitive)))
      (loop for i from lat downto 1
            for lat0 = (* F-PI (- (/ (1- i) lat) 0.5))
            for lat1 = (* F-PI (- (/ i lat) 0.5))
            for z0 = (sin lat0)
            for zr0 = (cos lat0)
            for z1 = (sin lat1)
            for zr1 = (cos lat1)
            when (< zr0 0.0001)
              do (setf zr0 0.0)
            when (< zr1 0.0001)
              do (setf zr1 0.0)
            do (loop for j from lng downto 1
                     for l1 = (* F-2PI (/ (- j 1) lng))
                     for l2 = (* F-2PI (/ (- j 2) lng))
                     for x1 = (cos l1) for x2 = (cos l2)
                     for y1 = (sin l1) for y2 = (sin l2)
                     unless (= zr0 0)
                       do (v (* x1 zr0 size) (* y1 zr0 size) (* z0 size))
                          (v (* x1 zr1 size) (* y1 zr1 size) (* z1 size))
                          (v (* x2 zr0 size) (* y2 zr0 size) (* z0 size))
                     unless (= zr1 0)
                       do (v (* x2 zr0 size) (* y2 zr0 size) (* z0 size))
                          (v (* x1 zr1 size) (* y1 zr1 size) (* z1 size))
                          (v (* x2 zr1 size) (* y2 zr1 size) (* z1 size)))))
    (multiple-value-bind (vertices faces) (finalize)
      (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces))))

(defmethod coerce-object ((primitive plane) (type (eql 'convex-mesh)) &key)
  (with-mesh-construction (v)
    (let* ((n (plane-normal primitive))
           (o (plane-offset primitive))
           (q (qtowards +vy3+ n))
           (a (vec -5 0 +5))
           (b (vec +5 0 +5))
           (c (vec +5 0 -5))
           (d (vec -5 0 -5)))
      (nv+* (!q* a q a) n o)
      (nv+* (!q* b q b) n o)
      (nv+* (!q* c q c) n o)
      (nv+* (!q* d q d) n o)
      (v (vx a) (vy a) (vz a)) (v (vx b) (vy b) (vz b)) (v (vx c) (vy c) (vz c))
      (v (vx c) (vy c) (vz c)) (v (vx d) (vy d) (vz d)) (v (vx a) (vy a) (vz a))
      ;; Make it double-faced.
      (v (vx a) (vy a) (vz a)) (v (vx c) (vy c) (vz c)) (v (vx b) (vy b) (vz b))
      (v (vx c) (vy c) (vz c)) (v (vx a) (vy a) (vz a)) (v (vx d) (vy d) (vz d)))
    (multiple-value-bind (vertices faces) (finalize)
      (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces))))

(defmethod coerce-object ((primitive box) (type (eql 'convex-mesh)) &key)
  (with-mesh-construction (v)
    (let ((w (vx (box-bsize primitive)))
          (h (vy (box-bsize primitive)))
          (d (vz (box-bsize primitive))))
      (v (+ w) (+ h) (- d)) (v (- w) (+ h) (- d)) (v (- w) (+ h) (+ d))
      (v (- w) (+ h) (+ d)) (v (+ w) (+ h) (+ d)) (v (+ w) (+ h) (- d))
      (v (+ w) (- h) (+ d)) (v (- w) (- h) (+ d)) (v (- w) (- h) (- d))
      (v (- w) (- h) (- d)) (v (+ w) (- h) (- d)) (v (+ w) (- h) (+ d))
      (v (+ w) (+ h) (+ d)) (v (- w) (+ h) (+ d)) (v (- w) (- h) (+ d))
      (v (- w) (- h) (+ d)) (v (+ w) (- h) (+ d)) (v (+ w) (+ h) (+ d))
      (v (+ w) (- h) (- d)) (v (- w) (- h) (- d)) (v (- w) (+ h) (- d))
      (v (- w) (+ h) (- d)) (v (+ w) (+ h) (- d)) (v (+ w) (- h) (- d))
      (v (- w) (+ h) (+ d)) (v (- w) (+ h) (- d)) (v (- w) (- h) (- d))
      (v (- w) (- h) (- d)) (v (- w) (- h) (+ d)) (v (- w) (+ h) (+ d))
      (v (+ w) (+ h) (- d)) (v (+ w) (+ h) (+ d)) (v (+ w) (- h) (+ d))
      (v (+ w) (- h) (+ d)) (v (+ w) (- h) (- d)) (v (+ w) (+ h) (- d)))
    (multiple-value-bind (vertices faces) (finalize)
      (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces))))

(defmethod coerce-object ((primitive cylinder) (type (eql 'convex-mesh)) &key (segments 32))
  (with-mesh-construction (v)
    (let ((s (cylinder-radius primitive))
          (h (cylinder-height primitive)))
      (loop with step = (/ F-2PI segments)
            for i1 = (- step) then i2
            for i2 from 0 to F-2PI by step
            do ;; Bottom disc
            (v (* s (cos i2)) (- h) (* s (sin i2)))
            (v 0.0            (- h) 0.0)
            (v (* s (cos i1)) (- h) (* s (sin i1)))
            ;; Top Disc
            (v 0.0            (+ h) 0.0)
            (v (* s (cos i2)) (+ h) (* s (sin i2)))
            (v (* s (cos i1)) (+ h) (* s (sin i1)))
            ;; Wall
            (v (* s (cos i2)) (- h) (* s (sin i2)))
            (v (* s (cos i1)) (- h) (* s (sin i1)))
            (v (* s (cos i2)) (+ h) (* s (sin i2)))
            (v (* s (cos i1)) (+ h) (* s (sin i1)))
            (v (* s (cos i2)) (+ h) (* s (sin i2)))
            (v (* s (cos i1)) (- h) (* s (sin i1)))))
    (multiple-value-bind (vertices faces) (finalize)
      (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces))))

(defmethod coerce-object ((primitive pill) (type (eql 'convex-mesh)) &key (segments 32))
  (with-mesh-construction (v)
    (let ((s (pill-radius primitive))
          (h (pill-height primitive))
          (lat (float segments 0f0))
          (lng (float segments 0f0)))
      (loop with step = (/ F-2PI segments)
            for i1 = (- step) then i2
            for i2 from 0 to F-2PI by step
            do ;; Wall
            (v (* s (cos i2)) (- h) (* s (sin i2)))
            (v (* s (cos i1)) (- h) (* s (sin i1)))
            (v (* s (cos i2)) (+ h) (* s (sin i2)))
            (v (* s (cos i1)) (+ h) (* s (sin i1)))
            (v (* s (cos i2)) (+ h) (* s (sin i2)))
            (v (* s (cos i1)) (- h) (* s (sin i1))))
      (flet ((cap (h lng-start lng-end)
               (loop for i from lat downto 1
                     for lat0 = (* F-PI (- (/ (1- i) lat) 0.5))
                     for lat1 = (* F-PI (- (/ i lat) 0.5))
                     for z0 = (sin lat0)
                     for zr0 = (cos lat0)
                     for z1 = (sin lat1)
                     for zr1 = (cos lat1)
                     do (loop for j from lng-start downto lng-end
                              for l1 = (* F-2PI (/ (- j 1) lng))
                              for l2 = (* F-2PI (/ (- j 2) lng))
                              for x1 = (cos l1) for x2 = (cos l2)
                              for y1 = (sin l1) for y2 = (sin l2)
                              do (v (* x1 zr0 s) (+ h (* y1 zr0 s)) (* z0 s))
                                 (v (* x1 zr1 s) (+ h (* y1 zr1 s)) (* z1 s))
                                 (v (* x2 zr0 s) (+ h (* y2 zr0 s)) (* z0 s))
                                 (v (* x2 zr0 s) (+ h (* y2 zr0 s)) (* z0 s))
                                 (v (* x1 zr1 s) (+ h (* y1 zr1 s)) (* z1 s))
                                 (v (* x2 zr1 s) (+ h (* y2 zr1 s)) (* z1 s))))))
        (cap (+ h) (1+ (truncate lng 2)) 2)
        (cap (- h) (1+ lng) (+ (truncate lng 2) 2))))
    (multiple-value-bind (vertices faces) (finalize)
      (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces))))

(defmethod coerce-object ((primitive triangle) (type (eql 'convex-mesh)) &key)
  (with-mesh-construction (v)
    (let ((a (triangle-a primitive))
          (b (triangle-b primitive))
          (c (triangle-c primitive)))
      (v (vx a) (vy a) (vz a))
      (v (vx b) (vy b) (vz b))
      (v (vx c) (vy c) (vz c))
      ;; Make it double-faced.
      (v (vx b) (vy b) (vz b))
      (v (vx a) (vy a) (vz a))
      (v (vx c) (vy c) (vz c)))
    (multiple-value-bind (vertices faces) (finalize)
      (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces))))

(defmethod coerce-object ((primitive general-mesh) (type (eql 'sphere)) &key)
  (let ((vertices (general-mesh-vertices primitive))
        (max 0.0))
    (loop for i from 0 below (length vertices) by 3
          for dist = (+ (expt (aref vertices (+ i 0)) 2)
                        (expt (aref vertices (+ i 1)) 2)
                        (expt (aref vertices (+ i 2)) 2))
          do (setf max (max max dist)))
    (make-primitive-like primitive #'make-sphere :radius (sqrt max))))

(defmethod coerce-object ((primitive general-mesh) (type (eql 'box)) &key)
  (let ((vertices (general-mesh-vertices primitive))
        (max (vec 0 0 0)))
    ;; TODO: This does not try to adjust the rotation of the resulting primitive to fit better.
    ;;       ideally we'd first try to find the ideal orientation along which to fit the bounding
    ;;       box, then determine the size along that orientation, and adjust the resulting
    ;;       primitive's transforms
    (loop for i from 0 below (length vertices) by 3
          do (setf (vx max) (max (vx max) (abs (aref vertices (+ i 0)))))
             (setf (vy max) (max (vy max) (abs (aref vertices (+ i 1)))))
             (setf (vz max) (max (vz max) (abs (aref vertices (+ i 2))))))
    (make-primitive-like primitive #'make-box :bsize max)))

(defmethod coerce-object ((primitive general-mesh) (type (eql 'cylinder)) &key)
  (let ((vertices (general-mesh-vertices primitive))
        (height 0.0) (radius 0.0))
    ;; TODO: This does not try to adjust the rotation of the resulting primitive to fit better.
    ;;       ideally we'd first try to find the ideal orientation along which to fit the bounding
    ;;       cylinder, then determine the size along that orientation, and adjust the resulting
    ;;       primitive's transforms
    (loop for i from 0 below (length vertices) by 3
          do (setf height (max height (abs (aref vertices (+ i 1)))))
             (setf radius (max radius (abs (aref vertices (+ i 0)))))
             (setf radius (max radius (abs (aref vertices (+ i 2))))))
    (make-primitive-like primitive #'make-cylinder :radius radius :height height)))

(defmethod coerce-object ((primitive general-mesh) (type (eql 'pill)) &key)
  (implement!))

(defmethod coerce-object ((primitive general-mesh) (type (eql 'convex-mesh)) &key)
  (multiple-value-bind (vertices faces)
      (org.shirakumo.fraf.quickhull:convex-hull (general-mesh-vertices primitive))
    (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces)))

(defmethod make-vertex-array ((primitive primitive) vao)
  (let* ((mesh (coerce-object primitive 'general-mesh))
         (vbo (make-instance 'vertex-buffer :buffer-data (general-mesh-vertices mesh)))
         (ebo (make-instance 'vertex-buffer :buffer-data (general-mesh-faces mesh)
                                            :buffer-type :element-array-buffer
                                            :element-type :unsigned-int)))
    (ensure-instance vao 'vertex-array :index-buffer ebo :bindings `((,vbo :size 3)))))

(defmethod coerce-object ((primitive primitive) (type (eql 'mesh-data)) &rest args &key &allow-other-keys)
  (apply #'coerce-object (make-vertex-array primitive NIL) 'mesh-data args))

(defmethod coerce-object ((mesh mesh-data) (type (eql 'primitive)) &rest args &key &allow-other-keys)
  (apply #'make-general-mesh
         :vertices (reordered-vertex-data mesh '(location))
         :faces (faces mesh)
         args))

(defmethod replace-vertex-data (target (primitive primitive) &rest args &key &allow-other-keys)
  (apply #'replace-vertex-data target (coerce-object primitive 'mesh-data) args))

(defun convexify (primitives)
  (let ((new (make-array 0 :adjustable T :fill-pointer T)))
    (loop for primitive across primitives
          do (etypecase primitive
               ((and general-mesh (not convex-mesh))
                (let ((hulls (org.shirakumo.fraf.convex-covering:decompose
                              (general-mesh-vertices primitive)
                              (general-mesh-faces primitive))))
                  (loop for hull across hulls
                        for mesh = (make-convex-mesh :vertices (org.shirakumo.fraf.convex-covering:vertices hull)
                                                     :faces (org.shirakumo.fraf.convex-covering:faces hull)
                                                     :material (primitive-material primitive)
                                                     :local-transform (mcopy (primitive-local-transform primitive)))
                        do (vector-push-extend mesh new))))
               (primitive
                (vector-push-extend primitive new))))
    (simplify new)))
