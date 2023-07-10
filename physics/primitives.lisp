(in-package #:org.shirakumo.fraf.trial)

(defgeneric intersects-p (a b))
(defgeneric detect-hits (a b contacts start end))

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
    (not (null (detect-hit a b hit)))))

(defmacro define-hit-detector ((a b) &body body)
  `(progn
     (defmethod detect-hits ((a ,a) (b ,b) hits start end)
       (declare (type (unsigned-byte 32) start end))
       (declare (type (simple-vector ,(1- (ash 1 32)))))
       (when (<= end start)
         (return-from detect-hits start))
       (let ((hit (aref hits start)))
         (block NIL
           (flet ((finish-hit ()
                    #-trial-release (when (v= 0 (hit-normal hit)) (error "Hit normal not set correctly."))
                    (let ((properties (material-interaction-properties
                                       (primitive-material a) ,(if (subtypep b 'primitive) `(primitive-material b) NIL))))
                      (setf (hit-a hit) (primitive-entity a))
                      (setf (hit-b hit) ,(if (subtypep b 'primitive) `(primitive-entity b) 'b))
                      (setf (hit-static-friction hit) (material-interaction-properties-static-friction properties))
                      (setf (hit-dynamic-friction hit) (material-interaction-properties-dynamic-friction properties))
                      (setf (hit-restitution hit) (material-interaction-properties-restitution properties)))
                    (incf start)
                    (if (< start end)
                        (setf hit (aref hits start))
                        (return))))
             ,@body))
         start))
     ,@(unless (eql a b)
         `((defmethod detect-hits ((a ,b) (b ,a) hits start end)
             (detect-hits b a hits start end))))))

(defstruct primitive
  (entity NIL :type T)
  (material NIL :type T)
  (local-transform (meye 4) :type mat4)
  (transform (mat4) :type mat4))

(defmethod global-location ((primitive primitive))
  (with-fast-matref (m (primitive-transform primitive) 4)
    (vec (m 3) (m 7) (m 11))))

(defmethod global-orientation ((primitive primitive))
  (qfrom-mat (primitive-transform primitive)))

(defmethod location ((primitive primitive))
  (with-fast-matref (m (primitive-local-transform primitive) 4)
    (vec (m 3) (m 7) (m 11))))

(defmethod (setf location) ((vec vec3) (primitive primitive))
  (with-fast-matref (m (primitive-local-transform primitive) 4)
    (setf (m 3) (vx3 vec))
    (setf (m 7) (vy3 vec))
    (setf (m 11) (vz3 vec))
    vec))

(defmethod orientation ((primitive primitive))
  (qfrom-mat (primitive-local-transform primitive)))

(defmethod (setf orientation) ((quat quat) (primitive primitive))
  (let ((src (mat3))
        (dst (primitive-local-transform primitive)))
    (declare (dynamic-extent src))
    (qmat3 quat src)
    (with-fast-matref (s src 3)
      (with-fast-matref (d dst 4)
        (setf (d 0 0) (s 0 0) (d 0 1) (s 0 1) (d 0 2) (s 0 2))
        (setf (d 1 0) (s 1 0) (d 1 1) (s 1 1) (d 1 2) (s 1 2))
        (setf (d 2 0) (s 2 0) (d 2 1) (s 2 1) (d 2 2) (s 2 2))))
    quat))

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

(define-primitive-type plane
  (normal (vec3 0 1 0) :type vec3)
  (offset 0.0 :type single-float))

(define-primitive-type (half-space plane))

;; NOTE: the box is centred at 0,0,0 and the bsize is the half-size along each axis.
(define-primitive-type box
  (bsize (vec3 1 1 1) :type vec3))

;; Frustums are just boxes skewed by a linear transform. We provide these shorthands
;; here to allow easier construction of frustum testing primitives.
(defun make-frustum-box (left right bottom top near far)
  (let ((transform (mfrustum left right bottom top near far)))
    (make-box :local-transform transform
              :transform (mcopy4 transform)
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

(define-primitive-type (pill cylinder))

(define-primitive-type triangle
  (a (vec3 0 0 0) :type vec3)
  (b (vec3 0 0 0) :type vec3)
  (c (vec3 0 0 0) :type vec3))

(define-primitive-type general-mesh
  ;; NOTE: Packed vertex positions as X Y Z triplets
  ;; [ X0 Y0 Z0 X1 Y1 Z1 X2 Y2 Z2 X3 Y3 Z3 ... ]
  (vertices #() :type (simple-array single-float (*)))
  ;; NOTE: Vertex indices pointing into the vertex array / 3
  ;; [ 0 1 2 2 3 0 ... ]
  (faces #() :type (simple-array (unsigned-byte 32) (*))))

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
            do (loop for j from lng downto 0
                     for l1 = (* F-2PI (/ (- j 1) lng))
                     for l2 = (* F-2PI (/ (- j 2) lng))
                     for x1 = (cos l1) for x2 = (cos l2)
                     for y1 = (sin l1) for y2 = (sin l2)
                     do (v (* x1 zr0 size) (* y1 zr0 size) (* z0 size))
                        (v (* x1 zr1 size) (* y1 zr1 size) (* z1 size))
                        (v (* x2 zr0 size) (* y2 zr0 size) (* z0 size))
                        (v (* x2 zr0 size) (* y2 zr0 size) (* z0 size))
                        (v (* x1 zr1 size) (* y1 zr1 size) (* z1 size))
                        (v (* x2 zr1 size) (* y2 zr1 size) (* z1 size)))))
    (multiple-value-bind (vertices faces) (finalize)
      (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces))))

(defmethod coerce-object ((primitive plane) (type (eql 'convex-mesh)) &key)
  (implement!))

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
            (v 0.0            (- h) 0.0)
            (v (* s (cos i2)) (- h) (* s (sin i2)))
            (v (* s (cos i1)) (- h) (* s (sin i1)))
            ;; Top Disc
            (v 0.0            (+ h) 0.0)
            (v (* s (cos i2)) (+ h) (* s (sin i2)))
            (v (* s (cos i1)) (+ h) (* s (sin i1)))
            ;; Wall
            (v (* s (cos i2)) (- h) (* s (sin i2)))
            (v (* s (cos i2)) (+ h) (* s (sin i2)))
            (v (* s (cos i1)) (- h) (* s (sin i1)))
            (v (* s (cos i2)) (+ h) (* s (sin i2)))
            (v (* s (cos i2)) (- h) (* s (sin i2)))
            (v (* s (cos i1)) (+ h) (* s (sin i1)))))
    (multiple-value-bind (vertices faces) (finalize)
      (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces))))

(defmethod coerce-object ((primitive pill) (type (eql 'convex-mesh)) &key)
  (implement!))

(defmethod coerce-object ((primitive triangle) (type (eql 'convex-mesh)) &key)
  (with-mesh-construction (v)
    (let ((a (triangle-a primitive))
          (b (triangle-b primitive))
          (c (triangle-c primitive)))
      (v (vx a) (vy a) (vz a))
      (v (vx b) (vy b) (vz b))
      (v (vx c) (vy c) (vz c)))
    (multiple-value-bind (vertices faces) (finalize)
      (make-primitive-like primitive #'make-convex-mesh :vertices vertices :faces faces))))

(defmethod coerce-object ((primitive convex-mesh) (type (eql 'sphere)) &key)
  (let ((vertices (convex-mesh-vertices primitive))
        (max 0.0))
    (loop for i from 0 below (length vertices) by 3
          for dist = (+ (expt (aref vertices (+ i 0)) 2)
                        (expt (aref vertices (+ i 1)) 2)
                        (expt (aref vertices (+ i 2)) 2))
          do (setf max (max max dist)))
    (make-primitive-like primitive #'make-sphere :radius (sqrt max))))

(defmethod coerce-object ((primitive convex-mesh) (type (eql 'box)) &key)
  (let ((vertices (convex-mesh-vertices primitive))
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

(defmethod coerce-object ((primitive convex-mesh) (type (eql 'cylinder)) &key)
  (let ((vertices (convex-mesh-vertices primitive))
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

(defmethod coerce-object ((primitive convex-mesh) (type (eql 'pill)) &key)
  (implement!))

