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
                      (setf (hit-b hit) ,(if (subtypep b 'primitive) `(primitive-entity b) b))
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

(defstruct (sphere (:include primitive))
  (radius 1.0 :type single-float))

(define-accessor-delegate-methods radius (sphere-radius sphere))

(defstruct (plane (:include primitive))
  (normal (vec3 0 1 0) :type vec3)
  (offset 0.0 :type single-float))

(define-accessor-delegate-methods normal (plane-normal plane))
(define-accessor-delegate-methods offset (plane-offset plane))

(defstruct (half-space (:include plane)))

;; NOTE: the box is centred at 0,0,0 and the bsize is the half-size along each axis.
(defstruct (box (:include primitive))
  (bsize (vec3 1 1 1) :type vec3))

(define-accessor-delegate-methods bsize (box-bsize box))

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
  (3d-matrices::with-floats ((fpi PI) (f360 360) (fovy fovy) (aspect aspect) (near near) (far far))
    (let* ((fh (* (the single-float (tan (* (/ fovy f360) fpi))) near))
           (fw (* fh aspect)))
      (frustum-box (- fw) fw (- fh) fh near far))))

;; NOTE: the cylinder is centred at 0,0,0 and points Y-up. the "height" is the half-height.
(defstruct (cylinder (:include primitive))
  (radius 1.0 :type single-float)
  (height 1.0 :type single-float))

(define-accessor-delegate-methods radius (cylinder-radius cylinder))
(define-accessor-delegate-methods height (cylinder-height cylinder))

(defstruct (pill (:include cylinder)))

(defstruct (triangle (:include primitive))
  (a (vec3 0 0 0) :type vec3)
  (b (vec3 0 0 0) :type vec3)
  (c (vec3 0 0 0) :type vec3))

(define-hit-detector (sphere sphere)
  (let* ((al (global-location a))
         (bl (global-location b))
         (dx (v- al bl))
         (len (vlength dx)))
    (when (and (<= (+ (sphere-radius a) (sphere-radius b)) len)
               (< 0 len))
      (v<- (hit-normal hit) dx)
      (nv/ (hit-normal hit) len)
      (v<- (hit-location hit) al)
      (nv+* (hit-location hit) dx 0.5)
      (setf (hit-depth hit) (- (+ (sphere-radius a) (sphere-radius b)) len))
      (finish-hit))))

(define-hit-detector (sphere half-space)
  (let* ((al (global-location a))
         (dist (- (v. (plane-normal b) al)
                  (sphere-radius a)
                  (plane-offset b))))
    (when (< dist 0)
      (v<- (hit-normal hit) (plane-normal b))
      (setf (hit-depth hit) (- dist))
      (v<- (hit-location hit) al)
      (nv+* (hit-location hit) (plane-normal b) (- (+ dist (sphere-radius a))))
      (finish-hit))))

(define-hit-detector (sphere plane)
  (let* ((al (global-location a))
         (dist (- (v. (plane-normal b) al)
                  (plane-offset b))))
    (when (< (* dist dist) (* (sphere-radius a) (sphere-radius a)))
      (v<- (hit-normal hit) (plane-normal b))
      (setf (hit-depth hit) (- dist))
      (when (< dist 0)
        (nv- (hit-normal hit))
        (setf (hit-depth hit) (- (hit-depth hit))))
      (incf (hit-depth hit) (sphere-radius a))
      (v<- (hit-location hit) al)
      (nv+* (hit-location hit) (plane-normal b) dist)
      (finish-hit))))

(define-hit-detector (box half-space)
  (let* ((bs (box-bsize a))
         (tf (primitive-transform a))
         (pd (plane-normal b))
         (po (plane-offset b))
         (a (vec3 (+ (vx bs)) (+ (vy bs)) (+ (vz bs))))
         (b (vec3 (- (vx bs)) (+ (vy bs)) (+ (vz bs))))
         (c (vec3 (+ (vx bs)) (- (vy bs)) (+ (vz bs))))
         (d (vec3 (- (vx bs)) (- (vy bs)) (+ (vz bs))))
         (e (vec3 (+ (vx bs)) (+ (vy bs)) (- (vz bs))))
         (f (vec3 (- (vx bs)) (+ (vy bs)) (- (vz bs))))
         (g (vec3 (+ (vx bs)) (- (vy bs)) (- (vz bs))))
         (h (vec3 (- (vx bs)) (- (vy bs)) (- (vz bs)))))
    (declare (dynamic-extent a b c d e f g h))
    (flet ((test (p)
             (n*m tf p)
             (let ((dist (v. p pd)))
               (when (<= dist po)
                 (v<- (hit-location hit) pd)
                 (nv* (hit-location hit) (- dist po))
                 (nv+ (hit-location hit) p)
                 (v<- (hit-normal hit) pd)
                 (setf (hit-depth hit) (- po dist))
                 (finish-hit)))))
      (test a)
      (test b)
      (test c)
      (test d)
      (test e)
      (test f)
      (test g)
      (test h))))

(define-hit-detector (sphere box)
  (let ((center (global-location a))
        (radius (sphere-radius a))
        (bs (box-bsize b)))
    (ntransform-inverse center (box-transform b))
    (unless (or (< (vx bs) (- (abs (vx center)) radius))
                (< (vy bs) (- (abs (vy center)) radius))
                (< (vz bs) (- (abs (vz center)) radius)))
      (let ((closest (vec 0 0 0))
            (dist 0.0))
        (macrolet ((test-axis (axis)
                     `(progn
                        (setf dist (,axis center))
                        (when (< (,axis bs) dist) (setf dist (,axis bs)))
                        (when (< dist (- (,axis bs))) (setf dist (- (,axis bs))))
                        (setf (,axis closest) dist))))
          (test-axis vx3)
          (test-axis vy3)
          (test-axis vz3))
        (setf dist (vsqrdistance closest center))
        (unless (< (* radius radius) dist)
          (n*m (box-transform b) closest)
          (v<- (hit-normal hit) center)
          (nv- (hit-normal hit) closest)
          (nvunit (hit-normal hit))
          (v<- (hit-location hit) closest)
          (setf (hit-depth hit) (- radius (sqrt dist)))
          (finish-hit))))))

(defun box-to-axis (box axis)
  (let ((bs (box-bsize box))
        (tf (box-transform box))
        (col (vec3 0 0 0)))
    (declare (dynamic-extent col))
    (+ (* (vx bs) (abs (v. axis (mcol3 tf 0 col))))
       (* (vy bs) (abs (v. axis (mcol3 tf 1 col))))
       (* (vz bs) (abs (v. axis (mcol3 tf 2 col)))))))

(defun box-depth-on-axis (a b axis center)
  (- (+ (box-to-axis a axis)
        (box-to-axis b axis))
     (abs (v. center axis))))

(defun box-contact-point (apoint aaxis asize bpoint baxis bsize one-p)
  (let* ((a-sqrlen (vsqrlength aaxis))
         (b-sqrlen (vsqrlength baxis))
         (a-b (v. aaxis baxis))
         (to-st (v- apoint bpoint))
         (a-sta (v. aaxis to-st))
         (b-sta (v. baxis to-st))
         (denominator (- (* a-sqrlen b-sqrlen) (* a-b a-b))))
    (cond ((< (abs denominator) 0.0001)  ; Some kinda precision constant
           (if one-p apoint bpoint))
          (T
           (let ((mua (/ (- (* a-b b-sta) (* b-sqrlen a-sta)) denominator))
                 (mub (/ (- (* a-sqrlen b-sta) (* a-b a-sta)) denominator)))
             (cond ((or (< asize mua)
                        (< mua (- asize))
                        (< bsize mub)
                        (< mub (- bsize)))
                    (if one-p apoint bpoint))
                   (T
                    (nv+ (nv* (nv+ (v* aaxis mua) apoint) 0.5)
                         (nv* (nv+ (v* baxis mub) bpoint) 0.5)))))))))

(define-hit-detector (box vec3)
    (let* ((atf (primitive-transform a))
           (rel (ntransform-inverse b atf))
           (bsize (box-bsize a))
           (normal (hit-normal hit))
           (min-depth most-positive-single-float))
      (macrolet ((try-axis (axis i)
                   `(let ((depth (- (,axis bsize) (abs (,axis rel)))))
                      (when (< depth 0) (return))
                      (when (< depth min-depth)
                        (setf min-depth depth)
                        (mcol3 atf ,i normal)
                        (when (< (,axis rel)) (nv- normal))))))
        (try-axis vx 0)
        (try-axis vy 1)
        (try-axis vz 2))
      (v<- (hit-location hit) b)
      (setf (hit-depth hit) min-depth)
      (finish-hit)))

(define-hit-detector (box box)
    (let* ((smallest-depth most-positive-single-float)
           (smallest-single most-positive-fixnum)
           (smallest most-positive-fixnum)
           (atf (box-transform a))
           (btf (box-transform b))
           (center (nv- (mcol3 btf 3) (mcol3 atf 3))))
      (macrolet ((try-axis (axis i)
                   `(let ((axis ,axis))
                      (when (<= 0.0001 (vsqrlength axis))
                        (nvunit axis)
                        (let ((new-depth (box-depth-on-axis a b axis center)))
                          (cond ((< new-depth 0)
                                 (return))
                                ((< new-depth smallest-depth)
                                 (setf smallest-depth new-depth)
                                 (setf smallest ,i)
                                 NIL)))))))
        (try-axis (mcol3 atf 0) 0)
        (try-axis (mcol3 atf 1) 1)
        (try-axis (mcol3 atf 2) 2)
        (try-axis (mcol3 btf 0) 3)
        (try-axis (mcol3 btf 1) 4)
        (try-axis (mcol3 btf 2) 5)
        (setf smallest-single smallest)
        (try-axis (vc (mcol3 atf 0) (mcol3 btf 0)) 6)
        (try-axis (vc (mcol3 atf 0) (mcol3 btf 1)) 7)
        (try-axis (vc (mcol3 atf 0) (mcol3 btf 2)) 8)
        (try-axis (vc (mcol3 atf 1) (mcol3 btf 0)) 9)
        (try-axis (vc (mcol3 atf 1) (mcol3 btf 1)) 10)
        (try-axis (vc (mcol3 atf 1) (mcol3 btf 2)) 11)
        (try-axis (vc (mcol3 atf 2) (mcol3 btf 0)) 12)
        (try-axis (vc (mcol3 atf 2) (mcol3 btf 1)) 13)
        (try-axis (vc (mcol3 atf 2) (mcol3 btf 2)) 14))
      (flet ((point-face ()
               (let ((normal (mcol3 atf smallest)))
                 (when (< 0 (v. normal center))
                   (nv- normal))
                 (let ((vert (vcopy (box-bsize b))))
                   (when (< (v. (mcol3 btf 0) normal) 0) (setf (vx vert) (- (vx vert))))
                   (when (< (v. (mcol3 btf 1) normal) 0) (setf (vy vert) (- (vy vert))))
                   (when (< (v. (mcol3 btf 2) normal) 0) (setf (vz vert) (- (vz vert))))
                   (v<- (hit-normal hit) normal)
                   (v<- (hit-location hit) (n*m btf vert))
                   (setf (hit-depth hit) smallest-depth)
                   (finish-hit)))))
        (cond ((< smallest 3)
               (point-face))
              ((< smallest 6)
               ;; Same algo but in reverse, so just flip it.
               (rotatef a b)
               (rotatef atf btf)
               (decf smallest 3)
               (nv- center)
               (point-face))
              (T
               (decf smallest 6)
               (let* ((aaxis-idx (floor smallest 3))
                      (baxis-idx (mod smallest 3))
                      (aaxis (mcol3 atf aaxis-idx))
                      (baxis (mcol3 btf baxis-idx))
                      (axis (nvunit (vc aaxis baxis)))
                      (aedge-point (vcopy (box-bsize a)))
                      (bedge-point (vcopy (box-bsize b))))
                 (when (< 0 (v. axis center))
                   (nv- axis))

                 ;; WTF
                 (macrolet ((set-edge (var val)
                              `(ecase i
                                 (0 (setf (vx3 ,var) ,val))
                                 (1 (setf (vy3 ,var) ,val))
                                 (2 (setf (vz3 ,var) ,val))))
                            (vidx (idx var)
                              `(ecase ,idx
                                 (0 (vx3 ,var))
                                 (1 (vy3 ,var))
                                 (2 (vz3 ,var)))))
                   (dotimes (i 3)
                     (cond ((= i aaxis-idx)
                            (set-edge aedge-point 0.0))
                           ((< 0 (v. (mcol3 atf i) axis))
                            (set-edge aedge-point (- (vidx i aedge-point)))))
                     (cond ((= i baxis-idx)
                            (set-edge bedge-point 0.0))
                           ((< (v. (mcol3 btf i) axis) 0)
                            (set-edge bedge-point (- (vidx i bedge-point))))))
                   
                   (n*m atf aedge-point)
                   (n*m btf bedge-point)

                   (setf (hit-depth hit) smallest-depth)
                   (v<- (hit-normal hit) axis)
                   (v<- (hit-location hit) (box-contact-point aedge-point aaxis (vidx aaxis-idx (box-bsize a))
                                                              bedge-point baxis (vidx baxis-idx (box-bsize b))
                                                              (< 2 smallest-single)))
                   (finish-hit))))))))
