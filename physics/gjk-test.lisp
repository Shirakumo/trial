(defpackage trial/tests/gjk
  (:use :cl
        :3d-matrices
        :3d-vectors
        :parachute))

(in-package :trial/tests/gjk)

(define-test test-1
  (true t))

(define-test intersecting-sphere-test
  (true (loop repeat 100000 always
              (let* ((location-1 (trial::vec3-random -1.0 1.0))
                     (location-2 (trial::vec3-random -1.0 1.0))
                     (distance-between-origins (3d-vectors:v2norm
                                                (3d-vectors:v- location-1 location-2)))
                     (radius-1 (random distance-between-origins))
                     (radius-2 (+ 0.1 (- distance-between-origins radius-1))))
                (trial::gjk (trial:make-sphere :transform (mtranslation location-1) :radius radius-1)
                     (trial:make-sphere :transform (mtranslation location-2) :radius radius-2))))))

(define-test non-intersecting-sphere-test
  (true (loop repeat 100000 never
                         (let* ((location-1 (trial::vec3-random -1.0 1.0))
                                (location-2 (trial::vec3-random -1.0 1.0))
                                (distance-between-origins (3d-vectors:v2norm
                                                           (3d-vectors:v- location-1 location-2)))
                                (radius-1 (random distance-between-origins))
                                (radius-2 (max 0.0 (- (- distance-between-origins radius-1) 0.1))))
                           (trial::gjk (trial:make-sphere :transform (mtranslation location-1) :radius radius-1)
                                (trial:make-sphere :transform (mtranslation location-2) :radius radius-2))))))

;; (define-test intersecting-mesh-test
;;   (true nil))

;; (define-test non-intersecting-mesh-test
;;   (true nil))

;; (define-test intersecting-mesh-and-sphere-test
;;   (true nil))

;; (define-test non-intersecting-mesh-and-sphere-test
;;   (true nil))

