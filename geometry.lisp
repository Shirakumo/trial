#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-pool geometry
  :base 'trial)

(define-asset (geometry fullscreen-square) packed-vao-asset
    (#(0 1 2 2 3 0)
     3 #(+1.0 +1.0 +0.0
         +1.0 -1.0 +0.0
         -1.0 -1.0 +0.0
         -1.0 +1.0 +0.0)
     2 #(1.0 1.0
         1.0 0.0
         0.0 0.0
         0.0 1.0)))

;; Factor out into function that generates this
;; based on requested size, whether it should be
;; textured or shaded, etc.
(define-asset (geometry cube) packed-vao-asset
    (#( 0  1  2  2  3  0
       4  5  6  6  7  4
       8  9 10 10 11  8
       12 13 14 14 15 12
       16 17 18 18 19 16
       20 21 22 22 23 20)
     3 #(+0.5 +0.5 -0.5
         -0.5 +0.5 -0.5
         -0.5 +0.5 +0.5
         +0.5 +0.5 +0.5
         
         +0.5 -0.5 +0.5
         -0.5 -0.5 +0.5
         -0.5 -0.5 -0.5
         +0.5 -0.5 -0.5

         +0.5 +0.5 +0.5
         -0.5 +0.5 +0.5
         -0.5 -0.5 +0.5
         +0.5 -0.5 +0.5

         +0.5 -0.5 -0.5
         -0.5 -0.5 -0.5
         -0.5 +0.5 -0.5
         +0.5 +0.5 -0.5

         -0.5 +0.5 +0.5
         -0.5 +0.5 -0.5
         -0.5 -0.5 -0.5
         -0.5 -0.5 +0.5

         +0.5 +0.5 -0.5
         +0.5 +0.5 +0.5
         +0.5 -0.5 +0.5
         +0.5 -0.5 -0.5)
     2 #(1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0

         1.0 1.0
         0.0 1.0
         0.0 0.0
         1.0 0.0)))

(defclass geometry (entity)
  ())

(defclass bounded-geometry (geometry)
  ((width :initarg :width :initform 10 :accessor width)
   (height :initarg :height :initform 10 :accessor height)))

(define-saved-slots bounded-geometry width height)

(defclass sized-geometry (geometry)
  ((size :initarg :size :initform 10 :accessor size)))

(define-saved-slots sized-geometry size)

(defclass segmented-geometry (geometry)
  ((segments :initarg :segments :initform 8 :accessor segments)))

(define-saved-slots segmented-geometry segments)
