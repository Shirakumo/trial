(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-pass fixed-render-pass (render-pass)
  ((color :port-type output :attachment :color-attachment0 :texspec (:width 64 :height 64 :mag-filter :nearest))
   (depth :port-type output :attachment :depth-stencil-attachment :texspec (:width 64 :height 64))))

;; By default the camera adjusts depending on the total screen size. We need to prevent this
;; since we always have a fixed render size.
(defclass fixed-view-camera (sidescroll-camera) ())
(defmethod handle ((ev resize) (camera fixed-view-camera))
  (vsetf (bsize camera) 32 32)
  (setup-perspective camera 64 64))

(define-example letterbox
  :title "Letterboxing"
  :slots ((disable-ui :initform T))
  (enter (make-instance 'tile-layer :tile-data (assets:asset :tilemap)) scene)
  (enter (make-instance 'fixed-view-camera :zoom 0.2) scene)
  (enter (make-instance 'fixed-render-pass) scene))
