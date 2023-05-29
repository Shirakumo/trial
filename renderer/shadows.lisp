#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass shadow-mapped-pass (standard-render-pass)
  ((allocated-shadow-casters :accessor allocated-shadow-casters)
   (shadow-map-block :reader shadow-map-block)
   (shadow-map-program :accessor shadow-map-program)))

(defmethod initialize-instance :after ((pass shadow-mapped-pass) &key (max-shadow-casters 8))
  (setf (allocated-shadow-casters pass) (make-lru-cache max-shadow-casters))
  (setf (slot-value pass 'shadow-map-block) (make-instance 'uniform-buffer :binding NIL :struct (make-instance 'shadow-map-block :size max-shadow-casters))))

(defmethod clear :after ((pass shadow-mapped-pass))
  (lru-cache-clear (allocated-shadow-casters pass)))

(defmethod enter :after ((light light) (pass shadow-mapped-pass))
  (when (cast-shadows-p light)
    (lru-cache-push light (allocated-shadow-casters pass))))

(defmethod leave :after ((light light) (pass shadow-mapped-pass))
  (when (cast-shadows-p light)
    (lru-cache-pop light (allocated-shadow-casters pass))))

(defmethod notice-update ((light light) (pass standard-render-pass))
  (if (cast-shadows-p light)
      (lru-cache-push light (allocated-shadow-casters pass))
      (lru-cache-pop light (allocated-shadow-casters pass))))

(defmethod render-frame :before ((pass light-cache-render-pass) frame)
  (let ((program (shadow-map-program pass)))
    (activate program)
    (loop for (object) across frame
          do (do-lru-cache (light id (allocated-shadow-casters pass))
               (with-buffer-tx (struct (shadow-map-block pass))
                 (transfer-to struct light))
               (render object program)))))
