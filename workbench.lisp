(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench noto) font
    (#p"noto-sans-regular.ttf"))

(defmethod paint :before ((source main) (target main))
  (let ((fps (unit :fps (scene source)))
        (tt (frame-time source)))
    (when (and fps (/= 0 tt))
      (setf (text fps) (format NIL "~a" (round (/ tt)))))))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (enter (make-instance 'text :font (asset 'workbench 'noto) :name :fps) scene)
      (enter (make-instance 'target-camera :location (vec 0 2 200)) scene)))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))
