#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defun primary-monitor-modes ()
  (cl-monitors:modes (find-if #'cl-monitors:primary-p (cl-monitors:detect))))

(define-widget launcher (QDialog)
  ((context :initform (make-instance 'context))))

(define-subwidget (launcher resolution) (q+:make-qcombobox launcher)
  (dolist (mode (primary-monitor-modes))
    (q+:add-item resolution
                 (format NIL "~ax~a @ ~a Hz"
                         (cl-monitors:width mode)
                         (cl-monitors:height mode)
                         (cl-monitors:refresh mode)))))

(define-subwidget (launcher vsync) (q+:make-qcombobox launcher)
  (q+:add-items vsync '("Off" "Adaptive" "Strict")))

(define-subwidget (launcher samples) (q+:make-qcombobox launcher)
  (let ((max (with-context (context)
               (gl:get* :max-samples))))
    (q+:add-item samples "Off")
    (loop for s = 2 then (* 2 s)
          while (<= s max)
          do (q+:add-item samples (format NIL "~ax" s)))))

(define-subwidget (launcher filtering) (q+:make-qcombobox launcher)
  (q+:add-items filtering '("Nearest" "Linear" "Bilinear" "Trilinear"))
  (let ((max (with-context (context)
               (when (gl:extension-present-p "GL_EXT_texture_filter_anisotropic")
                 (gl:get-float :max-texture-max-anisotropy-ext 1)))))
    (loop for s = 2 then (* 2 s)
          while (<= s max)
          do (q+:add-item filtering (format NIL "~ax Anisotropic" s)))))

(define-subwidget (launcher fov) (make-instance 'qui:slider :minimum 10 :maximum 200 :stepping 1
                                                            :default 75))

(define-subwidget (launcher ok) (q+:make-qpushbutton "Ok" launcher)
  (connect! ok (clicked) launcher (accept)))

(define-subwidget (launcher cancel) (q+:make-qpushbutton "Cancel" launcher)
  (connect! cancel (clicked) launcher (reject)))

(define-subwidget (launcher layout) (q+:make-qformlayout launcher)
  (q+:add-row layout "Resolution" resolution)
  (q+:add-row layout "VSync" vsync)
  (q+:add-row layout "Antialiasing" samples)
  (q+:add-row layout "Filtering" filtering)
  (q+:add-row layout "FOV" fov)
  (q+:add-row layout cancel ok))

(defun init-options (launcher)
  (with-slots-bound (launcher launcher)
    (let* ((mode (nth (q+:current-index resolution) (primary-monitor-modes)))
           (vsync (cond ((string= "Off" (q+:current-text vsync)) 0)
                        ((string= "Adaptive" (q+:current-text vsync)) -1)
                        ((string= "Strict" (q+:current-text vsync)) (cl-monitors:refresh mode))))
           (samples (parse-integer (q+:current-text samples)))
           (filtering (cond ((string= "Nearest" (q+:current-text filtering)) :nearest)
                            ((string= "Linear" (q+:current-text filtering)) :linear)
                            ((string= "Bilinear" (q+:current-text filtering)) :linear-mipmap-nearest)
                            (T :linear-mipmap-linear)))
           (anisotropic (float (parse-integer (q+:current-text filtering)) 0.0f0)))
      (list :resolution mode
            :swap-interval vsync
            :multisampling (when samples T)
            :samples samples
            :filtering filtering
            :anisotropic anisotropic
            :fov (qui:value fov)))))
