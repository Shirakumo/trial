#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.qt)
(in-readtable :qtools)

(defun make-context (&optional handler)
  (make-instance 'context :handler handler))

(define-widget context (QGLWidget trial:context)
  ((glformat :initform NIL :accessor glformat)
   (glcontext :initform NIL :accessor glcontext)
   (previous-pos :initform NIL)))

(defmethod construct ((context context))
  (new context (glformat context))
  (let ((glcontext (q+:context context)))
    (if (q+:is-valid glcontext)
        (v:info :trial.context "~a successfully created context." context)
        (error "Failed to create context."))
    (acquire-context context)))

(defmethod shared-initialize :after ((context context)
                                     slots
                                     &key (accumulation-buffer NIL accumulation-buffer-p)
                                          (alpha-buffer NIL alpha-buffer-p)
                                          (depth-buffer NIL depth-buffer-p)
                                          (stencil-buffer NIL stencil-buffer-p)
                                          (stereo-buffer NIL stereo-buffer-p)
                                          (double-buffering NIL double-buffering-p)
                                          (profile NIL profile-p)
                                          (version NIL version-p)
                                     ;; Qt-exclusive options.
                                          (direct-rendering T direct-rendering-p)
                                          (overlay NIL overlay-p)
                                          (plane 0 plane-p)
                                          (multisampling T multisampling-p)
                                          (samples 1 samples-p)
                                          (swap-interval 0 swap-interval-p))
  (with-accessors ((format glformat)) context
    (unless format (setf format (q+:make-qglformat)))
    (macrolet ((maybe-set (variable setter)
                 `(when ,(intern (format NIL "~a-~a" variable :p))
                    (setf (q+ ,setter format) ,variable))))
      (maybe-set accumulation-buffer accum)
      (maybe-set alpha-buffer alpha)
      (maybe-set depth-buffer depth)
      (maybe-set stencil-buffer stencil)
      (maybe-set stereo-buffer stereo)
      (maybe-set direct-rendering direct-rendering)
      (maybe-set double-buffering double-buffer)
      (maybe-set overlay overlay)
      (maybe-set plane plane)
      (maybe-set multisampling sample-buffers)
      (maybe-set samples samples)
      (maybe-set swap-interval swap-interval))
    (when version-p
      (setf (q+:version format) (values (first version)
                                        (second version))))
    (when profile-p
      (setf (q+:profile format)
            (ecase profile
              (NIL (q+:qglformat.no-profile))
              (:core (q+:qglformat.core-profile))
              (:compatibility (q+:qglformat.compatibility-profile)))))))

(defmethod initialize-instance :after ((context context) &key width height title)
  (setf (q+:updates-enabled context) NIL)
  (setf (q+:auto-buffer-swap context) NIL)
  (setf (q+:focus-policy context) (q+:qt.strong-focus))
  (setf (q+:mouse-tracking context) T)
  (setf (q+:auto-fill-background context) NIL)
  (setf (q+:auto-buffer-swap context) NIL)
  (when (and width height)
    (q+:resize context (round width) (round height)))
  (when title
    (setf (q+:window-title context) title)))

(defmethod reinitialize-instance :after ((context context) &key width height title)
  (when (and width height)
    (q+:resize (round width) (round height)))
  (when title
    (setf (q+:window-title context) title)))

(defmethod finalize ((context context))
  (trial:finalize (handler context))
  (call-next-method)
  (finalize (glformat context)))

(defmethod create-context ((context context))
  (unless (q+:create (q+:context context))
    (error "Failed to recreate context. Game over.")))

(defmethod valid-p ((context context))
  (q+:is-valid context))

(defmethod destroy-context ((context context))
  (q+:reset (q+:context context)))

(defmethod (setf parent) (parent (context context))
  ;; This is so annoying because Microsoft® Windows®™©
  (with-context (context)
    #+windows (destroy-context context)
    (setf (q+:parent context) parent)
    #+windows (create-context context)))

(defmethod make-current ((context context))
  (q+:make-current context))

(defmethod done-current ((context context))
  (q+:done-current context))

(defmethod hide ((context context))
  (q+:hide context))

(defmethod show ((context context) &key (fullscreen NIL fullscreen-p))
  (q+:show context)
  (when fullscreen-p
    (if fullscreen
        (q+:show-full-screen context)
        (q+:show-normal context))))

(defmethod quit ((context context))
  (q+:close context))

(defmethod title ((context context))
  (q+:window-title context))

(defmethod (setf title) (value (context context))
  (setf (q+:window-title context) value))

(defmethod resize ((context context) width height)
  (q+:resize context (round width) (round height)))

(defmethod swap-buffers ((context context))
  (q+:swap-buffers context))

(defmethod show-cursor ((context context))
  (setf (q+:cursor context) (q+:qt.arrow-cursor)))

(defmethod hide-cursor ((context context))
  (setf (q+:cursor context) (q+:qt.blank-cursor)))

(defmethod width ((context context))
  (q+:width context))

(defmethod height ((context context))
  (q+:height context))

(defmethod profile ((context context))
  (qtenumcase (q+:profile (glformat context))
    ((q+:qglformat.no-profile) NIL)
    ((q+:qglformat.core-profile) :core)
    ((q+:qglformat.compatibility-profile) :compatibility)))

(defmethod version ((context context))
  (list (q+:major-version (glformat context))
        (q+:minor-version (glformat context))))

(define-subwidget (context resize-timer) (q+:make-qtimer context)
  (setf (q+:single-shot resize-timer) T))

(define-slot (context resize-timer) ()
  (declare (connected resize-timer (timeout)))
  (handle (make-instance 'resize :width (q+:width context) :height (q+:height context))
          (handler context)))

;;; REASON FOR THE OVERRIDES:
;; The rendering in this engine works as follows.
;; There is a main thread that controls the Qt windows and a separate thread that handles
;; the display updating and GL rendering. Now, OpenGL has a context, that can only ever be
;; used from one thread at once. If we want to draw from another thread, we first need to
;; make the context current to that thread. As such, in order to start drawing in our
;; rendering thread, we need to make the context current there. Unfortunately for us, the
;; QGLWidget offers some convenience methods called initializeGL, resizeGL, and paintGL,
;; which are always called from the main thread, and /automatically/ acquire the context.
;; As such, if one of these methods is called by Qt, it fucks up our rendering thread as
;; it steals the GL context out from under its feet. Since we don't need these methods
;; and they're actually actively harmful, we need to prevent Qt from ever calling them.
;;
;; That's why the following two overrides exist. The resize-event merely issues a new
;; event to the scene, which will then trigger the actual resizing in the controller's
;; handler. The paint-event override does absolutely nothing, which is fine because we
;; do all the drawing and buffer swapping in the rendering thread anyway, and doing this
;; prevents the calling of paintGL.

(define-override (context resize-event) (ev)
  (q+:start resize-timer 100))

(define-override (context paint-event) (ev))

(defun launch-with-context (&optional (main 'main) &rest initargs)
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (let ((context))
    (with-main-window (main (apply #'make-instance main initargs)
                       :on-error #'standalone-error-handler
                       :show NIL
                       :finalize NIL)
      (setf context (trial:context main))
      (show context)
      (start main))
    (finalize context)))
