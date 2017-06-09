#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.glop)

(defun make-context (&optional handler)
  (make-instance 'context :handler handler))

(defclass context (trial:context glop:window)
  ((context :initform NIL :accessor context)
   (initargs :initform NIL :accessor initargs)
   (profile :initarg :profile :accessor profile)
   (version :initarg :version :accessor version)
   (mouse-pos :initform (vec 0 0) :accessor mouse-pos)
   (resize-time :initform 0 :accessor resize-time)))

(defmethod initialize-instance ((context context) &key)
  (call-next-method)
  (create-context context))

(defmethod shared-initialize :after ((context context) slots
                                     &key (width NIL width-p)
                                          (height NIL height-p)
                                          (title NIL title-p)
                                          (double-buffering NIL double-buffering-p)
                                          (accumulation-buffer NIL accumulation-buffer-p)
                                          (alpha-buffer NIL alpha-buffer-p)
                                          (depth-buffer NIL depth-buffer-p)
                                          (stencil-buffer NIL stencil-buffer-p)
                                          (stereo-buffer NIL stereo-buffer-p))
  (macrolet ((maybe-set (var)
               `(when ,(intern (format NIL "~a-~a" var 'p))
                  (setf (getf (initargs context) ,(intern (string var) :keyword))
                        ,var))))
    (maybe-set width)
    (maybe-set height)
    (maybe-set title)
    (maybe-set double-buffering)
    (maybe-set accumulation-buffer)
    (maybe-set alpha-buffer)
    (maybe-set depth-buffer)
    (maybe-set stencil-buffer)
    (maybe-set stereo-buffer)))

(defmethod create-context ((context context))
  (flet ((g (item &optional default)
           (getf (initargs context) item default)))
    (glop:open-window context
                      (g :title) (g :width) (g :height)
                      :x (g :x 0) :y (g :y 0)
                      :double-buffer (g :double-buffering)
                      :stereo (g :stereo-buffer)
                      :accum-buffer (g :accumulation-buffer)
                      :stencil-buffer (g :stencil-buffer))
    (with-cleanup-on-failure (destroy-context context)
      (setf (context context)
            (glop:create-gl-context context
                                    :major (first (version context))
                                    :minor (second (version context))
                                    :profile (profile context)
                                    :make-current T)))))

(defmethod destroy-context ((context context))
  (glop:destroy-window context)
  (setf context NIL))

(defmethod valid-p ((context context))
  (not (null (context context))))

(defmethod make-current ((context context))
  (glop:attach-gl-context context (context context)))

(defmethod done-current ((context context))
  (glop:detach-gl-context (context context)))

(defmethod hide ((context context))
  (glop:hide-window context))

(defmethod show ((context context) &key (fullscreen NIL fullscreen-p))
  (glop:show-window context)
  (when fullscreen-p
    (glop:set-fullscreen window fullscreen)))

(defmethod resize ((context context) width height)
  (setf (glop:window-width context) width)
  (setf (glop:window-height context) height))

(defmethod swap-buffers ((context context))
  (glop:swap-buffers context))

(defmethod show-cursor ((context context))
  (glop:show-cursor context))

(defmethod hide-cursor ((context context))
  (glop:hide-cursor context))

(defmethod title ((context context))
  (glop::window-title context))

(defmethod (setf title) (value (context context))
  (glop:set-window-title context value))

(defmethod width ((context context))
  (glop:window-width context))

(defmethod height ((context context))
  (glop:window-height context))

(defun launch-with-context (&optional (main 'main) &rest initargs)
  #+linux (cffi:foreign-funcall "XInitThreads" :int)
  (let* ((main (apply #'make-instance main initargs))
         (context (trial:context main)))
    (unwind-protect
         (catch 'escape
           (flet ((body ()
                    (start main)
                    (loop (glop:dispatch-events context :blocking T :on-foo NIL))))
             #+darwin
             (tmt:with-body-in-main-thread (:blocking T)
               (body))
             #-darwin
             (body)))
      (trial::finalize main))))

(defmethod glop:on-event ((context context) event)
  (typecase event
    (glop:key-press-event
     (handle (make-instance 'key-press :key (glop:keysym event)
                                       :text (glop:text event))
             (handler context)))
    (glop:key-release-event
     (handle (make-instance 'key-release :key (glop:keysym event)
                                         :text (glop:text event))
             (handler context)))
    (glop:button-press-event
     (case (glop:button event)
       (4 (handle (make-instance 'mouse-scroll :delta 1
                                               :pos (mouse-pos context))
                  (handler context)))
       (5 (handle (make-instance 'mouse-scroll :delta -1
                                               :pos (mouse-pos context))
                  (handler context)))
       (T (handle (make-instance 'mouse-press :button (glop-button->symbol
                                                       (glop:button event))
                                              :pos (mouse-pos context))
                (handler context)))))
    (glop:button-release-event
     (handle (make-instance 'mouse-release :button (glop:button event)
                                           :pos (mouse-pos context))
             (handler context)))
    (glop:mouse-motion-event
     (let ((current (vec (+ (glop:x event) (glop:dx event))
                         (+ (glop:y event) (glop:dy event)))))
       (setf (mouse-pos context) current)
       (handle (make-instance 'mouse-move :old-pos (vec (glop:x event) (glop:y event))
                                          :pos (vcopy current))
               (handler context))))
    (glop:resize-event
     (when (< (resize-time context) (get-universal-time))
       (setf (resize-time context) (get-universal-time))
       (handle (make-instance 'resize :width (glop:width event)
                                      :height (glop:height event))
               (handler context))))
    (glop:expose-event)
    (glop:visibility-event)
    (glop:focus-event)
    (glop:close-event
     (throw 'escape NIL))))

(defun glop-button->symbol (button)
  (case button
    (1 :left)
    (2 :middle)
    (3 :right)
    (T :x1)))
