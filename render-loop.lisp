(in-package #:org.shirakumo.fraf.trial)

(defclass render-loop ()
  ((thread :initform NIL :accessor thread)
   (delta-time :initarg :delta-time :initform 0.01f0 :accessor delta-time)
   (frame-time :initform 0.0d0 :accessor frame-time)
   (target-frame-time :initarg :target-frame-time :initform 0.0d0 :accessor target-frame-time)))

(defmethod initialize-instance :after ((loop render-loop) &key (target-framerate (setting :display :target-framerate)))
  (setf (target-frame-time loop) (float (typecase target-framerate
                                          (real (/ target-framerate))
                                          (T 0.0))
                                        0d0)))

(defmethod start ((render-loop render-loop))
  #+nx (setf (org.shirakumo.machine-state:thread-core-mask T) #b0001)
  (setf (thread render-loop) T)
  (setf (thread render-loop)
        (with-thread ("render-loop")
          #+nx (setf (org.shirakumo.machine-state:thread-core-mask T) #b0010)
          (render-loop render-loop))))

(defmethod stop ((render-loop render-loop))
  (let ((thread (thread render-loop)))
    (with-thread-exit (thread)
      (setf (thread render-loop) NIL))))

(defmethod finalize :before ((render-loop render-loop))
  (stop render-loop))

(defmethod render (thing (render-loop render-loop)))
(defmethod update ((render-loop render-loop) tt dt fc))

(defmethod render-loop ((render-loop render-loop))
  (declare (optimize speed))
  (let ((fc 0))
    (declare (type fixnum fc))
    (with-simple-restart (exit-render-loop "Exit the render loop entirely.")
      (with-unwind-protection (v:info :trial.render-loop "Exiting render-loop for ~a." render-loop)
        (v:debug :trial.render-loop "Entering render-loop")
        (with-retry-restart (reset-render-loop "Reset the render loop timing, not catching up with lost frames.")
          (let ((tt 0.0d0)
                (fdt (coerce (delta-time render-loop) 'single-float))
                (dt (coerce (delta-time render-loop) 'double-float))
                (target-frame-time (coerce (target-frame-time render-loop) 'double-float))
                (current-time (current-time))
                ;; Accumulator starts big such that we ensure an UPDATE happens
                ;; before the first RENDER can happen. Otherwise we get an ugly
                ;; first frame display.
                (accumulator 1000.0d0)
                (new-time 0.0d0)
                (frame-time 0.0d0)
                (extra-time 0.0d0))
            (declare (type double-float tt dt current-time
                           accumulator new-time frame-time))
            (with-error-logging (:trial.render-loop "Error in render thread")
              (loop while (thread render-loop)
                    do (loop (setf new-time (current-time))
                             (setf frame-time (- new-time current-time))
                             (setf extra-time (- target-frame-time frame-time))
                             (when (<= extra-time 0.0)
                               (return))
                             (sleep extra-time))
                       (setf current-time new-time)
                       (incf accumulator frame-time)
                       (loop while (<= dt accumulator)
                             do (when (<= 10d0 accumulator)
                                  (setf accumulator dt))
                                (update render-loop tt fdt fc)
                                (decf accumulator dt)
                                (incf tt dt)
                                (incf fc))
                       ;; FIXME: interpolate state
                       ;;        See http://gafferongames.com/game-physics/fix-your-timestep/
                       (setf (frame-time render-loop) frame-time)
                       (with-simple-restart (abort "Abort the update and retry.")
                         (render render-loop render-loop))))))))))

(defun reset-render-loop ()
  (when (find-restart 'reset-render-loop)
    (invoke-restart 'reset-render-loop)))

(defun exit-render-loop ()
  (when (find-restart 'exit-render-loop)
    (invoke-restart 'exit-render-loop)))

(defclass single-threaded-render-loop (render-loop)
  ())

(defmethod start ((render-loop single-threaded-render-loop))
  (setf (thread render-loop) (bt:current-thread))
  (render-loop render-loop))

(defmethod stop ((render-loop single-threaded-render-loop))
  (setf (thread render-loop) NIL)
  (when (find-restart 'exit-render-loop)
    (invoke-restart 'exit-render-loop)))

(defmethod update :after ((render-loop single-threaded-render-loop) tt dt fc)
  (poll-input render-loop))

;; NOTE: this will not be useful for 99.99% of cases as-is, since the context is not shared
;;       between the two threads and the render-loop will try to render stuff while the update-loop
;;       will try to fuck with the context.
(defclass decoupled-render-loop (render-loop)
  ((fc :initform 0 :accessor fc)
   (update-thread :initform NIL :accessor update-thread)))

(defmethod start ((render-loop decoupled-render-loop))
  (call-next-method)
  (setf (update-thread render-loop) T)
  (setf (update-thread render-loop)
        (with-thread ("update-loop")
          #+nx (setf (org.shirakumo.machine-state:thread-core-mask T) #b0100)
          (update-loop render-loop))))

(defmethod stop ((render-loop decoupled-render-loop))
  (call-next-method)
  (let ((thread (update-thread render-loop)))
    (with-thread-exit (thread)
      (setf (update-thread render-loop) NIL))))

(defmethod render-loop ((render-loop decoupled-render-loop))
  (declare (optimize speed))
  (let ((fc 0))
    (declare (type fixnum fc))
    (with-simple-restart (exit-render-loop "Exit the render loop entirely.")
      (with-unwind-protection (v:info :trial.render-loop "Exiting render-loop for ~a." render-loop)
        (v:debug :trial.render-loop "Entering render-loop")
        (with-retry-restart (reset-render-loop "Reset the render loop timing, not catching up with lost frames.")
          (let ((target-frame-time (coerce (target-frame-time render-loop) 'double-float))
                (current-time (current-time))
                (new-time 0.0d0)
                (frame-time 0.0d0)
                (extra-time 0.0d0))
            (declare (type double-float current-time
                            new-time frame-time))
            (with-error-logging (:trial.render-loop "Error in render thread")
              (loop while (thread render-loop)
                    do (loop (setf new-time (current-time))
                             (setf frame-time (- new-time current-time))
                             (setf extra-time (- target-frame-time frame-time))
                             (when (<= extra-time 0.0)
                               (return))
                             (sleep extra-time))
                       (setf current-time new-time)
                       (setf (frame-time render-loop) frame-time)
                       (with-simple-restart (abort "Abort the update and retry.")
                         (render render-loop render-loop))
                       (setf (fc render-loop) (incf fc))))))))))

(defmethod update-loop ((render-loop decoupled-render-loop))
  (declare (optimize speed))
  (with-simple-restart (exit-render-loop "Exit the update loop entirely.")
    (with-unwind-protection (v:info :trial.render-loop "Exiting udpate-loop for ~a." render-loop)
      (v:debug :trial.render-loop "Entering update-loop")
      (with-retry-restart (reset-render-loop "Reset the update loop timing, not catching up with lost frames.")
        (let ((tt 0.0d0)
              (fdt (coerce (delta-time render-loop) 'single-float))
              (dt (coerce (delta-time render-loop) 'double-float))
              (current-time (current-time))
              ;; Accumulator starts big such that we ensure an UPDATE happens
              ;; before the first RENDER can happen. Otherwise we get an ugly
              ;; first frame display.
              (accumulator 1000.0d0)
              (new-time 0.0d0)
              (frame-time 0.0d0)
              (extra-time 0.0d0))
          (declare (type double-float tt dt current-time
                         accumulator new-time frame-time))
          (with-error-logging (:trial.render-loop "Error in render thread")
            (loop while (update-thread render-loop)
                  do (loop (setf new-time (current-time))
                           (setf frame-time (- new-time current-time))
                           (setf extra-time (- dt frame-time))
                           (when (<= extra-time 0.0)
                             (return))
                           (sleep extra-time))
                     (setf current-time new-time)
                     (incf accumulator frame-time)
                     (loop while (<= dt accumulator)
                           do (when (<= 10d0 accumulator)
                                (setf accumulator dt))
                              (update render-loop tt fdt (fc render-loop))
                              (decf accumulator dt)
                              (incf tt dt)))))))))
