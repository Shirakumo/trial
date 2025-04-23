(in-package #:org.shirakumo.fraf.trial)

(define-action-set system-action)

(define-action reload-scene (system-action))
(define-action quit-game (system-action))
(define-action toggle-overlay (system-action))

(defclass controller (entity listener)
  ((handlers :initform NIL :accessor handlers)
   (name :initform :controller)))

(defmethod handle ((ev quit-game) (controller controller))
  (quit *context*))

(defmethod handle ((ev event) (controller controller))
  (dolist (handler (handlers controller))
    (handle ev handler))
  (map-event ev (scene +main+)))

(defmethod handle ((ev lose-focus) (controller controller))
  (clear-retained))

(defmethod handle ((ev reload-scene) (controller controller))
  (let ((old (scene +main+)))
    (change-scene +main+ (make-instance (type-of old)))))

(defmethod handle ((ev asset-changed) (controller controller))
  (when (loaded-p (changed-asset ev))
    (reload (changed-asset ev))))

(defclass load-request (event)
  ((thing :initarg :thing)))

(define-handler (controller load-request) (thing)
  (typecase thing
    (asset
     (if (loaded-p thing)
         (reload thing)
         (load thing)))
    (resource
     (unless (allocated-p thing)
       (allocate thing)))
    (T
     (commit thing (loader +main+) :unload NIL))))

(defun maybe-reload-scene (&optional (main +main+))
  (when main
    (issue (scene main) 'reload-scene)))

(defclass eval-request (event)
  ((func :initarg :func)
   (return-values :accessor return-values)))

(define-handler (controller eval-request) (func)
  (let ((vals (multiple-value-list (funcall func))))
    (setf (return-values eval-request) vals)))

(defun call-in-render-loop (function scene &key block)
  (let ((event (issue scene 'eval-request :func function)))
    (when block
      (loop until (slot-boundp event 'return-values)
            do (sleep 0.01))
      (values-list (return-values event)))))

(defmacro with-eval-in-render-loop ((&optional (scene '(scene +main+)) &rest args) &body body)
  `(call-in-render-loop (lambda () ,@body) ,scene ,@args))

(define-shader-entity display-controller (controller debug-text)
  ((fps-buffer :initform (make-array 100 :initial-element 1f0 :element-type 'single-float) :reader fps-buffer)
   (fps-buffer-idx :initform 0 :accessor fps-buffer-idx)
   (observers :initform (make-array 0 :adjustable T :fill-pointer T) :accessor observers)
   (background :initform (vec4 1 1 1 0.75))
   (show-overlay :initform T :accessor show-overlay)
   (%string :initform (make-array 4096 :element-type 'character :fill-pointer 0) :accessor %string)))

(defmethod handle ((ev toggle-overlay) (controller display-controller))
  (setf (show-overlay controller) (not (show-overlay controller))))

(defmethod handle ((ev resize) (controller display-controller))
  (setf (font-size controller) (if (< 1920 (width ev)) 32 17)))

(defun compute-fps-buffer-fps (fps-buffer)
  (declare (optimize speed (safety 0)))
  (declare (type (simple-array single-float (100)) fps-buffer))
  (let ((sum 0f0))
    (declare (type single-float sum))
    (loop for i of-type (unsigned-byte 16) from 0 below (length fps-buffer)
          do (incf sum (aref fps-buffer i)))
    (values (/ sum (length fps-buffer))
            (/ (* 1000 (length fps-buffer)) sum))))

(defmethod observe ((func function) &key title)
  (let ((node (node :controller T)))
    (when node
      (unless (typep node 'display-controller)
        (change-class node 'display-controller)
        (with-eval-in-render-loop ()
          (let ((area (make-instance 'staging-area)))
            (stage node area)
            (commit area (loader +main+) :unload NIL))))
      (let ((observers (observers node)))
        (when observers
          (let* ((title (or title (format NIL "~d" (length observers))))
                 (position (position title observers :key #'car :test #'equal)))
            (if position
                (setf (aref observers position) (cons title func))
                (vector-push-extend (cons title func) observers))
            func))))))

(defmethod observe (thing &rest args &key &allow-other-keys)
  (let ((func (compile NIL `(lambda (ev)
                              ,thing))))
    (apply #'observe func args)))

(defmacro observe! (form &rest args)
  `(observe (lambda () ,form) ,@args))

(defmethod stop-observing (&optional title)
  (let ((observers (ignore-errors (observers (node :controller T)))))
    (when observers
      (if title
          (let ((pos (position title observers :key #'car :test #'equal)))
            (when pos (array-utils:vector-pop-position observers pos)))
          (loop for i from 0 below (array-total-size observers)
                do (setf (aref observers i) NIL)
                finally (setf (fill-pointer observers) 0))))))

(defparameter *controller-pprint*
  (let ((table (copy-pprint-dispatch)))
    (set-pprint-dispatch 'float (lambda (s o) (format s "~,3@f" o))
                         10 table)
    table))

(defun compose-controller-debug-text (controller)
  (multiple-value-bind (gfree gtotal) (org.shirakumo.machine-state:gpu-room)
    (multiple-value-bind (cfree ctotal) (org.shirakumo.machine-state:gc-room)
      (setf (fill-pointer (%string controller)) 0)
      (with-discarded-allocations
        (with-output-to-string (stream (%string controller))
          (multiple-value-bind (fps dur) (compute-fps-buffer-fps (fps-buffer controller))
            (format stream "FPS  [Hz]: ~8,2f (~6,2fms)~%~
                          RAM  [KB]: ~8d (~2d%)~%~
                          VRAM [KB]: ~8d (~2d%)~%~
                          RESOURCES: ~8d"
                    fps dur
                    (truncate (- ctotal cfree) 1024) (if (< 0 ctotal) (floor (/ (- ctotal cfree) ctotal 0.01)) 0)
                    (truncate (- gtotal gfree) 1024) (if (< 0 gtotal) (floor (/ (- gtotal gfree) gtotal 0.01)) 0)
                    (hash-table-count (loaded (loader +main+)))))
          (let ((*print-pprint-dispatch* *controller-pprint*))
            (loop with observers = (observers controller)
                  for i from 0 below (length observers)
                  for (title . func) = (aref observers i)
                  when func
                  do (restart-case (format stream "~%~a:~12t~{~a~^, ~}" title (multiple-value-list (funcall func)))
                       (remove-observer ()
                         :report "Remove the offending observer."
                         (setf (aref observers i) NIL)))))))
      (%string controller))))

(defmethod render :around ((controller display-controller) (program shader-program))
  (when (show-overlay controller)
    (setf (text controller) (compose-controller-debug-text controller))
    (setf (vy (location controller)) (- (vy (size controller)) (font-size controller)))
    (setf (vx (location controller)) 5)
    (with-pushed-matrix ((view-matrix :identity)
                         (projection-matrix :identity))
      (orthographic-projection 0 (width *context*)
                               0 (height *context*)
                               0 10)
      (translate-by 2 (- (height *context*) (font-size controller) 2) 0)
      (let ((fps-buffer (fps-buffer controller))
            (idx (fps-buffer-idx controller)))
        (when (= (length fps-buffer) idx)
          (setf idx 0))
        (setf (aref fps-buffer idx)  (if (= 0 (frame-time +main+)) 1f0 (/ (float (frame-time +main+) 0f0))))
        (setf (fps-buffer-idx controller) (1+ idx)))
      (with-depth-mask T
        (call-next-method)))))
