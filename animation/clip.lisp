(in-package #:org.shirakumo.fraf.trial)

(defclass clip (sequences:sequence standard-object)
  ((name :initarg :name :initform NIL :accessor name)
   (tracks :initform #() :accessor tracks)
   (start-time :initform 0f0 :accessor start-time)
   (end-time :initform 0f0 :accessor end-time)
   (next-clip :accessor next-clip)
   (blocking-p :initform NIL :accessor blocking-p)
   (blend-duration :initarg :blend-duration :initform 0.2f0 :accessor blend-duration)))

(defmethod shared-initialize :after ((clip clip) slots &key tracks (loop-p NIL loop-pp) (next-clip NIL next-clip-p))
  (when tracks
    (setf (tracks clip) tracks))
  (cond (next-clip-p
         (setf (next-clip clip) next-clip))
        (loop-pp
         (setf (loop-p clip) loop-p))
        ((not (slot-boundp clip 'next-clip))
         (setf (loop-p clip) T))))

(defmethod loop-p ((clip clip))
  (eq clip (next-clip clip)))

(defmethod (setf loop-p) (value (clip clip))
  (if value
      (setf (next-clip clip) clip)
      (setf (next-clip clip) NIL)))

(defmethod valid-p ((clip clip))
  (< (start-time clip) (end-time clip)))

(defmethod print-object ((clip clip) stream)
  (print-unreadable-object (clip stream :type T)
    (if (valid-p clip)
        (format stream "~s ~a ~a" (name clip) (start-time clip) (end-time clip))
        (format stream "~s INVALID" (name clip)))))

(defmethod describe-object ((clip clip) stream)
  (call-next-method)
  (format stream "~&  DURATION~32t = ~a" (duration clip))
  (format stream "~&~%Tracks:")
  (flet ((print-track (track)
           (format stream "~3d frames ~a~%"
                   (length (frames track)) (interpolation track))))
    (loop for i from 0
          for track across (tracks clip)
          do (format stream "~&  ~3d: ~30a " i (name track))
             (etypecase track
               (animation-track
                (print-track track))
               (transform-track
                (format stream "~%")
                (format stream "      LOCATION: ") (print-track (location track))
                (format stream "      ROTATION: ") (print-track (rotation track))
                (format stream "      SCALING:  ") (print-track (scaling track)))))))

(defun fit-to-clip (clip time)
  (let ((start (start-time clip))
        (end (end-time clip)))
    (if (loop-p clip)
        (+ start (mod (- time start) (- end start)))
        (clamp start time end))))

(defun recompute-duration (clip)
  (declare (optimize speed))
  (loop for track across (the simple-vector (tracks clip))
        when (valid-p track) minimize (start-time track) into start of-type single-float
        when (valid-p track) maximize (end-time track) into end of-type single-float
        finally (setf (start-time clip) start
                      (end-time clip) end))
  clip)

(defmethod duration ((clip clip))
  (- (end-time clip) (start-time clip)))

(defmethod (setf tracks) :after (tracks (clip clip))
  (recompute-duration clip))

(defmethod sequences:length ((clip clip))
  (length (tracks clip)))

(defmethod sequences:adjust-sequence ((clip clip) length &rest args)
  (declare (ignore args))
  (let* ((tracks (tracks clip))
         (old (length tracks)))
    (setf tracks (adjust-array tracks length))
    (loop for i from old below length
          do (setf (svref tracks i) (make-instance 'transform-track)))
    (setf (tracks clip) tracks))
  clip)

(defmethod sequences:elt ((clip clip) index)
  (svref (tracks clip) index))

(defmethod (setf sequences:elt) (track (clip clip) index)
  (setf (svref (tracks clip) index) track)
  (recompute-duration clip)
  track)

(defmethod find-animation-track ((clip clip) name &key (if-does-not-exist :error))
  (loop for track across (tracks clip)
        do (when (eql name (name track))
             (return track))
        finally (ecase if-does-not-exist
                  (:error (error "No track with name ~s found." name))
                  (:create (progn 
                             (sequences:adjust-sequence clip (1+ (length clip)))
                             (let ((track (elt clip (1- (length clip)))))
                               (setf (name track) name)
                               (return track))))
                  ((NIL) (return NIL)))))

(defmacro %define-sampler-method (class accessor)
  `(defmethod sample ((thing ,class) (clip clip) time &key)
     (declare (type single-float time))
     (declare (optimize speed))
     (if (< 0.0 (the single-float (end-time clip)))
         (let ((time (fit-to-clip clip time))
               (tracks (tracks clip))
               (loop-p (loop-p clip)))
           (declare (type single-float time))
           (declare (type simple-array tracks))
           (loop for i from 0 below (length tracks)
                 for track = (svref tracks i)
                 for name = (name track)
                 do (setf ,accessor (sample ,accessor track time :loop-p loop-p)))
           time)
         0.0)))

(%define-sampler-method sequences:sequence (elt thing name))
(%define-sampler-method vector (aref thing name))
(%define-sampler-method hash-table (gethash name thing))
(%define-sampler-method standard-object (slot-value thing name))
(%define-sampler-method structure-object (slot-value thing name))

(defmethod reorder ((clip clip) map)
  (dotimes (i (length clip) clip)
    (let ((track (elt clip i)))
      (setf (name track) (gethash (name track) map (name track)))))
  (setf (tracks clip) (sort (tracks clip) (lambda (a b)
                                            (if (and (realp a) (realp b))
                                                (< a b)
                                                T))
                            :key #'name)))

(defvar *clips* (make-hash-table :test 'equal))

(defmethod clip ((name symbol))
  (gethash name *clips*))

(defmethod (setf clip) (clip (name symbol))
  (setf (gethash name *clips*) clip))

(defmacro define-clip (name tracks &body initargs-and-frames)
  (destructuring-bind (name &optional (class 'clip)) (enlist name)
    (form-fiddle:with-body-options (frames options) initargs-and-frames
      `(setf (clip ',name) (ensure-instance (clip ',name) ',class
                                            :tracks (compile-tracks ',(if (numberp tracks)
                                                                          (loop for i from 0 below tracks
                                                                                collect (list i :class 'animation-track))
                                                                          (loop for el in tracks
                                                                                collect (if (listp el) el (list el))))
                                                                    ,@(loop for part in frames
                                                                            collect (cond ((and (symbolp part) (string= "_" part))
                                                                                           NIL)
                                                                                          ((and (consp part) (not (symbolp (car part))))
                                                                                           `(list ,@part))
                                                                                          (T
                                                                                           part))))
                                            ,@options)))))

(defun compile-tracks (track-descriptions &rest frame-data)
  (let ((tracks (make-array (length track-descriptions))))
    (loop for i from 0
          for (name . initargs) in track-descriptions
          for track-frames = (loop for frame = frame-data then (nthcdr (1+ (length tracks)) frame)
                                   for value = (nth (1+ i) frame)
                                   while frame
                                   when value
                                   collect (cons (car frame) value))
          for track = (apply #'make-instance
                             (getf initargs :class 'animation-track)
                             :name name
                             :times (mapcar #'car track-frames)
                             :values (let ((values (mapcar #'cdr track-frames)))
                                       ;; Duplicate last frame values to ensure we leave enough data for the track to finish.
                                       (setf (cdr (last values)) (cons (car (last values)) (last values)))
                                       (alexandria:flatten values))
                             (progn (remf initargs :class) initargs))
          do (setf (aref tracks i) track))
    tracks))

#++
(define-clip foo (:linear :hermite)
  0.0 (vec 3 2 1) (1.0 2.0 0.0)
  1.0 _           (2.0 3.0 0.0)
  2.0 (vec 0 2 1) _)

(defclass forward-kinematic-clip (clip)
  ((velocity :initform (vec3) :accessor velocity)
   (rotation :initform (quat) :accessor rotation)
   (forward-track :initform NIL :accessor forward-track)
   (blocking-p :initform T)
   (velocity-scale :initarg :velocity-scale :initform 1.0 :accessor velocity-scale)))

(defmethod update-instance-for-different-class :after ((prev clip) (clip forward-kinematic-clip) &key (track 0))
  (setf (forward-track clip) track)
  (setf (velocity clip) (vec3))
  (setf (rotation clip) (quat)))

(defmethod update-instance-for-different-class :after ((prev forward-kinematic-clip) (clip clip) &key)
  (loop for track across (tracks clip)
        do (when (typep track 'dummy-track)
             (change-class track 'transform-track))))

(defmethod describe-object ((clip forward-kinematic-clip) stream)
  (call-next-method)
  (format stream "~
Forward clip:
  ~a
  ~a
  ~a~&"
          (location (forward-track clip))
          (rotation (forward-track clip))
          (scaling (forward-track clip))))

(defmethod (setf forward-track) ((track integer) (clip forward-kinematic-clip))
  (loop for track across (tracks clip)
        do (when (typep track 'dummy-track)
             (change-class track 'transform-track)))
  (setf (forward-track clip) (differentiate (aref (tracks clip) track)))
  ;; Stub the track out so it won't actually impart the change, since we now
  ;; have the change in forward kinematics instead.
  (change-class (aref (tracks clip) track) 'dummy-track))

(defmethod (setf forward-track) ((track symbol) (clip forward-kinematic-clip))
  (setf (forward-track clip) (or (position track (tracks clip) :key #'name)
                                 (error "No track with name ~s" track))))

(defmethod (setf forward-track) ((track string) (clip forward-kinematic-clip))
  (setf (forward-track clip) (or (position track (tracks clip) :key #'name :test #'equal)
                                 (error "No track with name ~s" track))))

(defmethod sample :after (target (clip forward-kinematic-clip) time &key loop-p)
  (let ((track (forward-track clip)))
    (when (< 1 (length (location track)))
      (sample (velocity clip) (location track) time :loop-p loop-p)
      (nv* (velocity clip) (velocity-scale clip)))
    (when (< 1 (length (rotation track)))
      (sample (rotation clip) (rotation track) time :loop-p loop-p))))
