(ql:quickload '(drakma cl-ppcre))

(defun split (string by)
  (let ((out (make-string-output-stream))
        (parts ()))
    (flet ((add ()
             (let ((part (get-output-stream-string out)))
               (when (string/= "" part)
                 (push part parts)))))
      (loop for char across string
            do (if (char= char by)
                   (add)
                   (write-char char out))
            finally (add)))
    (nreverse parts)))

(defun convert-key-name (key)
  (let ((key (intern (string-upcase key) :keyword)))
    (case key
      (:righty :right-v)
      (:rightx :right-h)
      (:lefty :left-v)
      (:leftx :left-h)
      (:dpup :dpad-up)
      (:dpright :dpad-right)
      (:dpdown :dpad-down)
      (:dpleft :dpad-left)
      (:rightshoulder :r1)
      (:righttrigger :r2)
      (:leftshoulder :l1)
      (:lefttrigger :l2)
      (:x :x)
      (:y :y)
      (:a :a)
      (:b :b)
      (:leftstick :left)
      (:rightstick :right)
      (:back :select)
      (:guide :home)
      (:start :start)
      (T key))))

(defun u16byte-swap (x)
  (let ((y 0))
    (setf (ldb (byte 8 0) y) (ldb (byte 8 8) x))
    (setf (ldb (byte 8 8) y) (ldb (byte 8 0) x))
    y))

(defun parse-guid (guid platform)
  (case platform
    ((:linux :darwin)
     (list (u16byte-swap (parse-integer guid :start 8 :end 12 :radix 16))
           (u16byte-swap (parse-integer guid :start 16 :end 20 :radix 16))))
    (:windows
     (list (u16byte-swap (parse-integer guid :start 0 :end 4 :radix 16))
           (u16byte-swap (parse-integer guid :start 4 :end 8 :radix 16))))
    (T guid)))

(defun parse-format (format)
  (case (char format 0)
    (#\b (list :button (parse-integer format :start 1)))
    (#\h (list :hat (parse-integer format :start (1+ (position #\. format)))))
    (#\a (list :axis (parse-integer format :start 1)))
    (T format)))

(defun parse-platform (platform)
  (cond ((string-equal platform "mac os x")
         :darwin)
        (T
         (intern (cl-ppcre:regex-replace-all " " (string-upcase platform) "-") :keyword))))

(defun convert-name (name)
  (flet ((r (thing s r)
           (cl-ppcre:regex-replace-all s thing r)))
    (string-downcase (r (r name " +" "-") "[()]." ""))))

(defun parse-line (line)
  (let ((parts (etypecase line
                 (list line)
                 (string (split line #\,))))
        (plist ()))
    (destructuring-bind (guid name &rest props) parts
      (loop for prop in props
            for (key format) = (split prop #\:)
            when format
            do (let* ((flist (parse-format format))
                      (key (convert-key-name key)))
                 (cond ((listp flist)
                        (push (list (second flist) key) (getf plist (first flist))))
                       ((eql key :platform)
                        (setf (getf plist key) (parse-platform flist)))
                       (T
                        (push (list key flist) (getf plist :properties))))))
      (list* :id (parse-guid guid (getf plist :platform))
             :name (convert-name name)
             plist))))

(defun load-db (&optional (url "https://raw.githubusercontent.com/gabomdq/SDL_GameControllerDB/master/gamecontrollerdb.txt"))
  (let ((stream (drakma:http-request url :want-stream T))
        (lines ()))
    (unwind-protect
         (loop for line = (read-line stream NIL)
               while line
               do (when (and (string/= "" line)
                             (char/= #\# (char line 0)))
                    (let ((line (parse-line line)))
                      (loop with i = 1
                            for other in lines
                            do (when (and (equal (getf line :name) (getf other :name))
                                          (equal (getf line :platform) (getf other :platform)))
                                 (setf (getf line :name) (convert-name (format NIL "~a-~a" (getf line :name) (incf i))))))
                      ;; Ensure no duplicate IDs on the same platform.
                      (pushnew line lines
                               :key (lambda (line) (list (getf line :id)
                                                         (getf line :platform)))
                               :test #'equal))))
      (close stream))
    (nreverse lines)))

(defun convert-map (map)
  (format NIL "~
#+~(~a~)
\(define-gamepad ~a ~a
  (:axes~:{
    (~2d ~(~s~))~})
  (:buttons~:{
    (~2d ~(~s~))~}))"
          ;; FIXME: What to do with hats? Seem to be used for DPAD sometimes.
          ;;        libstem_gamepad turns them into axis. ID conversion is
          ;;        not clear however.
          (getf map :platform)
          (getf map :name)
          (getf map :id)
          (sort (getf map :axis) #'< :key #'first)
          (sort (getf map :button) #'< :key #'first)))

(defun convert-db (&optional (db (load-db)) (file (asdf:system-relative-pathname :trial "gamepad-db.lisp")))
  (with-open-file (out file :direction :output
                            :if-exists :supersede)
    (format out "(in-package #:org.shirakumo.fraf.trial)~%~%")
    (dolist (map db)
      (format out "~a~%~%" (convert-map map)))))
