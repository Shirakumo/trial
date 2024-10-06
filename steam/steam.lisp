(defpackage #:org.shirakumo.fraf.trial.steam
  (:use #:cl)
  (:export
   #:main
   #:steam-required-p
   #:use-steaminput
   #:generate-vdf)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:steam #:org.shirakumo.fraf.steamworks)
   (#:steam* #:org.shirakumo.fraf.steamworks.cffi)
   (#:v #:org.shirakumo.verbose)))

(pushnew :trial-steam *features*)

(in-package #:org.shirakumo.fraf.trial.steam)

(setf trial::*open-in-browser-hook*
      (lambda (url)
        (when (and (steam:steamworks-available-p)
                   (steam:overlay-enabled-p (steam:interface 'steam:steamutils T)))
          (steam:activate-overlay (steam:interface 'steam:steamfriends T) :url url)
          T)))

(defun action-label (action)
  (let ((action (etypecase action
                  (symbol action)
                  (class (class-name action))
                  (standard-object (class-name (class-of action))))))
    (format NIL "~a_~a"
            (cl-ppcre:regex-replace-all "[ -]" (package-name (symbol-package action)) "")
            (cffi:translate-camelcase-name action))))

(defmethod use-steaminput ((main trial:main)) NIL)

(defclass main (trial:main)
  ((analog-actions :initform #() :accessor analog-actions)
   (digital-actions :initform #() :accessor digital-actions)
   (use-steaminput :initform NIL :initarg :use-steaminput :accessor use-steaminput)
   (steam-required-p :initform NIL :initarg :require-steam :accessor steam-required-p)))

(defmethod initialize-instance ((main main) &key steam)
  (call-next-method)
  (destructuring-bind (&key app-id) steam
    (handler-bind ((error
                     (lambda (e)
                       (v:severe :trial.steam "Failed to initialise steamworks: ~a" e)
                       (v:debug :trial.steam e)
                       (when (deploy:deployed-p)
                         (cond ((steam-required-p main)
                                (invoke-restart 'steam:restart))
                               (T
                                (invoke-restart 'ignore)))))))
      (when (or (steam-required-p main)
                (deploy:deployed-p))
        (with-simple-restart (ignore "Ignore the steamworks failure.")
          (v:info :trial.steam "Initialising steamworks~@[ for app id ~a~]" app-id)
          (make-instance 'steam:steamworks-client :app-id app-id)
          ;; Populate action sets
          (when (use-steaminput main)
            (multiple-value-bind (analog digital) (compute-action-sets)
              (setf (analog-actions main) analog)
              (setf (digital-actions main) digital)))
          (steam:list-achievements (steam:interface 'steam:steamuserstats T))))
      (handler-case (steam:steamworks)
        (steam:steamworks-not-initialized ()
          (v:info :trial.steam "Steamworks not initialised, disabling steam input.")
          (setf (use-steaminput main) NIL))))))

#+windows
(defmethod initialize-instance :around ((main main) &rest initargs &key require-steam)
  ;; KLUDGE: Steam overlay on Windows seems to use terribly old GL commands, causing
  ;;         spurious INVALID OPERATION errors in our code. Thanks, Valve!
  (when (or require-steam (deploy:deployed-p))
    (push :compatibility initargs)
    (push :profile initargs))
  (apply #'call-next-method main initargs))

(defmethod (setf trial:active-p) :after (value (set trial:action-set))
  (when (and value trial:*context* (use-steaminput (trial:handler trial:*context*)))
    (let* ((label (action-label set))
           (action-set (steam:find-action-set (steam:interface 'steam:steaminput T) label)))
      (cond (action-set
             (v:info :trial.steam "Switching action set to ~s ~a" label action-set)
             (steam:activate action-set T))
            (T
             (v:warn :trial.steam "No action set for ~s found!" label))))))

(defmethod trial:finalize :after ((main main))
  (handler-case
      (steam:free (steam:steamworks))
    (steam:steamworks-not-initialized ())))

(defmethod trial:username ((main main))
  (if steam::*steamworks*
      (steam:display-name (steam:interface 'steam:steamfriends T))
      (call-next-method)))

(defun compute-action-sets ()
  (let ((input (steam:interface 'steam:steaminput T))
        (analog ())
        (digital ()))
    (dolist (class (trial:list-leaf-classes (find-class 'trial:action)))
      (cond ((or (eql class (find-class 'trial:analog-action))
                 (eql class (find-class 'trial:directional-action))))
            ((or (c2mop:subclassp class (find-class 'trial:analog-action))
                 (c2mop:subclassp class (find-class 'trial:directional-action)))
             (let ((action (steam:find-analog-action input (action-label class))))
               (when action (push (cons action class) analog))))
            (T
             (let ((action (steam:find-digital-action input (action-label class))))
               (when action (push (cons action class) digital))))))
    (values (coerce analog 'vector) (coerce digital 'vector))))

(defmethod trial:poll-input :after ((main main))
  (when steam::*steamworks*
    (steam:run-callbacks T))
  (when (use-steaminput main)
    (let ((input (steam:interface 'steam:steaminput T)))
      (steam:run-frame input)
      (macrolet ((fire (type &rest args)
                   `(progn (v:info :trial.steam "Firing from steaminput: ~a" ,type)
                           (trial:handle (make-instance ,type ,@args) main))))
        (steam:do-controllers (controller input)
          (loop for (action . class) across (analog-actions main)
                do (destructuring-bind (px py) (cddr (steam:previous-action-data action))
                     (destructuring-bind (active mode x y) (steam:action-data action controller)
                       (declare (ignore mode))
                       (when active
                         (if (c2mop:subclassp class (find-class 'trial:directional-action))
                             (when (or (/= x px) (/= y py)) (fire class :x x :y y))
                             (when (/= x px) (fire class :value x)))))))
          (loop for (action . class) across (digital-actions main)
                do (let ((previous (second (steam:previous-action-data action))))
                     (destructuring-bind (active state) (steam:action-data action controller)
                       (when (and active state (null previous))
                         (fire class))))))))))

(deploy:define-hook (:build check-steamworks) ()
  (unless steam::*low-level-present*
    (error "CL-STEAMWORKS has not been set up properly!
Please check the CL-STEAMWORKS setup instructions.

Refusing to deploy as the game would not launch properly anyway.")))


(defun action-labels (action)
  (let ((docstring (documentation action 'type))
        (labels ()))
    (or (cl-ppcre:register-groups-bind (groups) ("Labels:((\\n  *[^\\n]*)*)" docstring)
          (dolist (label (cl-ppcre:split "\\n  *" groups) labels)
            (cl-ppcre:register-groups-bind (language label) ("^(.*?): *(.*?) *$" label)
              (push (list language label) labels))))
        (list (list "english" (string-downcase (class-name (trial:ensure-class action))))))))

;; TODO: rewrite this
#++
(defun generate-vdf (file &key (actions T) (if-exists :error))
  (let ((localization ())
        (action-sets ())
        (actions (etypecase actions
                   (list
                    actions)
                   ((eql T)
                    (loop for class in (trial:list-leaf-classes (find-class 'trial:action))
                          unless (find class (list (find-class 'trial:analog-action)
                                                   (find-class 'trial:directional-action)
                                                   (find-class 'trial:spatial-action)))
                          collect (class-name class)))
                   (symbol
                    (loop for mapping in (second (trial:mapping actions))
                          collect (second mapping))))))
    (loop for action in actions
          for action-set = (trial:action-set action)
          do (loop for (language label) in (action-labels action)
                   do (setf (getf (getf localization language) (action-label action)) label))
             (let ((set (assoc (action-label action-set) action-sets :test #'string=)))
               (loop for (language label) in (action-labels action-set)
                     unless (find (action-label action-set) (getf localization language) :test #'string=)
                     do (setf (getf (getf localization language) (action-label action-set)) label))
               (unless set
                 (setf set (list (action-label (trial:action-set action))
                                 (format NIL "#~a" (action-label action-set))
                                 () () ()))
                 (push set action-sets))
               (cond ((c2mop:subclassp (trial:ensure-class action) (find-class 'trial:directional-action))
                      (push (list (action-label action) (format NIL "#~a" (action-label action)) "joystick_move")
                            (third set)))
                     ((c2mop:subclassp (trial:ensure-class action) (find-class 'trial:analog-action))
                      (push (list (action-label action) (format NIL "#~a" (action-label action)))
                            (fourth set)))
                     ((c2mop:subclassp (trial:ensure-class action) (find-class 'trial:action))
                      (push (list (action-label action) (format NIL "#~a" (action-label action)))
                            (fifth set)))
                     (T (error "~s is not an action class." action)))))
    (with-open-file (stream file :direction :output :if-exists if-exists)
      (format stream "~
\"In Game Actions\"{
  \"actions\"{~{
    ~{~s{
      \"title\" ~s
      \"StickPadGyro\"{~{
        ~{~s {\"title\" ~s \"input_mode\" ~s}~}~}
      }
      \"AnalogTrigger\"{~{
        ~{~s ~s~}~}
      }
      \"Button\"{~{
        ~{~s ~s~}~}
      }
    }~}~}
  }
  \"localization\"{~{
    ~s{~{
      ~s ~s~}
    }~}
  }
}"
              action-sets localization))))

(steam:define-callback steam*::gamepad-text-input-dismissed (result submitted)
  (when submitted
    (let ((text (steam:input-text (steam:interface 'steam:steamutils T))))
      (trial:handle (make-instance 'trial:text-entered :text text :replace T) (trial:handler trial:*context*)))))
