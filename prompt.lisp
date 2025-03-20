(in-package #:org.shirakumo.fraf.trial)

(defvar *prompt-string-table* (make-hash-table :test 'eq))

(defmacro define-glyph-table (name &body entries)
  (destructuring-bind (name &key inherit) (enlist name)
    `(setf (gethash ',name *prompt-string-table*)
           (let ((table (make-hash-table :test 'eq)))
             ,@(when inherit
                 `((loop for name being the hash-keys of (gethash ',inherit *prompt-string-table*) using (hash-value char)
                         do (setf (gethash name table) char))))
             ,@(loop for (name char) in entries
                     collect `(setf (gethash ',name table)
                                    ,(string (etypecase char
                                               (integer (code-char char))
                                               (string char)
                                               (character char)))))
             table))))

(defun degeneralise-axis-symbol (axis &optional (threshold 0.0))
  (case axis
    (:l :analog-l-any)
    (:r :analog-r-any)
    (:l-h (cond ((< 0 threshold) :analog-l-right)
                ((< threshold 0) :analog-l-left)
                (T :analog-l-left-right)))
    (:l-v (cond ((< 0 threshold) :analog-l-up)
                ((< threshold 0) :analog-l-down)
                (T :analog-l-up-down)))
    (:r-h (cond ((< 0 threshold) :analog-r-right)
                ((< threshold 0) :analog-r-left)
                (T :analog-r-left-right)))
    (:r-v (cond ((< 0 threshold) :analog-r-up)
                ((< threshold 0) :analog-r-down)
                (T :analog-r-up-down)))
    (:dpad-h (cond ((< 0 threshold) :dpad-right)
                   ((< threshold 0) :dpad-left)
                   (T :dpad-left-right)))
    (:dpad-v (cond ((< 0 threshold) :dpad-up)
                   ((< threshold 0) :dpad-down)
                   (T :dpad-up-down)))
    (T axis)))

(defun recombine-prompts (prompts)
  (flet ((try (new args)
           (when (subsetp args prompts)
             (setf prompts (list* new (set-difference prompts args))))))
    (try :wasd '(:w :a :s :d))
    (try :ijkl '(:i :j :k :l))
    (try :arrows '(:up :down :left :right))
    (try :analog-l-any '(:analog-l-left-right :analog-l-up-down))
    (try :analog-l-any '(:analog-l-left :analog-l-right :analog-l-up :analog-l-down))
    (try :analog-r-any '(:analog-r-left-right :analog-r-up-down))
    (try :analog-r-any '(:analog-r-left :analog-r-right :analog-r-up :analog-r-down))
    (try :dpad '(:dpad-left-right :dpad-up-down))
    (try :dpad '(:dpad-up :dpad-down :dpad-left :dpad-right))
    (try :any '(:left-right :up-down))
    prompts))

(defun specific-prompt-for-event-trigger (thing &optional (type 'input-event) (default NIL))
  (let ((mapping (first (find-action-mappings thing type))))
    (if mapping
        (let ((symbol (first (qualifier mapping))))
          (cond ((typep mapping 'axis-directional-mapping)
                 (degeneralise-axis-symbol symbol))
                ((eql 'gamepad-move (event-type mapping))
                 (degeneralise-axis-symbol symbol (threshold mapping)))
                (T
                 symbol)))
        default)))

(defun specific-prompts-for-event-trigger (thing &optional (type 'input-event))
  (let ((list ()))
    (loop for mapping in (find-action-mappings thing type)
          do (typecase mapping
               (axis-directional-mapping
                (dolist (symbol (qualifier mapping))
                  (push (degeneralise-axis-symbol symbol) list)))
               (mouse-directional-mapping
                (push :left-right list)
                (push :up-down list))
               (T 
                (dolist (symbol (qualifier mapping))
                  (if (eql 'gamepad-move (event-type mapping))
                      (push (degeneralise-axis-symbol symbol (threshold mapping)) list)
                      (push symbol list))))))
    list))

(defun normalize-prompt-bank (bank)
  (etypecase bank
    (null
     (normalize-prompt-bank +input-source+))
    (gamepad:device
     (normalize-prompt-bank (gamepad:icon-type bank)))
    (symbol
     (case bank
       (:generic-nintendo :nintendo)
       (:generic-xbox :xbox)
       (:generic-playstation :sony)
       (T (if (gethash bank *prompt-string-table*)
              bank
              :gamepad))))))

(defun prompt-string (thing &key bank default)
  (etypecase thing
    (null
     thing)
    (character
     (string thing))
    (string
     thing)
    (integer
     (princ-to-string thing))
    (keyword
     (let ((table (gethash (normalize-prompt-bank bank) *prompt-string-table*)))
       (if table
           (gethash thing table default)
           (prompt-string thing :bank :gamepad :default default))))
    (symbol
     (let* ((type (case bank
                    ((NIL) (etypecase +input-source+
                             ((eql :keyboard) 'key-event)
                             (gamepad:device 'gamepad-event)))
                    (:keyboard 'key-event)
                    (:mouse 'mouse-event)
                    (T 'gamepad-event)))
            (prompt (specific-prompt-for-event-trigger thing type NIL)))
       (cond (prompt
              (let ((char (or (when (eql bank :keyboard)
                                (ignore-errors (local-key-string *context* prompt)))
                              prompt)))
                (prompt-string char :bank bank)))
             (T
              default))))
    (action
     (prompt-string (type-of thing) :bank bank :default default))
    (input-event
     (let* ((prompt (etypecase thing
                      ((or gamepad-added gamepad-removed) :gamepad)
                      (gamepad-move (degeneralise-axis-symbol (axis thing) (pos thing)))
                      (gamepad-event (button thing))
                      (text-entered)
                      (keyboard-event (key thing))
                      (mouse-scroll (cond ((< (delta thing) 0) :scroll-down)
                                          ((< 0 (delta thing)) :scroll-up)
                                          (T :middle)))
                      (mouse-button-event (button thing))
                      (mouse-move (let ((dp (v- (pos thing) (old-pos thing))))
                                    (cond ((< (vx dp) (vy dp)) :up-down)
                                          ((< 0 (abs (vx dp))) :left-right)
                                          (T :any))))
                      (input-event)))
            (bank (or bank
                      (etypecase thing
                        ((or gamepad-added gamepad-removed) :devices)
                        (gamepad-event
                         (if (typep +input-source+ 'gamepad:device)
                             (gamepad:icon-type +input-source+)
                             :gamepad))
                        (keyboard-event :keyboard)
                        (mouse-event :mouse)
                        (input-event)))))
       (let ((char (or (when (and prompt (eql bank :keyboard))
                         (local-key-string *context* prompt))
                       prompt)))
         (values (prompt-string char :bank bank :default default)
                 char))))))

(defun action-prompts (thing &key bank)
  (recombine-prompts
   (case (normalize-prompt-bank bank)
     (:keyboard
      (append (specific-prompts-for-event-trigger thing 'key-event)
              (specific-prompts-for-event-trigger thing 'mouse-event)))
     (:mouse
      (specific-prompts-for-event-trigger thing 'mouse-event))
     (T
      (specific-prompts-for-event-trigger thing 'gamepad-event)))))

(defun action-strings (thing &key bank)
  (let ((bank (normalize-prompt-bank bank))
        (prompts ()))
    (loop for prompt in (action-prompts thing :bank bank)
          for string = (when prompt
                         (if (eql bank :keyboard)
                             (or (ignore-errors (local-key-string *context* prompt))
                                 (prompt-string prompt :bank bank)
                                 (prompt-string prompt :bank :mouse))
                             (prompt-string prompt :bank bank)))
          do (when string (pushnew string prompts)))
    (nreverse prompts)))

(defun action-string (thing &key bank (join " "))
  (with-output-to-string (out)
    (loop for (string . next) on (action-strings thing :bank bank)
          do (write-string string out)
             (when next
               (write-string join out)))))

(defun prompt-charset ()
  (sort (delete-duplicates
         (with-output-to-string (out)
           (loop for bank being the hash-keys of *prompt-string-table* using (hash-value table)
                 do (loop for string being the hash-values of table
                          do (write-string string out)))))
        #'string<))

(define-shader-entity prompt ()
  ())

(defmethod (setf text) ((character character) (prompt prompt))
  (setf (text prompt) (string character)))

(defmethod (setf text) ((symbol symbol) (prompt prompt))
  (setf (text prompt) (prompt-string symbol :default "<unbound>")))

(defmethod (setf prompt-icon) (char (prompt prompt) &key (bank :gamepad))
  (setf (text prompt) (prompt-string char :bank bank)))
