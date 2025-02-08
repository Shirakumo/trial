(in-package #:org.shirakumo.fraf.trial)

(defparameter *keyboard-layout*
  '(((:ESCAPE 1) (NIL 1) :F1 :F2 :F3 :F4 (NIL 1) :F5 :F6 :F7 :F8 (NIL 1) :F9 :F10 :F11 :F12 () :PRINT  :SCROLL :BREAK     ())
    ((:SECTION 1) :1 :2 :3 :4 :5 :6 :7 :8 :9 :0 :MINUS :EQUAL                (:BACKSPACE 1) () :INSERT :HOME   :PAGE-UP   ())
    ((:TAB 1) :Q :W :E :R :T :Y :U :I :O :P :LEFT-BRACKET :RIGHT-BRACKET         (:ENTER 1) () :DELETE :END    :PAGE-DOWN ())
    ((:CAPS-LOCK 1) :A :S :D :F :G :H :J :K :L :SEMICOLON :APOSTROPHE :BACKSLASH (:ENTER 1) () ()      ()      ()         ())
    ((:LEFT-SHIFT 1) :Z :X :C :V :B :N :M :COMMA :PERIOD :SLASH            (:RIGHT-SHIFT 1) () ()      :UP     ()         ())
    (:LEFT-CONTROL :LEFT-SUPER :LEFT-ALT (:SPACE 1) :RIGHT-ALT               :RIGHT-CONTROL () :LEFT   :DOWN   :RIGHT     ())))

(define-global +input-source+ :keyboard)

(define-event input-event (event))
(define-event keyboard-event (input-event))
(define-event digital-event (input-event))
(define-event press-event (input-event))
(define-event release-event (input-event))

(defgeneric button (digital-event))

(define-event key-event (keyboard-event digital-event)
  (key arg! :reader key :reader button) (repeat NIL :reader repeat-p) (modifiers ()))

(defmethod print-object ((event key-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~s" (key event))))

(define-event key-press (key-event press-event))
(define-event key-release (key-event release-event))

(define-event text-entered (keyboard-event)
  text (replace NIL :reader replace-p))

(defmethod print-object ((event text-entered) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~s" (text event))))

(define-event mouse-event (input-event)
  pos)

(define-event mouse-button-event (mouse-event digital-event)
  button)

(defmethod print-object ((event mouse-button-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~s" (button event))))

(define-event mouse-press (mouse-button-event press-event))
(define-event mouse-release (mouse-button-event release-event))
(define-event mouse-double-click (mouse-button-event))
(define-event mouse-scroll (mouse-event)
  delta)

(defmethod print-object ((event mouse-scroll) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a" (delta event))))

(define-event mouse-move (mouse-event)
  old-pos)

(defmethod print-object ((event mouse-move) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a => ~a" (old-pos event) (pos event))))

(define-event file-drop-event (mouse-event)
  paths)

(define-event gamepad-event (input-event)
  device)

(define-event gamepad-button-event (gamepad-event digital-event)
  button)

(defmethod print-object ((event gamepad-button-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a ~s" (device event) (button event))))

(define-event gamepad-press (gamepad-button-event press-event))
(define-event gamepad-release (gamepad-button-event release-event))

(define-event gamepad-move (gamepad-event)
  axis old-pos pos)

(defmethod print-object ((event gamepad-move) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a ~s ~3f" (device event) (axis event) (pos event))))

(define-event gamepad-added (gamepad-event))
(define-event gamepad-removed (gamepad-event))
