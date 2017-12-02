(in-package #:org.shirakumo.fraf.trial)

;;; General name mapping conventions:
;; :left-h      -- Left analog stick, horizontal movement
;; :left-v      -- Left analog stick, vertical movement
;; :right-h     -- Right analog stick, horizontal movement
;; :right-v     -- Right analog stick, vertical movement
;; :dpad-h      -- Directional pad, horizontal movement
;; :dpad-v      -- Directional pad, vertical movement
;; :dpad-up     -- Directional pad up
;; :dpad-right  -- Directional pad right
;; :dpad-down   -- Directional pad down
;; :dpad-left   -- Directional pad left
;; :l1          -- Left upper trigger or bumper
;; :l2          -- Left lower trigger or bumper
;; :r1          -- Right upper trigger or bumper
;; :r2          -- Right lower trigger or bumper
;; :y           -- Upper button (Y on Xbox pads)
;; :b           -- Right button (B on Xbox pads)
;; :a           -- Lower button (A on Xbox pads)
;; :x           -- Left button  (X on Xbox pads)
;; :left        -- Left analog stick click
;; :right       -- Right analog stick click
;; :select      -- Left menu button
;; :home        -- Middle menu button
;; :start       -- Right menu button
;;
;; Gamepads with special hardware may have additional axes
;; and buttons and thus additional names. If you wish to
;; use those, see the respective mapping table for the
;; device

(define-gamepad xbox-360 (1118 654)
  #-windows
  (:axes
   ( 0 :left-h)
   ( 1 :left-v)
   ( 2 :l2)
   ( 3 :right-h)
   ( 4 :right-v)
   ( 5 :r2)
   ( 6 :dpad-h)
   ( 7 :dpad-v))
  #+windows
  (:axes
   ( 0 :left-h)
   ( 1 :left-v)
   ( 2 :right-h)
   ( 3 :right-v)
   ( 4 :l2)
   ( 5 :r2)
   ( 6 :dpad-h)
   ( 7 :dpad-v))
  #-windows
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :select)
   ( 7 :start)
   ( 8 :home)
   ( 9 :left)
   (10 :right))
  #+windows
  (:buttons
   ( 0 :up)
   ( 1 :down)
   ( 2 :left)
   ( 3 :right)
   ( 4 :start)
   ( 5 :select)
   ( 6 :l3)
   ( 7 :r3)
   ( 8 :l2)
   ( 9 :r2)
   (10 :a)
   (11 :b)
   (12 :x)
   (13 :y)
   (14 :home)))

(define-gamepad logitech-f310 (1133 49693 :inherit xbox-360)
  (:axes)
  (:buttons))

(define-gamepad dualshock-3 (1356 616)
  (:axes
   ( 0 :left-h)
   ( 1 :left-v)
   ( 2 :right-h)
   ( 3 :right-v)
   ( 8 :dpad-up)
   ( 9 :dpad-right)
   (10 :dpad-down)
   (11 :dpad-left)
   (12 :l2)
   (13 :r2)
   (14 :l1)
   (15 :r1)
   (16 :y) ; triangle
   (17 :b) ; circle
   (18 :a) ; cross
   (19 :x) ; square
   (23 :axis-x)
   (24 :axis-z)
   (25 :axis-y)
   (26 :axis-r))
  (:buttons
   ( 0 :select)
   ( 1 :left)
   ( 2 :right)
   ( 3 :start)
   ( 4 :dpad-up)
   ( 5 :dpad-right)
   ( 6 :dpad-down)
   ( 7 :dpad-left)
   ( 8 :l2)
   ( 9 :r2)
   (10 :l1)
   (11 :r1)
   (12 :y) ; triangle
   (13 :b) ; circle
   (14 :a) ; cross
   (15 :x) ; square
   (16 :home)))

(define-gamepad dualshock-4 (1356 2508)
  (:axes
   ( 0 :left-h)
   ( 1 :left-v)
   ( 2 :l2)
   ( 3 :right-h)
   ( 4 :right-v)
   ( 5 :r2))
  (:buttons
   ( 0 :a)
   ( 1 :b)
   ( 2 :y)
   ( 3 :x)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :l2)
   ( 7 :r2)
   ( 8 :select)
   ( 9 :start)
   (10 :home)
   (11 :left)
   (12 :right)))

(define-gamepad buffalo-bsgp801 (1411 8288)
  (:axes
   ( 0 :dpad-h)
   ( 1 :dpad-v))
  (:buttons
   ( 0 :b)
   ( 1 :a)
   ( 2 :x)
   ( 3 :y)
   ( 4 :l1)
   ( 5 :r1)
   ( 6 :select)
   ( 7 :start)))

(define-gamepad steam-controller (10462 4604 :inherit xbox-360)
  (:axes)
  (:buttons))
