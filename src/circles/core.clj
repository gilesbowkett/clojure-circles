; namespace and requires (clojure plumbing)
(ns circles.core
  (:require [quil.core :as q])
  (:require [overtone.live :as o]))

; Quil (Processing) for visuals
; Overtone (SuperCollider) for MIDI input

; clojure quirk for concurrency / software-transactional memory
; tldr: global variables
(def diameter                 (atom 10))
(def stroke-red               (atom '(171 3)))
(def stroke-blue              (atom '(163 23)))
(def stroke-green             (atom '(225 7)))
(def stroke-weight            (atom 1))
(def reset-background         (atom false))
(def the-distance-threshold   (atom 150))
(def draw-circles?            (atom false))
(def draw-lines?              (atom false))
(def cycle-colors?            (atom false))

; fixme: macro?
; change red when knob moves
(defn mutate-red-from-finger [finger] ; fixme: terrible naming
  (swap! stroke-red (fn [blarg] (list (int (* 1.9 finger)) 3))))

; change blue when knob moves
(defn mutate-blue-from-finger [finger] ; fixme: terrible naming
  (swap! stroke-blue (fn [blarg] (list (int (* 1.9 finger)) 23))))

; change green when knob moves
(defn mutate-green-from-finger [finger] ; fixme: terrible naming
  (swap! stroke-green (fn [blarg] (list (int (* 1.9 finger)) 7))))

; event handler: "do this any time any MIDI info comes in"
(o/on-event [:midi :control-change]
  (fn [{note :note data :data1 velocity :velocity}]
        (println note data velocity) ; this use of velocity makes no sense to me at all btw
        (case note
          ; controller numbers link up to changes the controllers cause

             ; tempo knob controls circle diameter
           7 (swap! diameter (fn [blarg] (* 3 velocity)))

             ; volume knob controls length of lines
          10 (swap! the-distance-threshold (fn [blarg] (* 2 (+ velocity (int (rand 25))))))

             ; P1 knob controls red
          16 (mutate-red-from-finger velocity)

             ; P2 knob controls blue
          17 (mutate-blue-from-finger velocity)

             ; P3 knob controls green
          18 (mutate-green-from-finger velocity)

             ; big selection knob controls line thickness
          19 (swap! stroke-weight (fn [blarg] velocity))

             ; "Instr" button toggles circle drawing
          76 (swap! draw-circles? (fn [d-c] (not d-c)))

             ; "Kit" button toggles line drawing
          77 (swap! draw-lines? (fn [d-l] (not d-l)))

             ; "Proj" button toggles color cycling
          78 (swap! cycle-colors? (fn [c-c] (not c-c)))

             ; pushing big selection knob as button toggles background modes
          90 (swap! reset-background (fn [reset-bkg] (not reset-bkg))))
  ) ::note-printer)

; global vars for drawing stuff
(def the-width 808)
(def the-height 500)

; represent a circle as a list, with x coordinate, y coordinate,
; and velocities also for x and y. you need velocities to track
; which direction it's going, so you can make it bounce when
; it hits an edge.
(defn circle-as-list []
  ; x y x-velocity y-velocity
  (list (int (rand the-width)) 5 (int (rand the-height)) 5))

; start things off with a bunch of random circles
(def circle-positions
  (atom (list (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list))))

; draw a circle
(defn draw-circle [circle]
  (let [x (nth circle 0)
        y (nth circle 2)]
    (q/ellipse x y @diameter @diameter)))

; move a circle in the x dimension (horizontal)
(defn move-x [x x-velocity]
  (if (or (>= (+ x x-velocity) the-width)
          (<= (+ x x-velocity) 0))
      (list x (* -1 x-velocity))
      (list (+ x-velocity x) x-velocity)))

; move a circle in the y dimension (vertical)
(defn move-y [y y-velocity]
  (if (or (>= (+ y y-velocity) the-height)
          (<= (+ y y-velocity) 0))
      (list y (* -1 y-velocity))
      (list (+ y-velocity y) y-velocity)))

; move a circle
(defn move-circle [circle]
  (let [x (nth circle 0)
        x-velocity (nth circle 1)
        y (nth circle 2)
        y-velocity (nth circle 3)

        new-x-vel (move-x x x-velocity)
        new-y-vel (move-y y y-velocity)]

    ; move 99% of circles; teleport 1%
    (if (> 0.99 (rand))
      (list (first new-x-vel)
            (second new-x-vel)
            (first new-y-vel)
            (second new-y-vel))
      (list (int (rand the-width))
            (int (rand 7))
            (int (rand the-height))
            (int (rand 11))))))

; move all the circles
(defn move-circles [circles]
  (map move-circle circles))

; a list of every bubble, mapped to every other bubble
; (list comprehension, like Haskell, Python, or CoffeeScript)
(defn bubble-coordinates [seq-1 seq-2]
  (for [elem-1 seq-1 elem-2 seq-2] [elem-1 elem-2]))

; is the other circle close enough to draw a line to?
(defn distance-within-threshold [x-1 y-1 x-2 y-2]
  (> @the-distance-threshold
     (Math/sqrt (+ (Math/pow (- x-1 x-2) 2)
                   (Math/pow (- y-1 y-2) 2)))))

; draw a line between two circles
(defn draw-line [pair]
  (let [circle-a (first pair)
        circle-b (second pair)]
    (if (distance-within-threshold (nth circle-a 0)   ; x1
                                   (nth circle-a 2)   ; y1
                                   (nth circle-b 0)   ; x2
                                   (nth circle-b 2))  ; y2
      (q/line (nth circle-a 0)     ; x1
              (nth circle-a 2)     ; y1
              (nth circle-b 0)     ; x2
              (nth circle-b 2))))) ; y2

; draw all the lines
(defn draw-lines [bubbles]
  (doall (map draw-line (distinct (bubble-coordinates bubbles bubbles)))))

; continually change colors
(defn cycle-color [hue-and-velocity]
  (let [hue (first hue-and-velocity)
        velocity (second hue-and-velocity)]
    (if (or (>= hue 255) (<= hue 0))
        (list (+ hue (* -1 velocity)) (* -1 velocity))
        (list (+ hue velocity) velocity))))

; line color & thickness; also circle color
(defn set-line-characteristics []

  (q/stroke-weight @stroke-weight)

  (if @cycle-colors?
    (do (swap! stroke-red cycle-color)
        (swap! stroke-blue cycle-color)
        (swap! stroke-green cycle-color)))
  (q/stroke (first @stroke-red)
            (first @stroke-blue)
            (first @stroke-green))

  (q/fill 0))

; main drawing loop
(defn draw []
  (if @reset-background (q/background 0))
  (set-line-characteristics)
  (swap! circle-positions move-circles)
  (if @draw-lines?
    (draw-lines @circle-positions))
  (if @draw-circles?
    (doall (map draw-circle @circle-positions)))
)

; initial conditions
(defn setup []
  (q/smooth)
  (q/background 0)
  (q/frame-rate 35))

; Quil (Processing) thing: set up your "sketch"
(q/defsketch circles
  :title "Circles"
  :setup setup
  :draw draw
  :size [the-width the-height])

; leiningen boilerplate

(defn -main [] ())

