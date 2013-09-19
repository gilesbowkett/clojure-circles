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

(defn mutate-rgb [rgb velocity knob-movement]
  (swap! rgb (fn [_] (list (int (* 1.9 knob-movement)) velocity))))

(def mutate-red (partial mutate-rgb stroke-red 3))
(def mutate-blue (partial mutate-rgb stroke-blue 23))
(def mutate-green (partial mutate-rgb stroke-green 7))

; event handler: "do this any time any MIDI info comes in"
(o/on-event [:midi :control-change]
  ; arturia sparkle seems to send velocity where it should send data, and vice versa?
  (fn [{note :note velocity :data1 data :velocity}]
        (println note velocity data)
        (case note
          ; controller numbers link up to changes the controllers cause

             ; tempo knob controls circle diameter
           7 (swap! diameter (fn [blarg] (* 3 data)))

             ; volume knob controls length of lines
          10 (swap! the-distance-threshold (fn [blarg] (* 2 (+ data (int (rand 25))))))

             ; P1 knob controls red
          16 (mutate-red data)

             ; P2 knob controls blue
          17 (mutate-blue data)

             ; P3 knob controls green
          18 (mutate-green data)

             ; big selection knob controls line thickness
          19 (swap! stroke-weight (fn [blarg] data))

             ; "Instr" button toggles circle drawing
          76 (swap! draw-circles? not)

             ; "Kit" button toggles line drawing
          77 (swap! draw-lines? not)

             ; "Proj" button toggles color cycling
          78 (swap! cycle-colors? not)

             ; pushing big selection knob as button toggles background modes
          90 (swap! reset-background not))

  ) ::note-printer)

; global vars for drawing stuff
(def the-width 808)
(def the-height 500)

; represent a circle as a map, with x coordinate, y coordinate,
; and velocities also for x and y. you need velocities to track
; which direction it's going, so you can make it bounce when
; it hits an edge.
(defn circle-as-map []
  {:x (int (rand the-width)), :x-velocity 5, :y (int (rand the-height)), :y-velocity 5})

; start things off with a bunch of random circles
(def circle-positions
  (atom (list (circle-as-map) (circle-as-map) (circle-as-map)
              (circle-as-map) (circle-as-map) (circle-as-map)
              (circle-as-map) (circle-as-map) (circle-as-map)
              (circle-as-map) (circle-as-map))))

; draw a circle
(defn draw-circle [circle]
  (let [x (circle :x)
        y (circle :y)]
    (q/ellipse x y @diameter @diameter)))

; FIXME: dry. macros? function composition?
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
  (let [x (circle :x)
        x-velocity (circle :x-velocity)
        y (circle :y)
        y-velocity (circle :y-velocity)

        new-x-vel (move-x x x-velocity)
        new-y-vel (move-y y y-velocity)]

    ; move 99% of circles; teleport 1%
    (if (> 0.99 (rand))
      {:x (first new-x-vel),
       :x-velocity (second new-x-vel),
       :y (first new-y-vel),
       :y-velocity (second new-y-vel)}
      {:x (int (rand the-width)),
       :x-velocity (int (rand 7)),
       :y (int (rand the-height)),
       :y-velocity (int (rand 11))})))

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
    (if (distance-within-threshold (circle-a :x)
                                   (circle-a :y)
                                   (circle-b :x)
                                   (circle-b :y))
      (q/line (circle-a :x)
              (circle-a :y)
              (circle-b :x)
              (circle-b :y)))))

; draw all the lines
(defn draw-lines [bubbles]
  (dorun (map draw-line (distinct (bubble-coordinates bubbles bubbles)))))

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

  (when @cycle-colors?
    (swap! stroke-red cycle-color)
    (swap! stroke-blue cycle-color)
    (swap! stroke-green cycle-color))
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
    (dorun (map draw-circle @circle-positions)))
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

