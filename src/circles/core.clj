; namespace and requires (clojure plumbing)
(ns circles.core
  (:require [quil.core :as q])
  (:require [overtone.live :as o]))

; Quil (Processing) for visuals
; Overtone (SuperCollider) for MIDI input

; clojure quirk for concurrency / software-transactional memory
; tldr: global variables
(def diameter                 (atom 75))
(def stroke-red               (atom '(171 3)))
(def stroke-blue              (atom '(163 23)))
(def stroke-green             (atom '(225 7)))
(def stroke-weight            (atom 1))
(def reset-background         (atom false))
(def the-distance-threshold   (atom 150))
(def draw-circles?            (atom false))
(def draw-lines?              (atom true))

; fixme: dry (use partial, probably)
(defn kick []
  (swap! stroke-red
    (fn [hue-and-velocity]
      (let [hue (first hue-and-velocity)
            velocity (second hue-and-velocity)]
        (if (> 255 (+ 10 hue))
            (list 255 velocity)
            (list (+ 10 hue) velocity)))))
  (swap! stroke-weight
    (fn [stroke-weight]
      (if (< 125 (+ stroke-weight 15))
          stroke-weight
          (+ stroke-weight 15)))))

(defn snare []
  (swap! stroke-red
    (fn [hue-and-velocity]
      (let [hue (first hue-and-velocity)
            velocity (second hue-and-velocity)]
        (if (> 155 (+ 50 hue))
            (list 155 velocity)
            (list (+ 50 hue) velocity)))))
  (swap! stroke-blue
    (fn [hue-and-velocity]
      (let [hue (first hue-and-velocity)
            velocity (second hue-and-velocity)]
        (if (> 255 (+ 50 hue))
            (list 255 velocity)
            (list (+ 50 hue) velocity)))))
  (swap! stroke-weight
    (fn [stroke-weight]
      (if (< 125 (+ stroke-weight 25))
          stroke-weight
          (+ stroke-weight 25)))))

(defn crash []
  (swap! reset-background not)
  (swap! stroke-red
    (fn [hue-and-velocity]
      (let [hue (first hue-and-velocity)
            velocity (second hue-and-velocity)]
        (if (< 0 (- 2 hue))
            (list 0 velocity)
            (list (- 2 hue) velocity)))))
  (swap! stroke-green
    (fn [hue-and-velocity]
      (let [hue (first hue-and-velocity)
            velocity (second hue-and-velocity)]
        (if (> 255 (+ 25 hue))
            (list 255 velocity)
            (list (+ 25 hue) velocity)))))
  (swap! stroke-weight
    (fn [stroke-weight]
      (if (> 0 (- stroke-weight 10))
          stroke-weight
          (- stroke-weight 10)))))

(defn hat []
  (swap! stroke-green
    (fn [hue-and-velocity]
      (list 100 10)))
  (swap! stroke-red
    (fn [hue-and-velocity]
      (list 130 10)))
  (swap! stroke-green
    (fn [hue-and-velocity]
      (list 250 10)))
  (swap! stroke-weight
    (fn [weight] 1)))

; event handler: "do this any time any MIDI message comes in"
(o/on-event [:midi :note-on]
  ; arturia sparkle (or overtone) seems to send velocity where it should send data, and vice versa?
  (fn [{note-number :note velocity :velocity}]
        (println note-number velocity)
        (case note-number
          64 (kick)
          60 (crash)
          67 (hat)
          65 (snare)
  )) ::note-printer)

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

(defn move [position velocity boundary]
  (let [destination (+ position velocity)]
    (if (or (>= destination boundary)
            (<= destination 0))
        (list position (* -1 velocity))
        (list (+ position velocity) velocity))))

; move a circle
(defn move-circle [circle]
  (let [x (circle :x)
        x-velocity (circle :x-velocity)
        y (circle :y)
        y-velocity (circle :y-velocity)

        new-x-vector (move x x-velocity the-width)
        new-y-vector (move y y-velocity the-height)]

    ; move 99% of circles; teleport 1%
    (if (> 0.99 (rand))
      {:x (first new-x-vector),
       :x-velocity (second new-x-vector),
       :y (first new-y-vector),
       :y-velocity (second new-y-vector)}
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
(defn decay-color [hue-and-velocity]
  (let [hue (first hue-and-velocity)
        velocity (second hue-and-velocity)]
    (if (> hue 0)
        (list (- hue velocity) velocity)
        (list hue velocity))))

; continually shrink circles
(defn shrink-circles [diameter]
    (if (> diameter 5)
        (- diameter 1)
        5))

; line color & thickness; also circle color
(defn set-line-characteristics []

  (q/stroke-weight @stroke-weight)

  (swap! diameter shrink-circles)

  (swap! stroke-red decay-color)
  (swap! stroke-blue decay-color)
  (swap! stroke-green decay-color)
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
    (dorun (map draw-circle @circle-positions))))

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

