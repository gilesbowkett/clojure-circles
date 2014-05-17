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
; TODO: parameterize the background color

(defn mutate-rgb [rgb velocity knob-movement]
  (swap! rgb (fn [_] (list (int (* 1.9 knob-movement)) velocity))))

(def mutate-red (partial mutate-rgb stroke-red 3))
(def mutate-blue (partial mutate-rgb stroke-blue 23))
(def mutate-green (partial mutate-rgb stroke-green 7))

; this preset function needs to become way less verbose, and it should be pretty
; easy, but the cool thing is, it makes it possible for you to put all the color
; settings on one note, and all the "what shapes to draw" settings on another
; note, and then control those factors by playing chords. that probably won't make
; it into this weekend's performance, but it's a nice design.

(defn preset [settings]
  (swap! diameter (fn [_] (settings :diameter))) ; epic FIXME DRY
  (swap! stroke-red (fn [_] (settings :stroke-red)))
  (swap! stroke-blue (fn [_] (settings :stroke-blue)))
  (swap! stroke-green (fn [_] (settings :stroke-green)))
  (swap! stroke-weight (fn [_] (settings :stroke-weight)))
  (swap! reset-background (fn [_] (settings :reset-background)))
  (swap! the-distance-threshold (fn [_] (settings :the-distance-threshold)))
  (swap! draw-circles? (fn [_] (settings :draw-circles?)))
  (swap! draw-lines? (fn [_] (settings :draw-lines?)))
  (swap! cycle-colors? (fn [_] (settings :cycle-colors?))))

(o/on-event [:midi :note-on]
  (fn [{note-number :note velocity :velocity}]
        (println "on" note-number velocity)

        (case note-number

          36 ( ; big, stringy network graph
               preset {:diameter 15
                        :stroke-red '(255 0)
                        :stroke-blue '(255 0)
                        :stroke-green '(255 0)
                        :stroke-weight 3
                        :reset-background true
                        :the-distance-threshold 500
                        :draw-circles? true
                        :draw-lines? true
                        :cycle-colors? false})
          38 ( ; blue simple network graph
               preset {:diameter 125
                        :stroke-red '(75 0)
                        :stroke-blue '(215 0)
                        :stroke-green '(100 0)
                        :stroke-weight 35
                        :reset-background true
                        :the-distance-threshold 250
                        :draw-circles? true
                        :draw-lines? true
                        :cycle-colors? false})
          42 ( ; flashing blue
               preset {:diameter 100
                        :stroke-red '(0 0)
                        :stroke-blue '(215 50)
                        :stroke-green '(0 0)
                        :stroke-weight 25
                        :reset-background true
                        :the-distance-threshold 250
                        :draw-circles? true
                        :draw-lines? false
                        :cycle-colors? true})
          46 ( ; pulsing red network
               preset {:diameter 10
                        :stroke-red '(235 52)
                        :stroke-blue '(15 0)
                        :stroke-green '(15 0)
                        :stroke-weight 1
                        :reset-background false
                        :the-distance-threshold 250
                        :draw-circles? true
                        :draw-lines? true
                        :cycle-colors? true})
          50 ( ; adventure time!
               preset {:diameter 100
                        :stroke-red '(255 3)
                        :stroke-blue '(255 2)
                        :stroke-green '(255 1)
                        :stroke-weight 57
                        :reset-background false
                        :the-distance-threshold 250
                        :draw-circles? true
                        :draw-lines? true
                        :cycle-colors? true})
          47 ( ; glowsticks
               preset {:diameter 500
                        :stroke-red '(255 1)
                        :stroke-blue '(255 5)
                        :stroke-green '(255 10)
                        :stroke-weight 100
                        :reset-background false
                        :the-distance-threshold 150
                        :draw-circles? true
                        :draw-lines? true
                        :cycle-colors? true})
          43 ( ; slow-ass greyscale
               preset {:diameter 100
                        :stroke-red '(255 1)
                        :stroke-blue '(255 1)
                        :stroke-green '(255 1)
                        :stroke-weight 11
                        :reset-background false
                        :the-distance-threshold 250
                        :draw-circles? true
                        :draw-lines? true
                        :cycle-colors? true})
          49 (
               preset {:diameter 55
                        :stroke-red '(171 3)
                        :stroke-blue '(225 7)
                        :stroke-green '(163 23)
                        :stroke-weight 1
                        :reset-background false
                        :the-distance-threshold 150
                        :draw-circles? true
                        :draw-lines? true
                        :cycle-colors? true}))
  ) ::handle-note-on)

(o/on-event [:midi :note-off]
  (fn [{note-number :note velocity :velocity}]
        (println "off" note-number velocity)
        ; actual functionality goes here
  ) ::handle-note-off)

; event handler: "do this any time any MIDI control change message comes in"
(o/on-event [:midi :control-change]
  ; arturia sparkle seems to send velocity where it should send data, and vice versa?
  (fn [{controller-number :note velocity :data1 data :velocity}]
        (println "cc" controller-number velocity data)
        (case controller-number
          ; controller numbers link up to changes the controllers cause

             ; tempo knob controls circle diameter
           7 (swap! diameter (fn [_] (* 3 data)))

             ; volume knob controls length of lines
          10 (swap! the-distance-threshold (fn [_] (* 2 (+ data (int (rand 25))))))

             ; P1 knob controls red
          16 (mutate-red data)

             ; P2 knob controls blue
          17 (mutate-blue data)

             ; P3 knob controls green
          18 (mutate-green data)

             ; big selection knob controls line thickness
          19 (swap! stroke-weight (fn [_] data))

             ; "Instr" button toggles circle drawing
          76 (swap! draw-circles? not)

             ; "Kit" button toggles line drawing
          77 (swap! draw-lines? not)

             ; "Proj" button toggles color cycling
          78 (swap! cycle-colors? not)

             ; pushing big selection knob as button toggles background modes
          90 (swap! reset-background not))

  ) ::handle-midi-cc) ; gratuitous names, wtf

; global vars for drawing stuff
(def the-width 1280)
(def the-height 800)

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
            (first @stroke-green)
            (first @stroke-blue))

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
  :size :fullscreen
  :features [:present])

; leiningen boilerplate

(defn -main [] ())

