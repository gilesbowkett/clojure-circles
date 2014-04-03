(ns circles.core
  (:use quil.core))

; Quil (Processing)

(def the-width 808)
(def the-height 500)
(def the-distance-threshold 150)

(def one-cos-memo (atom 1))
(defn one-cos-sq [number]
  (/ 1 (Math/pow (Math/cos number) 2)))

(defn circle-as-list []
  ; x y x-velocity y-velocity rotation
  (list (rand-int the-width) 5 (rand-int the-height) 5 (- 180 (rand-int 360))))

(def circle-positions
  (atom (list (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list))))

(defn draw-circle [circle]
  (let [diam 20
        x (nth circle 0)
        y (nth circle 2)
        rotation (nth circle 4)]
    (rotate (radians rotation))
    (triangle x (- y 75)
                (+ x 40) (+ y 15)
                (- x 40) (+ y 15))))

(defn move-x [x x-velocity]
  (if (or (>= (+ x x-velocity) the-width)
          (<= (+ x x-velocity) 0))
      (list x (* -1 x-velocity))
      (list (+ x-velocity x) x-velocity)))

(defn move-y [y y-velocity]
  (if (or (>= (+ y y-velocity) the-height)
          (<= (+ y y-velocity) 0))
      (list y (* -1 y-velocity))
      (list (+ y-velocity y) y-velocity)))

(defn move-circle [circle]
  (let [x (nth circle 0)
        x-velocity (nth circle 1)
        y (nth circle 2)
        y-velocity (nth circle 3)
        rotation (+ 0.2 (nth circle 4)) ; FIXME: use idiomatic Clojure

        new-x-vector (move-x x x-velocity)
        new-y-vector (move-y y y-velocity)]

    (if (> 0.99 (rand))
      (list (first new-x-vector)
            (second new-x-vector)
            (first new-y-vector)
            (second new-y-vector)
            rotation)
      (list (rand-int the-width)
            (rand-int 7)
            (rand-int the-height)
            (rand-int 11)
            rotation))))

(defn move-circles [circles]
  (map move-circle circles))

; idiomatic version, using sequence comprehensions
; https://gist.github.com/gilesbowkett/5893860
(defn bubble-coordinates [seq-1 seq-2]
  (for [elem-1 seq-1 elem-2 seq-2] [elem-1 elem-2]))

(defn distance-within-threshold [x-1 y-1 x-2 y-2]
  (> the-distance-threshold
     (Math/sqrt (+ (Math/pow (- x-1 x-2) 2)
                   (Math/pow (- y-1 y-2) 2)))))

(defn draw-line [pair]
  (let [circle-a (first pair)
        circle-b (second pair)]
    (if (distance-within-threshold (nth circle-a 0)   ; x1
                                   (nth circle-a 2)   ; y1
                                   (nth circle-b 0)   ; x2
                                   (nth circle-b 2))  ; y2
      (line (nth circle-a 0)     ; x1
            (nth circle-a 2)     ; y1
            (nth circle-b 0)     ; x2
            (nth circle-b 2))))) ; y2

(defn draw-lines [bubbles]
  (dorun (map draw-line (distinct (bubble-coordinates bubbles bubbles)))))

;                         hue velocity
(def stroke-red   (atom '(171 3)))
(def stroke-blue  (atom '(163 23)))
(def stroke-green (atom '(225 0.1)))

(defn cycle-color [hue-and-velocity]
  (let [hue (first hue-and-velocity)
        velocity (second hue-and-velocity)]
    (if (or (>= hue 255) (<= hue 0))
        (list (+ hue (* -1 velocity)) (* -1 velocity))
        (list (+ hue velocity) velocity))))

(def stroke-modulation-rate-throttle (atom 0))

(defn set-line-characteristics []

  (swap! stroke-modulation-rate-throttle inc)
  (if (zero? (rem @stroke-modulation-rate-throttle 14))
     (stroke-weight (int (swap! one-cos-memo one-cos-sq))))

  (swap! stroke-red cycle-color)
  (swap! stroke-blue cycle-color)
  (swap! stroke-green cycle-color)
  (stroke (first @stroke-red)
          (first @stroke-blue)
          (first @stroke-green))
  (no-fill))

(defn draw []
  (if (zero? (rem @stroke-modulation-rate-throttle 100))
    (if (> 0.9 (rand))
      (background 0)
      (background 255)))
  (set-line-characteristics)
  (swap! circle-positions move-circles)
  (draw-lines @circle-positions)
  (dorun (map draw-circle @circle-positions)))

(defn setup []
  (smooth)
  (frame-rate 35))

(defsketch circles
  :title "Circles"
  :setup setup
  :draw draw
  :size [the-width the-height])

; leiningen boilerplate

(defn -main [] ())

