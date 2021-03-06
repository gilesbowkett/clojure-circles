(ns circles.core
  (:use quil.core))

; Quil (Processing)

(def the-width 1200)
(def the-height 700)
(def the-distance-threshold 150)

(def one-cos-memo (atom 1))
(defn one-cos-sq [number]
  (/ 1 (Math/pow (Math/cos number) 2)))

(defn circle-as-list []
  ; x y x-velocity y-velocity
  (list (rand-int the-width) 5 (rand-int the-height) 5))

(def circle-positions
  (atom (list (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list))))

(defn draw-circle [circle]
  (let [diam 20
        x (nth circle 0)
        y (nth circle 2)]
    (ellipse x y diam diam)))

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

        new-x-vector (move-x x x-velocity)
        new-y-vector (move-y y y-velocity)]

    (if (> 0.99 (rand))
      (list (first new-x-vector)
            (second new-x-vector)
            (first new-y-vector)
            (second new-y-vector))
      (list (rand-int the-width)
            (rand-int 7)
            (rand-int the-height)
            (rand-int 11)))))

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
(def stroke-green (atom '(225 7)))

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

  (fill 255 255 255))

(defn draw []
  (if (zero? (rem @stroke-modulation-rate-throttle 100))
    (if (> 0.9 (rand))
      (background 255)
      (background 0)))
  (set-line-characteristics)
  (swap! circle-positions move-circles)
  (draw-lines @circle-positions)
  (dorun (map draw-circle @circle-positions)))

(defn setup []
  (smooth)
  (frame-rate 35))

(japplemenubar.JAppleMenuBar/hide)

; (.. (Class/forName "japplemenubar.JAppleMenuBar") (.getDeclaredMethod "hide") (.invoke nil))

(defsketch circles
  :title "Circles"
  :setup setup
  :draw draw
  ; :decor false
  :size :fullscreen)

; leiningen boilerplate

(defn -main [] ())

