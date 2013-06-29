(ns circles.core
  (:use quil.core))

; "hello world" for Quil meets the bit101 thing

(def the-width 808)
(def the-height 500)

(defn circle-as-list []
  ; x y x-velocity y-velocity
  (list (int (rand the-width)) (int (rand the-height)) 1 1))

(def circle-positions
  (atom (list (circle-as-list) (circle-as-list))))

(defn draw-circle [circle]
  (stroke 171 163 225)
  (stroke-weight 5)
  (fill 213 209 240)

  (let [diam 76
        x (first circle)
        y (second circle)]
    (ellipse x y diam diam)))

(defn move-circle [circle]
  (let [x (first circle)
        y (second circle)
        x-velocity (nth circle 2)
        y-velocity (nth circle 3)]
    (list (+ x-velocity x)
          (+ y-velocity y)
          x-velocity
          y-velocity)))

(defn move-circles [circles]
  (map move-circle circles))

(defn draw-circles []
  (background 0)
  (swap! circle-positions move-circles)
  (doall (map draw-circle @circle-positions)))

(defn setup []
  (smooth)
  (frame-rate 20)
  (background 0))

(defsketch example
  :title "Circles"
  :setup setup
  :draw draw-circles
  :size [the-width the-height])

(defn -main [] ())

