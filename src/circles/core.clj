(ns circles.core
  (:use quil.core))

; "hello world" for Quil meets the bit101 thing

(def the-width 808)
(def the-height 500)

(defn random-x-y []
  (list (int (rand the-width)) (int (rand the-height))))

(defn circle [xy]
  (stroke 171 163 225)
  (stroke-weight 5)
  (fill 213 209 240)

  (let [diam (random 100)
        x (first xy)
        y (second xy)]
    (ellipse x y diam diam)))

(defn setup []
  (smooth)
  (frame-rate 20)
  (background 0)
  (circle (random-x-y)))

(defn draw [])

(defsketch example
  :title "Circles"
  :setup setup
  :draw draw
  :size [the-width the-height])

(defn -main [] ())

