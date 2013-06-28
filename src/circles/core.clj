(ns circles.core
  (:use quil.core))

; based on the "hello world" for Quil and the bit101 thing

(def the-width 808)
(def the-height 500)

(defn setup []
  (smooth)
  (frame-rate 20)
  (background 0))

(defn circle [x y]
  (stroke 171 163 225)
  (stroke-weight 5)
  (fill 213 209 240)

  (let [diam (random 100)]
    (ellipse x y diam diam)))

(defn random-circle []
  (circle (random the-width) (random the-height)))

(defn draw []
  (random-circle))

(defsketch example
  :title "Circles"
  :setup setup
  :draw draw
  :size [the-width the-height])

(defn -main [] ()) ; lein wants a `-main`, but quil doesn't need it

