(ns circles.core
  (:use quil.core))

; based on the "hello world" for Quil and the bit101 thing

(defn setup []
  (smooth)
  (frame-rate 20)
  (background 0))

(defn random-circle []
  (stroke 171 163 225)
  (stroke-weight 5)
  (fill 213 209 240)

  (let [diam (random 100)
        x    (random (width))
        y    (random (height))]
    (ellipse x y diam diam)))

(defn draw []
  (random-circle)
)

(defsketch example
  :title "Circles"
  :setup setup
  :draw draw
  :size [808 500])

(defn -main [] ()) ; lein wants a `-main`, but quil doesn't need it

