(ns circles.core
  (:use quil.core))

; "hello world" for Quil meets the bit101 thing

(def the-width 808)
(def the-height 500)

(defn circle-as-list []
  ; x y x-velocity y-velocity
  (list (int (rand the-width)) 5 (int (rand the-height)) 5))

(def circle-positions
  (atom (list (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list)
              (circle-as-list))))

(defn draw-circle [circle]
  (stroke 171 163 225)
  (stroke-weight 5)
  (fill 213 209 240)

  (let [diam 3
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

        new-x-vel (move-x x x-velocity)
        new-y-vel (move-y y y-velocity)]

    (list (first new-x-vel)
          (second new-x-vel)
          (first new-y-vel)
          (second new-y-vel))))

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

