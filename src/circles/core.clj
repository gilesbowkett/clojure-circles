(ns circles.core
  (:use quil.core))

; "hello world" for Quil meets the bit101 thing

(def the-width 808)
(def the-height 500)
(def the-distance-threshold 200)

(defn circle-as-list []
  ; x y x-velocity y-velocity
  (list (int (rand the-width)) 5 (int (rand the-height)) 5))

(def circle-positions
  (atom (list (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list))))

(defn draw-circle [circle]
  (stroke 171 163 225)
  (stroke-weight 1)
  (fill 13 9 40)

  (let [diam 8
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

; worked, but non-idiomatic, and memory problems
; (defn bubble-coordinates [list-a list-b]
;   (loop [result ()
;          list-a list-a
;          list-b list-b]
;     (if (empty? list-a)
;         result
;         (recur (concat result (map (fn [bubble]
;                                      (list (first list-a) bubble))
;                                    list-b)
;                               result) (rest list-a) list-b))))

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
    (if (distance-within-threshold (nth circle-a 0)
                                   (nth circle-a 2)
                                   (nth circle-b 0)
                                   (nth circle-b 2))
      (line (nth circle-a 0)
            (nth circle-a 2)
            (nth circle-b 0)
            (nth circle-b 2)))))

(defn draw-lines [bubbles]
  (doall (map draw-line (distinct (bubble-coordinates bubbles bubbles)))))

(defn draw-circles []
  (background 0)
  (swap! circle-positions move-circles)
  ; this triggers OutOfMemory errors for reasons I do not know at all
  (draw-lines @circle-positions)
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

