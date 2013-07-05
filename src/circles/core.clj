(ns circles.core
  (:require [quil.core :as q])
  (:require [overtone.live :as o]))

; 16, 17, 18: red, green, blue

; Overtone

(def diameter (atom 10))

(def stroke-red               (atom '(171 3)))
(def stroke-blue              (atom '(163 23)))
(def stroke-green             (atom '(225 7)))
(def stroke-weight            (atom 1))
(def reset-background         (atom false))
(def the-distance-threshold   (atom 150))
(def draw-circles?            (atom false))
(def draw-lines?              (atom false))
(def cycle-colors?            (atom false))

; fixme: macro?
(defn mutate-red-from-finger [finger] ; fixme: terrible naming
  (swap! stroke-red (fn [blarg] (list (int (* 1.9 finger)) 3))))

(defn mutate-blue-from-finger [finger] ; fixme: terrible naming
  (swap! stroke-blue (fn [blarg] (list (int (* 1.9 finger)) 23))))

(defn mutate-green-from-finger [finger] ; fixme: terrible naming
  (swap! stroke-green (fn [blarg] (list (int (* 1.9 finger)) 7))))

(o/on-event [:midi :control-change]
  (fn [{note :note data :data1 velocity :velocity}]
        (println note data velocity) ; this use of velocity makes no sense to me at all btw
        (case note
           7 (swap! diameter (fn [blarg] (* 3 velocity)))
          10 (swap! the-distance-threshold (fn [blarg] (* 2 (+ velocity (int (rand 25))))))
          16 (mutate-red-from-finger velocity)
          17 (mutate-blue-from-finger velocity)
          18 (mutate-green-from-finger velocity)
          19 (swap! stroke-weight (fn [blarg] velocity))
          76 (swap! draw-circles? (fn [d-c] (not d-c)))
          77 (swap! draw-lines? (fn [d-l] (not d-l)))
          78 (swap! cycle-colors? (fn [c-c] (not c-c)))
          90 (swap! reset-background (fn [reset-bkg] (not reset-bkg))))
  ) ::note-printer)

; Quil (Processing)

(def the-width 808)
(def the-height 500)

(def one-cos-memo (atom 1))
(defn one-cos-sq [number]
  (/ 1 (Math/pow (Math/cos number) 2)))

(defn circle-as-list []
  ; x y x-velocity y-velocity
  (list (int (rand the-width)) 5 (int (rand the-height)) 5))

(def circle-positions
  (atom (list (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list) (circle-as-list)
              (circle-as-list) (circle-as-list))))

(defn draw-circle [circle]
  (let [x (nth circle 0)
        y (nth circle 2)]
    (q/ellipse x y @diameter @diameter)))

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

    (if (> 0.99 (rand))
      (list (first new-x-vel)
            (second new-x-vel)
            (first new-y-vel)
            (second new-y-vel))
      (list (int (rand the-width))
            (int (rand 7))
            (int (rand the-height))
            (int (rand 11))))))

(defn move-circles [circles]
  (map move-circle circles))

(defn bubble-coordinates [seq-1 seq-2]
  (for [elem-1 seq-1 elem-2 seq-2] [elem-1 elem-2]))

(defn distance-within-threshold [x-1 y-1 x-2 y-2]
  (> @the-distance-threshold
     (Math/sqrt (+ (Math/pow (- x-1 x-2) 2)
                   (Math/pow (- y-1 y-2) 2)))))

(defn draw-line [pair]
  (let [circle-a (first pair)
        circle-b (second pair)]
    (if (distance-within-threshold (nth circle-a 0)   ; x1
                                   (nth circle-a 2)   ; y1
                                   (nth circle-b 0)   ; x2
                                   (nth circle-b 2))  ; y2
      (q/line (nth circle-a 0)     ; x1
              (nth circle-a 2)     ; y1
              (nth circle-b 0)     ; x2
              (nth circle-b 2))))) ; y2

(defn draw-lines [bubbles]
  (doall (map draw-line (distinct (bubble-coordinates bubbles bubbles)))))

(defn cycle-color [hue-and-velocity]
  (let [hue (first hue-and-velocity)
        velocity (second hue-and-velocity)]
    (if (or (>= hue 255) (<= hue 0))
        (list (+ hue (* -1 velocity)) (* -1 velocity))
        (list (+ hue velocity) velocity))))

(def stroke-modulation-rate-throttle (atom 0))

(defn set-line-characteristics []

  ; (swap! stroke-modulation-rate-throttle inc)
  ; (if (= 0 (rem @stroke-modulation-rate-throttle 14))
  ;    (q/stroke-weight (int (swap! one-cos-memo one-cos-sq))))
  (q/stroke-weight @stroke-weight)

  (if @cycle-colors?
  (do (swap! stroke-red cycle-color)
      (swap! stroke-blue cycle-color)
      (swap! stroke-green cycle-color)))
  (q/stroke (first @stroke-red)
            (first @stroke-blue)
            (first @stroke-green))

  (q/fill 0))

(defn draw []
  ; (if (= 0 (rem @stroke-modulation-rate-throttle 300))
  ;   (if (> 0.9 (rand))
  ;     (q/background 255)
  ;     (q/background 0)))
  (if @reset-background (q/background 0))
  (set-line-characteristics)
  (swap! circle-positions move-circles)
  (if @draw-lines?
    (draw-lines @circle-positions))
  (if @draw-circles?
    (doall (map draw-circle @circle-positions)))
)

(defn setup []
  (q/smooth)
  (q/background 0)
  (q/frame-rate 35))

(q/defsketch circles
  :title "Circles"
  :setup setup
  :draw draw
  :size [the-width the-height])

; leiningen boilerplate

(defn -main [] ())

