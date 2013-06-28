(ns barnsley-fern.core
  (:use quil.core))

; based on the "hello world" for Quil. I think the strategy here is:
;
; 1. separate fern.lisp music code from fern.lisp visual code
; 2. translate fern.lisp visual code into Clojure
; 3. slot translated Clojure into this Quil setup
; 4. if time left, translate fern.lisp music code
; OR 4. if time left throw in some original music

(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 1)                    ;;Set framerate to 1 FPS
  (background 0))                   ;;Set the background colour to
                                    ;;  a nice shade of grey.
(defn draw []
  (stroke (random 255))             ;;Set the stroke colour to a random grey
  (stroke-weight (random 10))       ;;Set the stroke thickness randomly
  (fill (random 255))               ;;Set the fill colour to a random grey

  (let [diam (random 100)           ;;Set the diameter to a value between 0 and 100
        x    (random (width))       ;;Set the x coord randomly within the sketch
        y    (random (height))]     ;;Set the y coord randomly within the sketch
    (ellipse x y diam diam)))       ;;Draw a circle at x y with the correct diameter

(defsketch example                  ;;Define a new sketch named example
  :title "Barnsley's fern"          ;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :draw draw                        ;;Specify the draw fn
  :size [808 500])                  ;;You struggle to beat the golden ratio

(defn -main [] ()) ; lein wants a `-main`, but quil doesn't need it

