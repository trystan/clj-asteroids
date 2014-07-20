(ns repl-asteroids.core
  (:require [quil.core :refer :all]
            [quil.middleware :refer :all]
            [repl-asteroids.quil-helpers :refer :all]))

(def window-width 800)
(def window-height 500)

(def initial-state {})

(defn setup []
  (smooth)
  (frame-rate 30)
  (image-mode :center)
  initial-state)

(defn update [state]
  state)

(defn draw [state]
  (background 4 4 16)
  (text (str (int (current-frame-rate)) " fps") 10 20))

(defn key-pressed [state event]
  state)

(defn key-released [state]
  state)

(defn main []
  (defsketch demo
    :title "Asteroids"
    :size [window-width window-height]
    :setup setup
    :update update
    :draw draw
    :key-pressed key-pressed
    :key-released key-released
    :middleware [fun-mode]))
