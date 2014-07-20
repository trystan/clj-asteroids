(ns repl-asteroids.core
  (:require [quil.core :refer :all]
            [repl-asteroids.quil-helpers :refer :all]))

(def window-width 800)
(def window-height 500)

(def game-state-atom (atom {}))

(defn setup []
  (smooth)
  (frame-rate 30)
  (image-mode :center))

(defn update [state]
  state)

(defn draw []
  (swap! game-state-atom update)
  (background 4 4 16)
  (text (str (int (current-frame-rate)) " fps") 10 20))

(defn key-pressed []
  nil)

(defn key-released []
  nil)

(defn main []
  (defsketch demo
    :title "Asteroids"
    :size [window-width window-height]
    :setup setup
    :draw draw
    :key-pressed key-pressed
    :key-released key-released))
