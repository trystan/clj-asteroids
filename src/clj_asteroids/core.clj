(ns clj-asteroids.core
  (:gen-class)
  (:require [quil.core :refer :all]))


(defn setup []
  (smooth)
  (frame-rate 30)
  (background 8 8 32))

(defn draw []
  (background 8 8 32))

(defn -main [& args]
  (defsketch example
    :title "Asteroids"
    :setup setup
    :draw draw
    :size [600 500]))
