(ns repl-asteroids.core
  (:require [quil.core :refer :all]
            [repl-asteroids.quil-helpers :refer :all]))

(def window-width 800)
(def window-height 500)

(defn new-ship []
  { :x (/ window-width 2)  :vx 0
    :y (/ window-height 2) :vy 0
    :angle 0               :vangle 0
    :image "ship.png"    :width 49    :height 16
    :thrust 0})

(defn new-asteroid []
  { :x (rand-int window-width)  :vx (- (rand) 0.5)
    :y (rand-int window-height) :vy (- (rand) 0.5)
    :angle (rand (* 2 Math/PI)) :vangle (* (- (rand) 0.5) 0.2)
    :image "asteroid.png" :width 64    :height 64 })


(def game-state-atom (atom { :ship (new-ship)
                             :asteroids [(new-asteroid) (new-asteroid) (new-asteroid)]
                             :bullets []}))

(defn setup []
  (smooth)
  (frame-rate 30)
  (image-mode :center))



;; updating
(defn update [state]
  state)


;;drawing
(defn draw-thing [thing]
  (with-translation [(:x thing) (:y thing)]
    (with-rotation [(:angle thing)]
      (image (get-image (:image thing)) 0 0 (:width thing) (:height thing)))))

(defn draw []
  (swap! game-state-atom update)
  (background 4 4 16)
  (let [state @game-state-atom]
    (dorun (map draw-thing (:asteroids state)))
    (dorun (map draw-thing (:bullets state)))
    (draw-thing (:ship state)))
  (text (str (int (current-frame-rate)) " fps") 10 20))



;; gui
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

;(main)
