(ns clj-asteroids.core
  (:gen-class)
  (:require [quil.core :refer :all]))



(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))



(defonce preloaded-images-atom (atom {}))

(defn preload-image [path]
  (swap! preloaded-images-atom #(assoc % path (load-image path))))

(defn get-image [path]
  (get @preloaded-images-atom path))



(def game-atom (atom { :player { :x 200 :y 100
                                 :vx 0.2 :vy 0.0 :va 0.1
                                 :image "player.png"
                                 :angle (/ Math/PI 4) }}))



(defn update-entity [entity]
  (-> entity
      (update-in [:x] #(+ % (:vx entity)))
      (update-in [:y] #(+ % (:vy entity)))
      (update-in [:angle] #(+ % (:va entity)))))




(defn setup []
  (preload-image "player.png")
  (smooth)
  (frame-rate 30)
  (background 8 8 32))

(defn draw []
  (background 8 8 32)
  (swap! game-atom #(fmap update-entity %))
  (doseq [[k obj] @game-atom]
    (with-translation [(:x obj) (:y obj)]
      (with-rotation [(:angle obj)]
        (image (get-image (:image obj)) 0 0 32 32)))))

(defn -main [& args]
  (defsketch example
    :title "Asteroids"
    :setup setup
    :draw draw
    :size [600 500]))

; (-main)
