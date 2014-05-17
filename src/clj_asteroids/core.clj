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
                                 :vx 0.0 :vy 0.0 :va 0.0 :thrust 0
                                 :w 32 :h 32
                                 :image "player.png"
                                 :angle (/ Math/PI 4) }}))



(defn update-entity [entity]
  (-> entity
      (update-in [:vx] #(+ % (* (:thrust entity) (Math/cos (:angle entity)))))
      (update-in [:vy] #(+ % (* (:thrust entity) (Math/sin (:angle entity)))))
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
        (with-translation [(- (/ (:w obj) 2)) (- (/ (:h obj) 2))]
          (image (get-image (:image obj)) 0 0 (:w obj) (:h obj)))))))

(defn translate-key [k]
  (get { :a :left :d :right :w :up } k k))

(defn key-pressed []
  (case (translate-key (key-as-keyword))
    :left  (swap! game-atom (fn [game] (update-in game [:player :va] #(- % 0.1))))
    :right (swap! game-atom (fn [game] (update-in game [:player :va] #(+ % 0.1))))
    :up    (swap! game-atom (fn [game] (update-in game [:player :thrust] #(+ % 0.1))))
           (println "Unexpected key press" (key-as-keyword))))

(defn key-released []
  (case (translate-key (key-as-keyword))
    :left  (swap! game-atom (fn [game] (update-in game [:player :va] #(+ % 0.1))))
    :right (swap! game-atom (fn [game] (update-in game [:player :va] #(- % 0.1))))
    :up    (swap! game-atom (fn [game] (update-in game [:player :thrust] #(- % 0.1))))
           (println "Unexpected key release" (key-as-keyword))))


(defn -main [& args]
  (defsketch example
    :title "Asteroids"
    :setup setup
    :draw draw
    :size [600 500]
    :key-pressed key-pressed
    :key-released key-released))

; (-main)
