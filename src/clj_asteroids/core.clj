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



(defn new-player []
  { :image "player.png"
    :w 32 :h 32
    :x 200 :y 100 :angle (/ Math/PI 4)
    :vx 0.0 :vy 0.0 :va 0.0 :thrust 0 })

(defn new-bullet [source]
  (-> source
      (assoc :vx (+ (:vx source) (* 5 (Math/cos (:angle source)))))
      (assoc :vy (+ (:vy source) (* 5 (Math/sin (:angle source)))))
      (assoc :va 0)
      (assoc :thrust 0)
      (assoc :image "bullet.png")
      (assoc :w 8)
      (assoc :h 8)))



(def game-atom (atom { :player (new-player)}))



(defn wrap-around [v min max]
  (cond
   (< v min)  (+ v max)
   (> v max)  (- v max)
   :else      v))

(defn update-entity [entity]
  (-> entity
      (update-in [:vx] #(+ % (* (:thrust entity) (Math/cos (:angle entity)))))
      (update-in [:vy] #(+ % (* (:thrust entity) (Math/sin (:angle entity)))))
      (update-in [:x] #(wrap-around (+ % (:vx entity)) 0 (width)))
      (update-in [:y] #(wrap-around (+ % (:vy entity)) 0 (height)))
      (update-in [:angle] #(+ % (:va entity)))))



(defn setup []
  (preload-image "player.png")
  (preload-image "bullet.png")
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
  (get { :a :left :d :right :w :up :l :fire } k k))

(defn key-pressed []
  (case (translate-key (key-as-keyword))
    :left  (swap! game-atom (fn [game] (update-in game [:player :va] #(- % 0.1))))
    :right (swap! game-atom (fn [game] (update-in game [:player :va] #(+ % 0.1))))
    :up    (swap! game-atom (fn [game] (update-in game [:player :thrust] #(+ % 0.1))))
    :fire  (swap! game-atom (fn [game] (assoc-in  game [(count game)] (new-bullet (:player game)))))
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
