(ns clj-asteroids.core
  (:gen-class)
  (:require [quil.core :refer :all]))


;; helper functions
(defn map-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn filter-values [f m]
  (into {} (filter (comp f val) m)))

(defn safe-update-in [m p f]
  (if (= m (update-in m p identity))
    (update-in m p f)
    m))


;; images
(defonce preloaded-images-atom (atom {}))

(defn preload-image [path]
  (swap! preloaded-images-atom #(assoc % path (load-image path))))

(defn get-image [path]
  (get @preloaded-images-atom path))


;; constructors
(defn new-player []
  { :image "player.png"
    :w 32 :h 32
    :x 200 :y 100 :angle (/ Math/PI 4)
    :vx 0.0 :vy 0.0 :va 0.0 :thrust 0
    :max-speed 5 })

(defn new-bullet [source]
  (-> source
      (update-in [:x] #(+ % (* (:w source) 0.6 (Math/cos (:angle source)))))
      (update-in [:y] #(+ % (* (:w source) 0.6 (Math/sin (:angle source)))))
      (assoc :vx (+ (:vx source) (* 8 (Math/cos (:angle source)))))
      (assoc :vy (+ (:vy source) (* 8 (Math/sin (:angle source)))))
      (assoc :va 0)
      (assoc :thrust 0)
      (assoc :image "bullet.png")
      (assoc :w 8)
      (assoc :h 8)
      (assoc :max-speed 10)
      (assoc :ttl 3000)))


;; time
(def previous-time-atom (atom 0))

(defn elapsed-time []
  (- (millis) @previous-time-atom))


;; state
(def game-atom (atom { :player (new-player) }))

(def next-id-atom (atom 0))

(defn next-id []
  (swap! next-id-atom inc))


;; updates
(defn wrap-around [v min max]
  (cond
   (< v min)  (+ v max)
   (> v max)  (- v max)
   :else      v))

(defn clamp-speed [entity]
  (let [max-speed (:max-speed entity)
        v (Math/sqrt (+ (* (:vx entity) (:vx entity)) (* (:vy entity) (:vy entity))))]
    (if (< v max-speed)
      entity
      (-> entity
          (update-in [:vx] #(* max-speed (/ % v)))
          (update-in [:vy] #(* max-speed (/ % v)))))))

(defn update-entity [entity]
  (-> entity
      (update-in [:vx] #(+ % (* (:thrust entity) (Math/cos (:angle entity)))))
      (update-in [:vy] #(+ % (* (:thrust entity) (Math/sin (:angle entity)))))
      (clamp-speed)
      (update-in [:x] #(wrap-around (+ % (:vx entity)) 0 (width)))
      (update-in [:y] #(wrap-around (+ % (:vy entity)) 0 (height)))
      (update-in [:angle] #(+ % (:va entity)))
      (safe-update-in [:ttl] #(- % (elapsed-time)))))

(defn update-game [game]
  (->> game
       (map-values update-entity)
       (filter-values #(or (nil? (:ttl %)) (< 0 (:ttl %))))))


;; user input
(defn translate-key [k]
  (get { :a :left :d :right :w :up :l :fire } k k))

(defn key-pressed []
  (case (translate-key (key-as-keyword))
    :left  (swap! game-atom (fn [game] (update-in game [:player :va] #(- % 0.1))))
    :right (swap! game-atom (fn [game] (update-in game [:player :va] #(+ % 0.1))))
    :up    (swap! game-atom (fn [game] (assoc-in  game [:player :thrust] 0.2)))
    :fire  (swap! game-atom (fn [game] (assoc     game (next-id) (new-bullet (:player game)))))
    :p     (println @game-atom)
           (println "Unexpected key press" (key-as-keyword))))

(defn key-released []
  (case (translate-key (key-as-keyword))
    :left  (swap! game-atom (fn [game] (update-in game [:player :va] #(+ % 0.1))))
    :right (swap! game-atom (fn [game] (update-in game [:player :va] #(- % 0.1))))
    :up    (swap! game-atom (fn [game] (assoc-in  game [:player :thrust] 0)))
           nil))


;; quil
(defn setup []
  (preload-image "player.png")
  (preload-image "bullet.png")
  (smooth)
  (frame-rate 30)
  (background 8 8 32))

(defn draw []
  (background 8 8 32)
  (swap! game-atom update-game)
  (doseq [[k obj] @game-atom]
    (with-translation [(:x obj) (:y obj)]
      (with-rotation [(:angle obj)]
        (with-translation [(- (/ (:w obj) 2)) (- (/ (:h obj) 2))]
          (image (get-image (:image obj)) 0 0 (:w obj) (:h obj))))))
  (reset! previous-time-atom (millis)))


(defn -main [& args]
  (defsketch example
    :title "Asteroids"
    :setup setup
    :draw draw
    :size [600 500]
    :key-pressed key-pressed
    :key-released key-released))

; (-main)
