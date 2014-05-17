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
  { :entity-type :ship
    :health 100
    :children (list)
    :image "player.png"
    :radius 32
    :x 200 :y 100 :angle (/ Math/PI 4)
    :vx 0.0 :vy 0.0 :va 0.0 :thrust 0
    :max-speed 5 })

(defn new-bullet [source]
  (-> source
      (assoc :entity-type :bullet)
      (assoc :children (list))
      (update-in [:x] #(+ % (* (:radius source) 0.6 (Math/cos (:angle source)))))
      (update-in [:y] #(+ % (* (:radius source) 0.6 (Math/sin (:angle source)))))
      (assoc :vx (+ (:vx source) (* 8 (Math/cos (:angle source)))))
      (assoc :vy (+ (:vy source) (* 8 (Math/sin (:angle source)))))
      (assoc :va 0)
      (assoc :thrust 0)
      (assoc :image "bullet.png")
      (assoc :radius 8)
      (assoc :max-speed 10)
      (assoc :ttl 3000)))

(defn new-asteroid [radius]
  { :entity-type :asteroid
    :children (list)
    :radius radius
    :x (rand-int 600) :y (rand-int 500) :angle (* (rand) Math/PI 2)
    :vx (- (* 6 (rand)) 3) :vy (- (* 6 (rand)) 3) :va (* (- (rand) 0.5) 0.1)
    :max-speed 4
    :points (for [angle (range 10)]
              (let [angle (+ angle (* (rand) -0.5) 0.1)
                    radians (* Math/PI 2 (/ angle 10.0))
                    r (+ radius (rand-int 10) 5)
                    px (* r (Math/cos radians))
                    py (* r (Math/sin radians))]
                [px py]))})

(defn new-asteroid-from [a]
  (-> (new-asteroid (* (:radius a) 0.66))
      (assoc :x (+ (:x a) (rand-int 10) -5))
      (assoc :y (+ (:y a) (rand-int 10) -5))
      (assoc :vx (* 1.15 (+ (:vx a) (- (rand-int 6) 3))))
      (assoc :vy (* 1.15 (+ (:vy a) (- (rand-int 6) 3))))
      (assoc :va (+ (:va a) (* (- (rand) 0.5) 0.05)))))

;; time
(def previous-time-atom (atom 0))

(defn elapsed-time []
  (- (millis) @previous-time-atom))


;; state
(def next-id-atom (atom 0))

(defn next-id []
  (swap! next-id-atom inc))

(def game-atom (atom (-> {}
                         (assoc :player (new-player))
                         (assoc (next-id) (new-asteroid 32))
                         (assoc (next-id) (new-asteroid 22))
                         (assoc (next-id) (new-asteroid 12)))))



;; collision detection and collision update
(defn collides? [a b]
  (let [dx (- (:x a) (:x b))
        dy (- (:y a) (:y b))
        r (+ (:radius a) (:radius b))]
    (> (* r r) (+ (* dx dx) (* dy dy)))))

(defmulti collide
  (fn [a b] [(:entity-type a) (:entity-type b)]))

(defmethod collide [:asteroid :bullet] [a b]
  (if (> 5 (:radius a))
    (assoc a :ttl -1)
    (-> a
        (assoc :ttl -1)
        (update-in [:children] #(conj % (new-asteroid-from a)))
        (update-in [:children] #(conj % (new-asteroid-from a))))))

(defmethod collide [:bullet :asteroid] [a b]
  (assoc a :ttl -1))

(defmethod collide [:ship :asteroid] [a b]
  (-> a
      (update-in [:vx] #(+ % (* 0.05 (- (:x a) (:x b)))))
      (update-in [:vy] #(+ % (* 0.05 (- (:y a) (:y b)))))
      (update-in [:health] #(- % (* (:radius b) 0.1)))))

(defmethod collide :default [a b]
  a)

(defn do-collisions-on [e game]
  (let [[_ other] (first game)
        remaining (rest game)]
    (cond
     (empty? remaining)         e
     (and (not (= e other))
          (collides? e other))  (recur (collide e other) remaining)
     :else                      (recur e remaining))))

(defn do-collisions [game]
  (map-values #(do-collisions-on % game) game))


;; updates
(defn wrap-around [v min max]
  (cond
   (< v min)  (+ v max)
   (> v max)  (- v max)
   :else      v))

(defn apply-thrust [e]
  (if (:thrust e)
    (-> e
        (update-in [:vx] #(+ % (* (:thrust e) (Math/cos (:angle e)))))
        (update-in [:vy] #(+ % (* (:thrust e) (Math/sin (:angle e))))))
    e))

(defn clamp-velocity [e]
  (let [max-speed (:max-speed e)
        v (Math/sqrt (+ (* (:vx e) (:vx e)) (* (:vy e) (:vy e))))]
    (if (< v max-speed)
      e
      (-> e
          (update-in [:vx] #(* max-speed (/ % v)))
          (update-in [:vy] #(* max-speed (/ % v)))))))

(defn apply-velocity [e]
  (-> e
     (update-in [:x] #(wrap-around (+ % (:vx e)) 0 (width)))
     (update-in [:y] #(wrap-around (+ % (:vy e)) 0 (height)))
     (update-in [:angle] #(+ % (:va e)))))

(defn update-entity [e]
  (-> e
      (apply-thrust)
      (clamp-velocity)
      (apply-velocity)
      (safe-update-in [:ttl] #(- % (elapsed-time)))))

(defn add-children [game]
  (let [children (mapcat #(:children (val %)) game)
        game2 (map-values #(assoc % :children (list)) game)]
    (into game2 (map #(identity [(next-id) %]) children))))

(defn update-game [game]
  (->> game
       (do-collisions)
       (map-values update-entity)
       (add-children)
       (filter-values #(or (nil? (:ttl %)) (< 0 (:ttl %))))))


;; user input
(defn translate-key [k]
  (get { :a :left :d :right :w :up :l :fire :f :fire } k k))

(defn key-pressed []
  (case (translate-key (key-as-keyword))
    :left  (swap! game-atom (fn [game] (assoc-in  game [:player :va] -0.1)))
    :right (swap! game-atom (fn [game] (assoc-in  game [:player :va]  0.1)))
    :up    (swap! game-atom (fn [game] (assoc-in  game [:player :thrust] 0.2)))
    :fire  (swap! game-atom (fn [game] (update-in game [:player :children] #(conj % (new-bullet (:player game))))))
    :p     (println @game-atom)
           nil))

(defn key-released []
  (case (translate-key (key-as-keyword))
    :left  (swap! game-atom (fn [game] (assoc-in game [:player :va] 0)))
    :right (swap! game-atom (fn [game] (assoc-in game [:player :va] 0)))
    :up    (swap! game-atom (fn [game] (assoc-in game [:player :thrust] 0)))
           nil))


;; drawing
(defn draw-asteroid-at [e x y]
  (with-fill [200]
    (with-stroke [0]
      (with-translation [x y]
        (with-rotation [(:angle e)]
          (begin-shape :triangle-fan)
          (vertex 0 0)
          (doseq [[x y] (:points e)]
              (vertex x y))
          (let [[x y] (first (:points e))]
              (vertex x y))
          (end-shape))))))

(defn draw-image-at [e x y]
  (with-translation [x y]
      (with-rotation [(:angle e)]
        (with-translation [(- (/ (:radius e) 2)) (- (/ (:radius e) 2))]
          (image (get-image (:image e)) 0 0 (:radius e) (:radius e))))))

(defn draw-with [f e]
  (let [r (:radius e)]
    (f e (:x e) (:y e))
    (when (< (- (:x e) r)   0)     (f e (+ (:x e) 600)    (:y e)))
    (when (> (+ (:x e) r) 600)     (f e (- (:x e) 600)    (:y e)))
    (when (< (- (:y e) r)   0)     (f e    (:x e)      (+ (:y e) 500)))
    (when (> (+ (:y e) r) 500)     (f e    (:x e)      (- (:y e) 500)))))

(defn draw-entity [e]
  (if (:image e)
    (draw-with draw-image-at e)
    (draw-with draw-asteroid-at e)))


;; quil
(defn setup []
  (preload-image "player.png")
  (preload-image "bullet.png")
  (smooth)
  (stroke-weight 0)
  (frame-rate 30)
  (background 8 8 32))

(defn draw []
  (background 8 8 32)
  (doseq [[k e] @game-atom]
    (draw-entity e))
  (text (str (Math/floor (:health (:player @game-atom))) "% health") 20 20)
  (swap! game-atom update-game)
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
