(ns clj-asteroids.core
  (:gen-class)
  (:require [quil.core :refer :all]))

(def WIDTH 1024)
(def HEIGHT 768)

;; utility functions
(defn map-values
  "map a function over the values of a map"
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn filter-values
  "filter a map based on its values"
  [f m]
  (into {} (filter (comp f val) m)))

(defn safe-update-in
  "like update-in, except it counts as the identity function if the path isn't in the map"
  [m p f]
  (if (= m (update-in m p identity))
    (update-in m p f)
    m))


;; image cahce
(defonce preloaded-images-atom (atom {}))

(defn preload-image [path]
  (swap! preloaded-images-atom #(assoc % path (load-image path))))

(defn get-image [path]
  (get @preloaded-images-atom path))


;; entity constructors
(defn new-player []
  { :entity-type :ship
    :health 100
    :children (list)
    :image "player.png"
    :radius 32
    :x (* 0.5 WIDTH) :y (* 0.5 HEIGHT) :angle (rand (* 2 Math/PI))
    :vx 0.0 :vy 0.0 :va 0.0 :thrust 0
    :max-speed 3 })

(defn new-bullet [source]
  (merge source
         { :entity-type :bullet
           :children (list)
           :x (+ (:x source) (* (:radius source) 0.6 (Math/cos (:angle source))))
           :y (+ (:y source) (* (:radius source) 0.6 (Math/sin (:angle source))))
           :vx (+ (:vx source) (* 8 (Math/cos (:angle source))))
           :vy (+ (:vy source) (* 8 (Math/sin (:angle source))))
           :va 0
           :thrust 0
           :image "bullet.png"
           :radius 8
           :max-speed 5
           :ttl 3000 }))

(defn new-asteroid [radius]
  { :entity-type :asteroid
    :children (list)
    :radius radius
    :x (rand-int WIDTH) :y (rand-int HEIGHT) :angle (* (rand) Math/PI 2)
    :vx (- (* 6 (rand)) 3) :vy (- (* 6 (rand)) 3) :va (* (- (rand) 0.5) 0.1)
    :max-speed 3.5
    :points (for [angle (range 10)]
              (let [angle (+ angle (* (rand) -0.5) 0.1)
                    radians (* Math/PI 2 (/ angle 10.0))
                    r (+ radius (rand-int 10) 5)
                    px (* r (Math/cos radians))
                    py (* r (Math/sin radians))]
                [px py]))})

(defn new-asteroid-from [a]
  (merge (new-asteroid (* (:radius a) 0.66))
         { :x (+ (:x a) (rand-int 10) -5)
           :y (+ (:y a) (rand-int 10) -5)
           :vx (* 1.1 (+ (:vx a) (- (rand) 0.5)))
           :vy (* 1.1 (+ (:vy a) (- (rand) 0.5)))
           :va (+ (:va a) (* (- (rand) 0.5) 0.05)) }))

(defn new-game [] (-> {}
                   (assoc :player   (new-player))
                   (assoc (next-id) (new-asteroid 32))
                   (assoc (next-id) (new-asteroid 22))
                   (assoc (next-id) (new-asteroid 12)) ))


;; global state
(def previous-time-atom (atom 0))

(defn elapsed-time []
  (- (millis) @previous-time-atom))

(def next-id-atom (atom 0))

(defn next-id []
  (swap! next-id-atom inc))

(def game-atom (atom {}))


;; collision detection and response
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
      (update-in [:vx] #(+ % (* 0.1 (:vx b)) (* 0.02 (- (:x a) (:x b)))))
      (update-in [:vy] #(+ % (* 0.1 (:vy b)) (* 0.02 (- (:y a) (:y b)))))
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


;; entity update functions
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
     (update-in [:x] #(wrap-around (+ % (:vx e)) 0 WIDTH))
     (update-in [:y] #(wrap-around (+ % (:vy e)) 0 HEIGHT))
     (update-in [:angle] #(+ % (:va e)))))

(defn apply-ttl [e]
  (safe-update-in e [:ttl] #(- % (elapsed-time))))

(defn update-entity [e]
  (-> e
      (apply-thrust)
      (clamp-velocity)
      (apply-velocity)
      (apply-ttl)))

(defn promote-children-to-entities [game]
  (let [children (mapcat #(:children (val %)) game)
        game2 (map-values #(assoc % :children (list)) game)]
    (into game2 (map #(identity [(next-id) %]) children))))

(defn is-alive? [e]
  (or (nil? (:ttl e)) (< 0 (:ttl e))))

(defn update-game [game]
  (->> game
       (do-collisions)
       (map-values update-entity)
       (promote-children-to-entities)
       (filter-values is-alive?)))

(open-window)

;; user input
(defn translate-key [k]
  (get { :a :left :d :right :w :up :space :fire :enter :fire } k k))

(defn get-key []
  (translate-key
   (case (key-code)
     10       :enter
     32       :space
              (key-as-keyword))))

(defn key-pressed []
  (case (get-key)
    :left  (swap! game-atom (fn [game] (assoc-in  game [:player :va] -0.05)))
    :right (swap! game-atom (fn [game] (assoc-in  game [:player :va]  0.05)))
    :up    (swap! game-atom (fn [game] (assoc-in  game [:player :thrust] 0.1)))
    :fire  (swap! game-atom (fn [game] (update-in game [:player :children] #(conj % (new-bullet (:player game))))))
    :p     (println @game-atom)
           nil))

(defn key-released []
  (case (get-key)
    :left  (swap! game-atom (fn [game] (assoc-in game [:player :va] 0)))
    :right (swap! game-atom (fn [game] (assoc-in game [:player :va] 0)))
    :up    (swap! game-atom (fn [game] (assoc-in game [:player :thrust] 0)))
           nil))


;; drawing
(defn draw-asteroid-at [e x y]
  (with-fill [200 150 140]
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

(defn draw-using [f e]
  (let [x (:x e)
        y (:y e)
        r (:radius e)]
    (f e x y)
    (when (< (- x r)      0)     (f e (+ x WIDTH)    y))
    (when (> (+ x r)  WIDTH)     (f e (- x WIDTH)    y))
    (when (< (- y r)      0)     (f e    x      (+ y HEIGHT)))
    (when (> (+ y r) HEIGHT)     (f e    x      (- y HEIGHT)))))

(defn draw-entity [e]
  (if (:image e)
    (draw-using draw-image-at e)
    (draw-using draw-asteroid-at e)))

(defn draw-hud [player]
  (text (str (int (:health player)) "% health") 20 20)
  (text (str (int (current-frame-rate)) " fps") (- WIDTH 60) 20))


;; quil
(defn setup []
  (preload-image "player.png")
  (preload-image "bullet.png")
  (smooth)
  (stroke-weight 0)
  (frame-rate 60)
  (background 8 8 32)
  (reset! game-atom (new-game)))

(defn draw []
  (swap! game-atom update-game)
  (background 8 8 32)
  (doseq [[k e] @game-atom]
    (draw-entity e))
  (draw-hud (:player @game-atom))
  (reset! previous-time-atom (millis)))

(defn open-window []
  (defsketch example
    :title "Asteroids"
    :setup setup
    :draw draw
    :size [WIDTH HEIGHT]
    :key-pressed key-pressed
    :key-released key-released))


(defn -main [& args]
  (open-window))
