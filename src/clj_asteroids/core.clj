(ns clj-asteroids.core
  (:require [quil.core :refer :all]
            [clj-asteroids.quil-helpers :refer :all]))

(def window-width 800)

(def window-height 500)

(defn new-ship []
  { :type :ship
    :x (/ window-width 2)
    :y (/ window-height 2)
    :angle 0
    :vx 0
    :vy 0
    :vangle 0
    :image "ship.png"
    :width 49
    :height 16
    :thrust 0
    :max-speed 5 })

(defn new-bullet [ship]
  (let [front-x (+ (:x ship) (* 8 (Math/cos (:angle ship))))
        front-y (+ (:y ship) (* 8 (Math/sin (:angle ship))))]
    { :type :bullet
      :x front-x
      :y front-y
      :angle (:angle ship)
      :vx (+ (:vx ship) (* 10 (Math/cos (:angle ship))))
      :vy (+ (:vy ship) (* 10 (Math/sin (:angle ship))))
      :vangle 0
      :image "shot.png"
      :width 8
      :height 8
      :max-speed 10
      :ttl 50 }))

(defn new-asteroid
  ([]
   { :type :asteroid
     :x (rand-int window-width)
     :y (rand-int window-height)
     :angle (rand (* 2 Math/PI))
     :vx (- (rand) 0.5)
     :vy (- (rand) 0.5)
     :vangle (* (- (rand) 0.5) 0.2)
     :image "asteroid.png"
     :width 64
     :height 64
     :max-speed 5})
  ([parent]
   { :type :asteroid
     :x (+ (:x parent) (* (- (rand) 0.5)) 5)
     :y (+ (:y parent) (* (- (rand) 0.5)) 5)
     :angle (rand (* 2 Math/PI))
     :vx (+ (:vx parent) (- (rand) 0.5))
     :vy (+ (:vy parent) (- (rand) 0.5))
     :vangle (+ (:vy parent) (* (- (rand) 0.5) 0.2))
     :image "asteroid.png"
     :width (* (:width parent) 0.66)
     :height (* (:height parent) 0.66)
     :max-speed 5}))

(def game-state-atom (atom [(new-ship)
                            (new-asteroid)
                            (new-asteroid)
                            (new-asteroid)]))



;; collisions
(defn collides? [a b]
  (let [dx (- (:x a) (:x b))
        dy (- (:y a) (:y b))
        distance-squared (+ (* dx dx) (* dy dy))
        radius-squared (* (:height a) (:height b))]
    (< distance-squared radius-squared)))


(defmulti collide (fn [a b] [(:type a) (:type b)]))

(defmethod collide [:bullet :asteroid] [bullet asteroid]
  (assoc bullet :ttl -1))

(defmethod collide [:asteroid :bullet] [asteroid bullet]
  (if (< (:width asteroid) 16)
    (assoc asteroid :ttl -1)
    (assoc asteroid :ttl -1
                    :children [(new-asteroid asteroid) (new-asteroid asteroid)])))

(defmethod collide :default [a b]
  a)


(defn collide-with-1 [a b]
  (if (collides? a b)
    (collide a b)
    a))

(defn collide-with [a bs]
  (reduce collide-with-1 a bs))



;; updating
(defn clamp [x min max]
  (cond
   (< x min)    min
   (> x max)    max
   :else        x))

(defn clamp-values [thing]
  (assoc thing :vx (clamp (:vx thing) (- 0 (:max-speed thing)) (:max-speed thing))
               :vy (clamp (:vy thing) (- 0 (:max-speed thing)) (:max-speed thing))
               :vangle (clamp (:vangle thing) -1 1)))

(defn forward [thing]
  (if (:thrust thing)
    (assoc thing :vx (+ (:vx thing) (* (:thrust thing) (Math/cos (:angle thing))))
                 :vy (+ (:vy thing) (* (:thrust thing) (Math/sin (:angle thing)))))
    thing))

(defn move [thing]
  (assoc thing :x (+ (:x thing) (:vx thing))
               :y (+ (:y thing) (:vy thing))
               :angle (+ (:angle thing) (:vangle thing))))

(defn wrap [x max]
  (cond
   (< x 0)   (+ max x)
   (> x max) (- x max)
   :else     x))

(defn wrap-around [thing]
  (assoc thing :x (wrap (:x thing) window-width)
               :y (wrap (:y thing) window-height)))

(defn update-ttl [thing]
  (if (:ttl thing)
    (assoc thing :ttl (dec (:ttl thing)))
    thing))

(defn update-thing [thing things]
  (-> (collide-with thing things)
      (clamp-values)
      (forward)
      (move)
      (wrap-around)
      (update-ttl)))

;; or for those who like nesting...
;(defn update-thing [thing things]
;  (update-ttl (wrap-around (move (forward (clamp-values (collide-with thing things)))))))

;; or for those who like point-free style...
;(def update-thing
;  (comp update-ttl wrap-around move forward clamp-values collide-with))

(defn alive? [thing]
  (or (nil? (:ttl thing)) (< 0 (:ttl thing))))

(defn promote-children [things]
  (let [children (flatten (remove nil? (map :children things)))
        without-children (map (fn [thing] (dissoc thing :children)) things)]
    (concat without-children children)))

(defn update [state]
  (filter alive? (promote-children (map update-thing state (repeat state)))))



;;drawing
(defn draw-thing [thing]
  (with-translation [(:x thing) (:y thing)]
    (with-rotation [(:angle thing)]
      (image (get-image (:image thing)) 0 0 (:width thing) (:height thing)))))

(defn draw []
  (swap! game-state-atom update)
  (background 4 4 16)
  (doseq [thing @game-state-atom]
    (draw-thing thing))
  (text (str (int (current-frame-rate)) " fps") 10 20))



;; ugly....
(defn assoc-in-ship [k v]
  (swap! game-state-atom (fn [state]
                           (map (fn [thing]
                                  (if (= :ship (:type thing))
                                    (assoc thing k v)
                                    thing)) state))))

;; also ugly....
(defn add-bullet []
  (swap! game-state-atom (fn [state]
                           (if-let [ship (first (filter (fn [thing] (= :ship (:type thing))) state))]
                             (conj state (new-bullet ship))
                             state))))



;; controlls
(defn key-pressed []
  (case (key-as-keyword)
        :left    (assoc-in-ship :vangle -0.1)
        :right   (assoc-in-ship :vangle  0.1)
        :up      (assoc-in-ship :thrust  0.25)
        :f       (add-bullet)
        :p       (println @game-state-atom)
        nil))

(defn key-released []
  (case (key-as-keyword)
        :left    (assoc-in-ship :vangle  0)
        :right   (assoc-in-ship :vangle  0)
        :up      (assoc-in-ship :thrust  0)
        nil))

(defn setup []
  (smooth)
  (frame-rate 30)
  (image-mode :center))

(defn main []
  (defsketch demo
    :title "Asteroids"
    :size [window-width window-height]
    :setup setup
    :draw draw
    :key-pressed key-pressed
    :key-released key-released))

;(main)
