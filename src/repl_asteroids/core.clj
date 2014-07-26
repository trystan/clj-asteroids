(ns repl-asteroids.core
  (:require [quil.core :refer :all]
            [repl-asteroids.quil-helpers :refer :all]))

(def window-width 800)
(def window-height 500)

(defn new-ship []
  { :type :ship
    :x (/ window-width 2)  :vx 0
    :y (/ window-height 2) :vy 0
    :angle 0               :vangle 0
    :image "ship.png"    :width 49    :height 16
    :thrust 0
    :max-speed 5})

(defn new-asteroid []
  { :type :asteroid
    :x (rand-int window-width)  :vx (- (rand) 0.5)
    :y (rand-int window-height) :vy (- (rand) 0.5)
    :angle (rand (* 2 Math/PI)) :vangle (* (- (rand) 0.5) 0.2)
    :image "asteroid.png" :width 64    :height 64
    :max-speed 5})

(defn new-bullet [ship]
  (let [front-x (+ (:x ship) (* 8 (Math/cos (:angle ship))))
        front-y (+ (:y ship) (* 8 (Math/sin (:angle ship))))]
    { :type :bullet
      :x front-x :vx (+ (:vx ship) (* 10 (Math/cos (:angle ship))))
      :y front-y :vy (+ (:vy ship) (* 10 (Math/sin (:angle ship))))
      :angle (:angle ship) :vangle 0
      :image "shot.png" :width 8    :height 8
      :max-speed 10
      :ttl 50 }))


(def game-state-atom (atom { :ship (new-ship)
                             :asteroids [(new-asteroid) (new-asteroid) (new-asteroid)]
                             :bullets []}))

(defn setup []
  (smooth)
  (frame-rate 30)
  (image-mode :center))


;; collisions
(defn collides? [a b]
  (let [dx (- (:x a) (:x b))
        dy (- (:y a) (:y b))
        distance-squared (+ (* dx dx) (* dy dy))
        radius-squared (* (:height a) (:height b))]
    (< distance-squared radius-squared )))

(defmulti collide (fn [a b] [(:type a) (:type b)]))

(defmethod collide [:bullet :asteroid] [bullet asteroid]
  (assoc bullet :ttl -1))

(defmethod collide :default [a b]
  a)

(defn collide-two [a b]
  (if (collides? a b)
    (collide a b)
    a))

(defn collide-one-vs-group [a bs]
  (if (empty? bs)
    a
    (recur (collide-two a (first bs)) (rest bs))))

(defn collide-groups [as bs]
  (map (fn [a] (collide-one-vs-group a bs)) as))


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

(defn inertia [thing]
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
    (update-in thing [:ttl] dec)
    thing))

(defn update-thing [thing]
  (-> thing
      (clamp-values)
      (forward)
      (inertia)
      (wrap-around)
      (update-ttl)))

(defn is-dead [thing]
  (or (nil? (:ttl thing)) (< 0 (:ttl thing))))

(defn remove-dead [things]
  (filter is-dead things))

(defn update-things [things]
  (remove-dead (map update-thing things)))

(defn update [state]
  (-> state
    (update-in [:asteroids] update-things)
    (update-in [:bullets] update-things)
    (update-in [:ship] update-thing)
    (update-in [:bullets] (fn [bs] (collide-groups bs (:asteroids state))))))


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



;; controlls
(defn assoc-in-ship [k v]
  (swap! game-state-atom (fn [state]
                           (assoc-in state [:ship k] v))))

(defn add-bullet []
  (swap! game-state-atom (fn [state]
                           (assoc state :bullets (conj (:bullets state) (new-bullet (:ship state)))))))


(defn key-pressed []
  (case (key-as-keyword)
        :left    (assoc-in-ship :vangle -0.1)
        :right   (assoc-in-ship :vangle  0.1)
        :up      (assoc-in-ship :thrust  0.25)
        :f       (add-bullet)
        nil))

(defn key-released []
  (case (key-as-keyword)
        :left    (assoc-in-ship :vangle  0)
        :right   (assoc-in-ship :vangle  0)
        :up      (assoc-in-ship :thrust  0)
        nil))

(defn main []
  (defsketch demo
    :title "Asteroids"
    :size [window-width window-height]
    :setup setup
    :draw draw
    :key-pressed key-pressed
    :key-released key-released))

;(main)
