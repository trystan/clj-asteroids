(ns repl-asteroids.quil-helpers
  (:require [quil.core :refer :all]))


;; image cahce
(defonce preloaded-images-atom (atom {}))

(defn preload-image [path]
  (swap! preloaded-images-atom #(assoc % path (load-image path))))

(defn get-image [path]
  (when (nil? (get path @preloaded-images-atom))
    (preload-image path))
  (get @preloaded-images-atom path))
