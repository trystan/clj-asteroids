(ns clj-asteroids.core-test
  (:require [clj-asteroids.core :refer :all]))

(defmacro expect [exp]
  (let [parts (for [e exp] (if (= e (first exp)) e (eval e)))]
    `(if ~exp
       true
       (throw (Exception. (str "Failed!\n"
                               "Expected      " '~exp "\n"
                               "But got  (not " '~parts ")"))))))

(expect (= (map-values inc {1 1 2 2 3 3})       { 1 2 2 3 3 4 }))

(expect (= (filter-values even? {1 1 2 2 3 3})  { 2 2 }))

(expect (= (safe-update-in { :z 1 } [:z] inc)   { :z 2 }))

(expect (= (safe-update-in { :z 1 } [:x] inc)   { :z 1 }))
