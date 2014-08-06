(ns repl-asteroids.core-test
  (:require [clojure.test :refer :all]
            [repl-asteroids.core :refer :all]))


(testing "promote-children, empty"
  (is (= (promote-children ())
         ())))

(testing "promote-children, no children"
  (is (= (promote-children '({ :a 1 }))
         '({ :a 1 }))))

(testing "promote-children, with children"
  (is (= (promote-children '({ :a 1 :children [{ :b 2 } { :c 3}] }))
         '({ :a 1 } { :b 2 } { :c 3 }))))
