(ns clj-asteroids.core-test
  (:require [clojure.test :refer :all]
            [clj-asteroids.core :refer :all]))


(testing "alive?, ttl > 0"
  (is (alive? { :ttl 1 })))

(testing "alive?, ttl <= 0"
  (is (not (alive? { :ttl 0 }))))

(testing "alive?, no ttl"
  (is (alive? { :ttlmnop 0 })))



(testing "promote-children, empty"
  (is (= (promote-children ())
         ())))

(testing "promote-children, no children"
  (is (= (promote-children '({ :a 1 }))
         '({ :a 1 }))))

(testing "promote-children, with children"
  (is (= (promote-children '({ :a 1 :children [{ :b 2 } { :c 3}] }))
         '({ :a 1 } { :b 2 } { :c 3 }))))
