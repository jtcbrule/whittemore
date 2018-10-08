(ns acausal.core-test
  (:require [clojure.test :refer :all]
            [acausal.core :refer :all]))

(deftest trivial-test
  (testing "Example test"
    (is (= 1 1))))


(def dag3
  {:x [:z]
   :y [:x :z]
   :z []})

(def dag3-transpose
  {:x #{:y}
   :y #{}
   :z #{:x :y}})


(deftest transpose-test
  (is (= (transpose dag3)
         dag3-transpose)))


(def m
  (model
    {:w []
     :z [:w :x]
     :y [:z]
     :x []}
    
    #{:x :y}
    #{:w :z}))

