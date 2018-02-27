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
    
    [:x :y]
    [:w :z]))


(deftest model-test
    (is (= (:vars m)
           #{:w :x :y :z}))

    (is (= (:latents m)
        #{#{:x :y} #{:w :z}}))

    (is (= (:parents m)
           {:w #{#{:w :z}}
            :x #{#{:x :y}}
            :y #{:z #{:x :y}}
            :z #{:w :x #{:w :z}}
            #{:w :z} #{}
            #{:x :y} #{}}))

    (is (= (:children m)
           {:w #{:z}
            :x #{:z}
            :y #{}
            :z #{:y}
            #{:w :z} #{:w :z}
            #{:x :y} #{:x :y}})))

