(ns acausal.viz-test
  (:require [clojure.test :refer :all]
            [acausal.viz :refer :all]))

(def dag
  {:x #{:z}
   :y #{:x :z}
   :z #{}})

(def dag-transpose
  {:x #{:y}
   :y #{}
   :z #{:x :y}})

(deftest transpose-test
  (is (= (transpose dag)
         dag-transpose)))

