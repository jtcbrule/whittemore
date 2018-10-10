(ns acausal.core-test
  (:refer-clojure :exclude [ancestors parents])
  (:require [clojure.test :refer :all]
            [acausal.core :refer :all]))


;; Models where P(y | do(x)) is identifiable

(def ident-a
  (model
    {:y [:x]
     :x []}))

(def ident-b
  (model
    {:x []
     :y [:x :z]
     :z [:x]}
    #{:z :y}))
     
(def ident-c
  (model
    {:x [:z]
     :y [:x :z]
     :z []}
    #{:z :y}))

(def ident-d
  (model
    {:x [:z]
     :y [:x :z]
     :z []}
    #{:x :z}))

(def ident-e
  (model
    {:x []
     :y [:z]
     :z [:x]}
    #{:x :y}))

(def ident-f
  (model
    {:x []
     :z_1 [:x]
     :z_2 [:z_1]
     :y [:x :z_1 :z_2]}
    #{:x :z_2}
    #{:z_1 :y}))

(def ident-g
  (model
    {:x [:z_2]
     :z_1 [:x :z_2]
     :z_2 []
     :z_3 [:z_2]
     :y [:z_1 :z_3]}
    #{:x :z_2}
    #{:x :z_3}
    #{:x :y}
    #{:y :z_2}))


;; Models where P(y | do(x)) is not identifiable

(def non-a
  (model
    {:x []
     :y [:x]}
    #{:x :y}))

(def non-b
  (model
    {:x []
     :z [:x]
     :y [:z]}
    #{:x :z}))

(def non-c
  (model
    {:x []
     :z [:x]
     :y [:z]}
    #{:x :z}))

(def non-d
  (model
    {:x []
     :y [:x :z]
     :z []}
    #{:x :z}
    #{:z :y}))

(def non-e
  (model
    {:x [:z]
     :y [:x]
     :z []}
    #{:x :z}
    #{:z :y}))

(def non-f
  (model
    {:x []
     :z [:x]
     :y [:z]}
    #{:x :y}
    #{:z :y}))

(def non-g
  (model
    {:x []
     :z_1 [:x]
     :z_2 []
     :y [:z_1 :z_2]}
    #{:x :z_2}
    #{:z_1 :z_2}))

(def non-h
  (model
    {:x [:z]
     :z []
     :w [:x]
     :y [:w]}
    #{:x :z}
    #{:x :y}
    #{:z :y}
    #{:z :w}))


(deftest identify-test
  (testing "Identifiable"
    (is (formula? (identify ident-a (q [:y] :do [:x]))))
    (is (formula? (identify ident-b (q [:y] :do [:x]))))
    (is (formula? (identify ident-c (q [:y] :do [:x]))))
    (is (formula? (identify ident-d (q [:y] :do [:x]))))
    (is (formula? (identify ident-e (q [:y] :do [:x]))))
    (is (formula? (identify ident-f (q [:y] :do [:x]))))
    (is (formula? (identify ident-g (q [:y] :do [:x])))))
  (testing "Nonidentifiable"
    (is (not (formula? (identify non-a (q [:y] :do [:x])))))
    (is (not (formula? (identify non-b (q [:y] :do [:x])))))
    (is (not (formula? (identify non-c (q [:y] :do [:x])))))
    (is (not (formula? (identify non-d (q [:y] :do [:x])))))
    (is (not (formula? (identify non-e (q [:y] :do [:x])))))
    (is (not (formula? (identify non-f (q [:y] :do [:x])))))
    (is (not (formula? (identify non-g (q [:y] :do [:x])))))
    (is (not (formula? (identify non-h (q [:y] :do [:x])))))))


(def wainer
  (model
    {:z_0 []
     :b [:z_0]
     :z_1 [:z_0]
     :x [:z_0]
     :z_2 [:z_1 :x]
     :z_3 [:z_2 :b]
     :y [:x :z_2 :z_3]}))


(def wainer-latent
  (model
    {:z_1 []
     :x []
     :z_2 [:z_1 :x]
     :z_3 [:z_2]
     :y [:x :z_2 :z_3]}
    #{:x :z_1 :z_3}))


(deftest latent-projection-test
  (is
    (= (latent-projection wainer #{:z_0 :b})
       wainer-latent)))

(deftest wainer-identification
  (is (formula? (identify wainer-latent (q [:y] :do [:x])))))


(def hedge-less
  (model
    {:w_1 []
     :x [:w_1]
     :y_1 [:x]
     :w_2 []
     :y_2 [:w_2]}
    #{:w_1 :y_1}
    #{:w_1 :w_2}
    #{:w_2 :x}
    #{:w_1 :y_2}))


(def napkin
  (model
    {:z_1 [:z_2]
     :x [:z_1]
     :y [:x]
     :z_2 []}
    #{:x :z_2}
    #{:z_2 :y}))


(deftest hedge-less-multiple
  (is (formula? (identify hedge-less
                          (q [:y_1 :y_2] :do [:x])))))


(deftest napkin-id
  (is (formula? (identify napkin (q [:y] :do [:x])))))

