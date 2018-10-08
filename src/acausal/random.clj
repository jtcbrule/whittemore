(ns acausal.random
  (:require [acausal.core :as a
             :refer [verticies transpose pairs-of
                     model q identifiable?
                     view-model]]))


(defn gen-nodes
  "Generate n keywords, :n_i from i=1 to n"
  [n]
  (vec
    (for [i (range n)]
      (keyword (str "n_" (inc i))))))


;; TODO: make more efficient?
(defn erdos-renyi-dag
  "Erdos-renyi model for directed acyclic graphs."
  [n p]
  (loop [g {}, nodes (gen-nodes n)]
    (if (empty? nodes)
      g
      (recur (assoc g (first nodes) (set (random-sample p (rest nodes))))
             (rest nodes)))))


(defn erdos-renyi-model
  "Produce a random model (dag) with n nodes and probability p of edge"
  [n p]
  (model (transpose (erdos-renyi-dag n p))))


(defn josh-model
  "n nodes, p probability of edge, q probability of confounding"
  [n p q]
  (let [confounding (random-sample q (pairs-of (gen-nodes n)))]
    (apply model (transpose (erdos-renyi-dag n p)) confounding)))


;; TODO: fix hack
(defn nontrivial-singleton-queries
  [nodes]
  (for [i nodes
        j nodes
        :when (< (compare i j) 0)]
    (q [i] :do [j])))


(defn percent-nontrivial-id
  [m]
  (let [queries (nontrivial-singleton-queries (verticies m))]
    (/
     (->>
       queries
       (map #(identifiable? m %))
       (filter identity)
       count)
     (count queries))))


(defn er-num [num-vars]
  "Erdos Renyi number"
    (/ (java.lang.Math/log num-vars) num-vars))


(defn mean
  [coll]
  (/ (reduce + coll) (count coll)))


(defn gen-josh-model!
  [num-vars p q]
  #(josh-model num-vars p q))


(comment

(map
  #(->>
     (gen-josh-model! 10 (er-num 10) %)
     repeatedly
     (map (fn [m] (identifiable? m (q [:n_9] :do [:n_1]))))
     (take 1000)
     (filter identity)
     count)
  (range 0 1 0.1))


(nontrivial-singleton-queries (verticies (josh-model 10 (* 2 (er-num 10)) 0.9)))


)


(def a-run
  (map
    #(mean
      (take 1000
            (map percent-nontrivial-id
                 (repeatedly (fn [] (josh-model 10 (* 2 (er-num 10)) %))))))
    (range 0 1 0.05)))


(comment

(incanter.core/view
  (incanter.charts/scatter-plot
    (range 0 1 0.05)
    a-run))

)

(comment

(let [n 10]
  (view-model (erdos-renyi-model n (/ (java.lang.Math/log 10) 10))))

(let [n 10]
  (view-model (erdos-renyi-model n (/ 1 n))))

)


;; need incanter.charts/scatter-plot
;; (-> thechart (.createBufferedImage width height))
;; https://github.com/aria42/clojupyter/blob/mime-improvements/examples/incanter-demo.ipynb

