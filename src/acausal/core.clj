(ns acausal.core
  (:require [rhizome.viz]
            [clojupyter.protocol.mime-convertible :as mc]))


(defn transpose
  "Compute the transpose of directed graph g
   Returns a map of sets (adjacency list representation)
   TODO: make more efficient"
  [g]
  (apply merge-with
         into
         (into {}
               (for [k (keys g)]
                 {k #{}}))
         (for [k (keys g)
               v (get g k)]
           {v #{k}})))


(defrecord Model [vars latents parents children])


(defn model
  "Construct a model
   TODO: validate latents"
  [m & more]
  (let [vars (set (keys m))

        latents (set (map set more))
              

        children (apply merge-with
                        into
                        (transpose m)
                        (for [coll latents]
                          {coll coll}))

        parents (transpose children)]

    (Model. vars latents parents children)))



(defmacro defmodel
  "TODO: finish, fix"
  [name docstring m & more]
  `(def ~name
     ~docstring
     (model
       ~m
       ~@more)))



(def m
  (model
    {:w []
     :z [:w :x]
     :y [:z]
     :x []}
    [:x :y]
    [:w :z]))


(def m2
  (model
    {:w []
     :z [:w :x]
     :y [:z]
     :x []}
    [:w :y :z]))



(defn view-model
  "View model m
  TODO: refactor dot and rendering steps, better label formatting"
  [m]
  (rhizome.viz/view-graph
    (keys (:children m))
    (:children m)
    :vertical? false
    :node->descriptor (fn [n]
                        (if (contains? (:latents m) n)
                          {:label "", :shape "none", :width 0, :height 0}
                          {:label (name n), :shape "circle"}))
    :edge->descriptor (fn [i j]
                        (if (contains? (:latents m) i)
                          {:style "dotted"}
                          {}))))

(deftype SVG [s]

  mc/PMimeConvertible
  (to-mime [_]
    (mc/stream-to-string
      {:image/svg+xml s})))



(defn model->svg
  "Model m as SVG
  TODO: refactor dot and rendering steps, better label formatting"
  [m]
  (SVG.
    (rhizome.viz/graph->svg
      (keys (:children m))
      (:children m)
      :vertical? false
      :node->descriptor (fn [n]
                          (if (contains? (:latents m) n)
                            {:label "", :shape "none", :width 0, :height 0}
                            {:label (name n), :shape "circle"}))
      :edge->descriptor (fn [i j]
                          (if (contains? (:latents m) i)
                            {:style "dotted"}
                            {})))))



