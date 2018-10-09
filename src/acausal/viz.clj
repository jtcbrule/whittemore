(ns acausal.viz
  (:require [clojure.string :as string]
            [rhizome.viz]))


;; TODO: refactor rhizome/graphviz code


(defn transpose
  "Returns the transpose of directed graph g.
  Graphs are assumed to be of the form {nodes #{nodes}}."
  [g]
  (apply merge-with
         into

         ; key -> #{}
         (into {}
               (for [k (keys g)]
                 {k #{}}))

         ; val -> #{key}
         (for [k (keys g)
               v (get g k)]
           {v #{k}})))


(defn format-keyword
  "Returns an html subscripted string, given a keyword with single underscore."
  [kword]
  (let [s (string/split (name kword) #"_")]
    (if (= (count s) 2)
      (str "<" (first s) "<SUB>" (second s) "</SUB>" ">")
      (name kword))))


(defrecord Latent [ch])

(defn latent?
  "Returns true iff node is Latent."
  [node]
  (instance? Latent node))


(defn node->descriptor
  "Graphviz options for nodes."
  [node]
  (if (latent? node)
    {:label "", :shape "none", :width 0, :height 0}
    {:label (if (keyword? node) (name node) (str node))}))


(defn edge->descriptor
  "Graphviz options for edges."
  [i j]
  (if (latent? i)
    {:style "dotted" :arrowhead "empty"}
    {}))


(def model-options 
  [:vertical? true
   :node->descriptor node->descriptor
   :edge->descriptor edge->descriptor])


(defn rhizome-graph
  "Returns a graph to be rendered by rhizome, given model m."
  [m]
  (into (transpose (:pa m))
        (for [multiedge (:bi m)]
          {(->Latent multiedge) multiedge})))


(defn view-model
  "Rhizome visualization of model m."
  [m]
  (let [g (rhizome-graph m)]
    (apply rhizome.viz/view-graph
           (keys g)
           g
           model-options)))


(defn model->svg
  "Returns m as an svg graphic."
  [m]
  (let [g (rhizome-graph m)]
    (apply rhizome.viz/graph->svg
           (keys g)
           g
           model-options)))


(defn hedge->descriptor
  "Graphviz options for hedge nodes."
  [s node]
  (cond
    (latent? node)
    {:label "", :shape "none", :width 0, :height 0}

    (contains? s node)
    {:label (if (keyword? node) (name node) (str node))
     :color "red"}

    :else
    {:label (if (keyword? node) (name node) (str node))}))


(defn hedge-options
  [h]
  [:options {:label "Hedge" :labelloc "t"}
   :vertical? true
   :edge->descriptor edge->descriptor
   :node->descriptor (partial hedge->descriptor (:s h))])


(defn view-hedge
  [h]
  (let [g (rhizome-graph (:g h))]
    (apply rhizome.viz/view-graph
           (keys g)
           g
           (hedge-options h))))


(defn hedge->svg
  [h]
  (let [g (rhizome-graph (:g h))]
    (apply rhizome.viz/graph->svg
           (keys g)
           g
           (hedge-options h))))


