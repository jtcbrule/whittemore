(ns acausal.viz
  (:require [clojure.string :as string]
            [rhizome.viz]
            [dorothy.core :as dot]))


;; TODO: dynamic selection of rendering function, objection viz.cljc
(defn dot->svg
  "Render dot spec as svg."
  [dot-str]
  (dot/render dot-str {:format :svg}))


;; DEPRECATED
(defn dot->png
  "Render dot spec as png."
  [dot-str]
  (dot/render dot-str {:format :png}))


;; DEPRECATED
;; use dot2tex , -tmath
(defn format-keyword
  "Returns an html subscripted string, given a keyword with single underscore."
  [kword]
  (let [s (string/split (name kword) #"_")]
    (if (= (count s) 2)
      (str "<FONT>" (first s) "<SUB>" (second s) "</SUB>" "</FONT>")
      (name kword))))


(def digraph-options
  {:rankdir "LR"})

(def bi-options
  {:style "dashed", :dir "both", :arrowhead "empty", :arrowtail "empty",
   :constraint "true"})

(defn model->dot
  "Convert a model to a dot spec."
  [m]
  (dot/dot
    (dot/digraph
      (concat
        [digraph-options]
        ; nodes (unused, but can add node options)
        (for [n (keys (:pa m))]
          [n])
        ; edges from parents to children
        (for [[ch pa-set] (:pa m)
              pa pa-set]
          [pa ch])
        ; bidirectional edges
        (for [pair (:bi m)
              :let [pair (sort pair)]]
          [(first pair) (second pair) bi-options])))))


(defn view-model
  "View a model.
  
  Requires dot to be on your PATH, i.e. install graphviz."
  [m]
  (-> m model->dot dot/show!))


(def model->svg
  "Convert a model as an svg string."
  (comp dot->svg model->dot))




(comment

(require '[acausal.core :refer [model]])

(def napkin
  (model
    {:z_1 [:z_2]
     :x [:z_1]
     :y [:x]
     :z_2 []}
    #{:x :z_2}
    #{:z_2 :y}))

(def wainer
  (model
    {:z_0 []
     :b [:z_0]
     :z_1 [:z_0]
     :x [:z_0]
     :z_2 [:z_1 :x]
     :z_3 [:z_2 :b]
     :y [:x :z_2 :z_3]}))

; from acausal.core
(extend-protocol mc/PMimeConvertible
  Model
  (to-mime [this]
    (mc/stream-to-string
      {:image/png (-> this viz/model->dot viz/dot->png)})))

)


(comment

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


(defn format-keyword
  "Returns an html subscripted string, given a keyword with single underscore."
  [kword]
  (let [s (string/split (name kword) #"_")]
    (if (= (count s) 2)
      (str "<" (first s) "<SUB>" (second s) "</SUB>" ">")
      (name kword))))

)

