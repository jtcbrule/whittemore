(ns acausal.core
  (:require [rhizome.viz]))

(defn transpose
  "Compute the transpose of directed graph g (adjacency list representation)
   Returns map of nodes -> set of nodes"
  [g]
  (apply merge-with
         into
         ; each key -> #{}
         (into {}
               (for [k (keys g)]
                 {k #{}}))
         ; each val -> #{key}
         (for [k (keys g)
               v (get g k)]
           {v #{k}})))


;; Models are stored as map of vars to set of parents
;; TODO: consider alternative representations
(defrecord Model [pa])

;; A Latent is a 'wrapper' around set of children to act as a latent node
(defrecord Latent [ch])

(defn latent?
  "True iff node is a Latent node"
  [node]
  (= Latent (type node)))


;; TODO: consider alternative representations; validate latents
;; Consider doing full Verma-style latent projections, i.e. convert to only
;; pairs of latent variables
(defn model
  "Construct a model"
  [dag & confounded]
  (let [latents (map set confounded)
        children (apply merge-with
                        into
                        (transpose dag)
                        (for [s latents]
                          {(Latent. s) s}))
        parents (transpose children)]
    (Model. parents)))


(defn node->descriptor
  "Graphviz options for node"
  [n]
  (if (latent? n)
    {:label "", :shape "none", :width 0, :height 0}
    {:label (if (keyword? n) (name n) (str n))}))
   ; :shape "circle"


(defn edge->descriptor
  "Graphviz options for edge"
  [i j]
  (if (latent? i)
    {:style "dotted"}
    {}))


(defn view-model
  "View model m"
  [m]
  (let [children (transpose (:pa m))]
    (rhizome.viz/view-graph
      (keys children)
      children
      :vertical? false
      :node->descriptor node->descriptor
      :edge->descriptor edge->descriptor)))


;; TODO: consider renaming
(defrecord Data [vars surrogate])

;; TODO: validate
(defn data
  "Construct a data object"
  [v & {:keys [do*] :or {do* []}}]
  (Data. (set v) (set do*)))


;; TODO: consider different field names
;; TODO: consider supporting conditional queries, :given
;; alias (q ..) to query?
;; Note that by convention: y is effect, x is do(), w is given
(defrecord Query [effect do])

(defn query
  "Construct a query
  TODO validation, etc"
  [effect & {:keys [do] :or {do []}}]
  (Query. (set effect) (set do)))


;; TODO: determine structure of Formula
;; name field(s)?
(defrecord Formula [f])


;; This should be an implementaion of zID(C)
(defn identify
  "zID(C) algorithm
   By default, assume P(v) as data"
  ([m q d]
   nil)
  ([m q]
   nil))


;;; example models

(def m1
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

