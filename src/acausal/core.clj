(ns acausal.core
  (:refer-clojure :exclude [ancestors parents])
  (:require [rhizome.viz]
            [clojure.set :refer [union subset? intersection difference]]))


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
  (instance? Latent node))


;; TODO: consider alternative representations; validate latents
;; Consider doing full Verma-style latent projections, i.e. convert to only
;; pairs of latent variables
;; alt: check all confounding sets for being subsets of another set
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
;; Needs :imap argument
;; probably needs a way to define support of random variables
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


(defn parents
  "Return the (non-latent) parents of the collection of nodes x in model g"
  [g x]
  (let [all-parents (apply union
                           (for [node x]
                             (get (:pa g) node)))]
    (set (filter #(not (latent? %)) all-parents))))


(defn ancestors
  "Return (non-latent) ancestors of x in model g, inclusive
  
  x is a collection of nodes in g; g is a causal model"
  [g x]
  (loop [frontier (set x)
         visited #{}]
  (if (empty? frontier)
    visited
    (recur (parents g frontier)
           (union visited frontier)))))


(defn cut-latent
  "Helper function; given latent node l and a set x, return a new latent
   where children in x are absent. If this results in less than 2 children,
   return nil"
  [l x]
  (let [new-children (difference (:ch l) x)]
    (if (< (count new-children) 2)
      nil
      (Latent. new-children))))


;;; Okay, we have a problem with the latents, what if we end up with singletons?

(defn raw-cut
  "Helper function; given a dag and set x, return a dag with every node in x
   having no parents"
  [dag x]
  (into {}
        (for [[k v] dag]
          (if (contains? x k)
            [k #{}]
            [k v]))))


;; BORK! If cut-latent returns nil, then don't include it?
;; This definetly needs to be reviewed
(defn fix-latents
  "Helper function; given a dag in 'child' format, fix latents"
  [dag x]
  (into {}
        (for [[k v] dag
              :when (or
                      (and (latent? k) (cut-latent k x))
                      (not (latent? k)))]
          (if (latent? k)
            (let [new-k (cut-latent k x)]
              [new-k (:ch new-k)])
            [k v]))))



;; TODO: write
(defn cut-incoming
  "G_{\\overline{x}}
  
  Return a model where all incoming edges to nodes x have been severed in g"
  [m x]

  (let [new-children (transpose (raw-cut (:pa m) x))
        new-parents (transpose (fix-latents new-children x))]
    (Model. new-parents)))


(view-model (cut-incoming m1 #{:z}))

(def tmp #{:z :y})


(comment

(for [[k v] (:pa m1)
      :when (latent? k)]
  k)


(:pa m1)

(transpose
(into {}

(for [[k v] (:pa m1)
      :when (not (latent? k))]
  (cond
    (contains? tmp k) [k #{}]
    :else [k v]))

)
)


(transpose
(into {}

(for [[k v] (:pa m1)]
  (cond
    (contains? tmp k) [k #{}]
    :else [k v]))

)
)





)

;; TODO: confounded components

;; TODO: some kind of 'subgraph' funtion (graph, projected down to these vars?)

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

