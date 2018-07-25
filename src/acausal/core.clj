(ns acausal.core)
  ;(:require [clojure.string]
  ;          [clojupyter.protocol.mime-convertible :as mc]))


(defn transpose
  "Compute the transpose of directed graph g
   Returns a map of nodes -> set of nodes (adjacency list representation)
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



;; TODO: consider restructuring; lots of redundant data here
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


;; TODO: consider different field names
;; Note that by convention: y is effect, x is do(), w is given

(defrecord Query [effect do given])

(defn query
  "Construct a query
  TODO validation, etc"
  [effect & {:keys [do given] :or {do [] given []}}]
  (Query. effect do given))

;;(def q "Alias for query" query)

;; TODO: rename this info or 'information set' to match causal programming?
;; TODO: definitely rename the params
;; v is the observables, z is the surrogate experiment set

(defrecord Data [vars surrogate])

(defn data
  "Construct a data object"
  [v & {:keys [do*] :or {do* []}}]
  (Data. v do*))

(data [:x :y] :do* [:z_1 :z_2])


;; TODO: this is broken; fix
(defrecord Formula [s])

;; This should be an implementaion of zIDC
(defn identify
  "zIDC algorithm
   By default, assume P(v) as information set"
  ([m q]
   (Formula.
     (str
       "P("
       
       (clojure.string/join ", " (map name (:effect q)))

       " \\mid "

       "do("
       (clojure.string/join ", " (map name (:do q)))
       ")"

       (if (empty? (:given q))
         ""
         (str ", " (clojure.string/join ", " (map name (:given q)))))
       
       ")"
       
       " = "
       
       "\\sum_{z'} P(z' \\mid x) \\sum_{x'} P(y \\mid x', z) P(x') ")))

  ([m q d]
   (identify m q)))


;;; TODO: temporarily put the (repl based) visualization functions here
;;; TODO: create a jupyter namespace
;;; TODO: create a 'live' namespace that can be (use '...) ed?
;;; TODO: d-seperation algorithm
;;; TODO: checkout the zID algorithm to see what kind of graph manipulations
;;;       are needed


;;; examples

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

