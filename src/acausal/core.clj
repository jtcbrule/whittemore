(ns acausal.core
  (:refer-clojure :exclude [ancestors parents])
  (:require [rhizome.viz]
            [clojure.set :refer [union subset? intersection difference]]
            [clojure.string :as string]
            [clojupyter.protocol.mime-convertible :as mc]))


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
;; TODO: consider alternative representations,
;; specifically, consider not keeping latents in the parent sets of observable
;; nodes. This is redundant information.
(defrecord Model [pa])

;; A Latent is a 'wrapper' around set of children to act as a latent node
(defrecord Latent [ch])

(defn latent?
  "True iff node is a Latent node"
  [node]
  (instance? acausal.core.Latent node))


;; TODO: validate latents
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


(defn model->svg
  "Render model m as svg graphic"
  [m]
   (let [children (transpose (:pa m))]
     (rhizome.viz/graph->svg
       (keys children)
       children
       :vertical? false
       :node->descriptor node->descriptor
       :edge->descriptor edge->descriptor)))



;; TODO: Needs :imap argument
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
  "Construct a query"
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

(defn verticies
  "Return verticies of model m as a set"
  [m]
  (into #{} (filter #(not (latent? %)) (keys (:pa m)))))


(defn cut-latent
  "Helper function; given latent node l and a set x, return a new latent
   where children in x are absent. If this results in less than 2 children,
   return nil"
  [l x]
  (let [new-children (difference (:ch l) x)]
    (if (< (count new-children) 2)
      nil
      (Latent. new-children))))


;; TODO: make more efficient
(defn raw-cut
  "Helper function; given a dag and set x, return a dag with every node in x
   having no parents"
  [dag x]
  (into {}
        (for [[k v] dag]
          (if (contains? x k)
            [k #{}]
            [k v]))))


;; TODO: review
(defn fix-latents
  "Helper function; given a dag in 'child' format, fix latents
   Specifically, cut every latent with x, and remove those that have less than
   two children, reassmble new dag in 'child' format"
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


;; TODO: review, test more thoroughly
(defn cut-incoming
  "G_{\\overline{x}}
  
  Return a model where all incoming edges to nodes x have been severed in g"
  [m x]

  (let [new-children (transpose (raw-cut (:pa m) x))
        new-parents (transpose (fix-latents new-children x))]
    (Model. new-parents)))


;; TODO: needs considerable cleanup, testing;
;; should be a lot more efficient after restructuring the model design
(defn subgraph
  "G_{X}
  
  Return a model containing all verticies in (set) X and edges between those
  verticies (including the bidirected/latents)"
  [m x]
  (let [nodes (into #{} (filter #(not (latent? %)) (keys (:pa m)))) ;; cleanup?
        filtered-pa (into {} (filter #(contains? x (first %)) (:pa m)))
        fixed-latents (fix-latents (transpose filtered-pa) (difference nodes x))
        filtered-ch (into {} (filter #(or (latent? (first %))
                                          (contains? x (first %)))
                                     fixed-latents))]
    (Model. (transpose filtered-ch))))


;; TODO: restructure Model to have :pa, :latent... call it :bi ?
(defn latents
  "Helper function, return set of latents of m"
  [m]
  (into #{}
        (for [[k v] (:pa m)
              :when (latent? k)]
          (:ch k))))


;; TODO: more testing, cleanup, make more efficient; rename?
;; note that we redudantly add the searched node back to the visited set
(defn connected-component
  "Helper function... Assumes edges are set of set of multiedges, n is node"
  [edges node]
  (loop [frontier (list node)
         visited #{}]
    (if (empty? frontier)
      visited
      (let [current (peek frontier)]
        (if (contains? visited current)
          (recur (pop frontier) visited)
          (let [adjacent-set (apply union (filter #(contains? % current) edges))]
            (recur (into (pop frontier) adjacent-set) (conj visited current))))))))


;; TODO: test more thoroughly, cleanup
(defn c-components
  "Return confounded components of m as set of sets of verticies"
  [m]
  (loop [nodes (into #{} (filter #(not (latent? %)) (keys (:pa m)))) ;; cleanup?
         components #{}]
    (if (empty? nodes)
      components
      (let [current-node (first nodes)
            current-component (connected-component (latents m) current-node)]
        (recur (difference nodes current-component)
               (conj components current-component))))))


;;; TODO: cleanup/restructure
(defn node->latex
  [n]
  (if (keyword? n)
    (name n)
    (str n)))


;; TODO: cleanup/restucture
(defn marginalize
  "Temporary representation of \\sum_{sub} p"
  [p sub]
  (conj p
        (str "\\sum_{"
               (string/join "," (map node->latex sub))
               "}")))


;; TODO: cleanup/restucture
(defn product
  "Temporary representation of \\sum_{sub} p"
  [p exprs]
  (conj p
        exprs
        (str "\\prod_{i}")))


; TODO: write/design
(defn topological-sort
  "???"
  []
  nil)


;; helper function
(defn find-superset
  "Given a collection of sets, return a set that is a superset of s,
  or nil if no such superset exists"
  [coll s]
  (first (filter #(subset? s %) coll)))


;; TODO: considerable work
(defn id
  "y set
   x set
   p ??? vector, for now (initially call with ['P(v)']?)
   g model"
  [y x p g]
  (let [v (into #{} (filter #(not (latent? %)) (keys (:pa g))))]
    (cond
      ;; line 1
      (empty? x)
      (marginalize p (difference v y))

      ;; line 2
      (not (empty? (difference v (ancestors g y))))
      (id y
          (intersection x (ancestors g y))
          (marginalize p (difference v (ancestors g y)))
          (subgraph g (ancestors g y)))

      ;; line 3
      :else
      (let [w (difference (difference v x) (ancestors (cut-incoming g x) y))]
        (if (not (empty? w))
          (id y (union x w) p g)
          
          ;; line 4
          (let [cg-x (c-components (subgraph g (difference v x)))]
            (if (> (count cg-x) 1)
              (marginalize (product p
                                    (vec
                                      (for [si cg-x]
                                        (id si
                                            (difference v si)
                                            p
                                            g))))
                           (difference v (union y x)))

              ;; line 5 (cgx should be singleton)
              (let [s (first cg-x)
                    cg (c-components g)]
                (if (= (first cg) (verticies g))
                  ["fail" g cg-x] ;; restructure?

                  ;; line 6
                  (if (contains? cg s)
                    (marginalize (product p
                                          ["P(v_i \\mid v_pi)"]) ;; fix!
                                 (difference s y))

                    ;; line 7;
                    (if-let [s-prime (find-superset cg s)]
                      (id y
                          (intersection x s-prime)
                          (conj p "new P=P(v \\mid ...)")
                          (subgraph g s-prime))

                      ;; fall-through
                      (throw (Error. "Should be unreachable")))))))))))))
                      

  


(def kidney
  (model 
    {:recovery [:treatment :size]
     :size []
     :treatment [:size]}))





(comment


(view-model m2)

(difference #{:recovery :size :treatment} #{:treatment})

(cut-incoming kidney #{:treatment})

(ancestors (cut-incoming kidney #{:treatment}) #{:recovery})

(id #{:recovery} #{:treatment} ["P(v)"] kidney)

(id #{:recovery} #{} [] kidney)

)

;; TODO: Restructure; should be (or call) an implementation of zID(C?)
(defn identify-template
  "zID(C) algorithm
   By default, assume P(v) as data"
  ([m q d]
   (throw (Error. "Not implemented")))
  ([m q]
   nil))


;;; Jupyter integration (TODO: seperate this out later?)

(extend-protocol mc/PMimeConvertible
  Model
  (to-mime [this]
    (mc/stream-to-string
      {:image/svg+xml (model->svg this)})))

(comment

(extend-protocol mc/PMimeConvertible
  Formula
  (to-mime [this]
    (mc/stream-to-string
      {:text/latex (str "$" (:s this) "$")})))

)

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


(def m3
  (model
         {:x []
          :z [:x]
          :y [:z]}
         #{:x :z :y}))

(def m4
  (model
         {:v []
          :w [:v]
          :z [:w :x]
          :y [:z]
          :x []}
         #{:w :z}
         #{:z :y :x}))


(comment

(def example #{#{:a :b :c} #{:c :d} #{:g :h} #{:d :f} #{1 2} #{3 4} #{2 5} #{2 9} #{9 10 11}})

(disj (apply union (filter #(contains? % 2) example)) 2)

(connected-component example 1)

(c-components m1)

)

