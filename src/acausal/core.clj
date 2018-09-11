(ns acausal.core
  (:refer-clojure :exclude [ancestors parents])
  (:require [clojure.pprint :as p]
            [clojure.set :refer [difference intersection subset? union]]
            [clojure.string :as string]
            [clojupyter.protocol.mime-convertible :as mc]
            [rhizome.viz]
            [rhizome.dot]))


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


;; TODO: improve efficiency?
(defn pairs-of
  "Returns all pairs of elements of coll as a set of sets."
  [coll]
  (set
    (for [i coll
          j coll
          :when (not= i j)]
      #{i j})))



;; TODO: validate arguments of constructor
(defrecord Model [pa bi])

(defn model
  "Returns a new model from dag with confounding."
  [dag & confounding]
  (let [bi (apply union (map pairs-of confounding))
        pa (into {} (for [[k v] dag] [k (set v)]))]
    (Model. pa bi)))



;; TODO: refactor (currently a hack for the rhizome visualization)
(defrecord Latent [ch])

(defn latent?
  "Returns true iff node is Latent."
  [node]
  (instance? acausal.core.Latent node))


(defn format-keyword
  "Returns a html subscripted string, given an appropriate keyword."
  [kword]
  (let [s (string/split (name kword) #"_")]
    (if (= (count s) 2)
      (str "<" (first s) "<SUB>" (second s) "</SUB>" ">")
      (name kword))))
 

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


(defn rhizome-graph
  "Returns a 'rhizome graph', given model m"
  [m]
  (into (transpose (:pa m))
        (for [multiedge (:bi m)]
          {(Latent. multiedge) multiedge})))


(def rhizome-options
  [:vertical? true
   :node->descriptor node->descriptor
   :edge->descriptor edge->descriptor])


;; TODO: refactor
;; A better design could incorporate 'native' bidirected edges, with
;; constraint=false, but rhizome doesn't support a clean way to do this.
;; Consider switching to Dorothy or Tangle.
(defn view-model
  "Rhizome visualization of model m."
  [m]
  (let [children (rhizome-graph m)]
    (apply rhizome.viz/view-graph
           (keys children)
           children
           rhizome-options)))


(defn model->svg
  "Returns m as an svg graphic."
  [m]
  (let [children (rhizome-graph m)]
    (apply rhizome.dot/graph->dot
           (keys children)
           children
           rhizome-options)))



;; TODO: validate arguments of constructor
;; TODO: rename arguments of constructor?
;; Note that i-map is an independence map: (x y z) -> bool
(defrecord Data [vars surrogate i-map])

(defn data
  "Returns a representation of the known joint probability function.
  i.e. P(v | do(z')) \\forall z' \\subseteq z"
  [v & {:keys [do* i-map] :or {do* [] i-map nil}}]
  (Data. (set v) (set do*) i-map))



;; TODO: validate arguments of constructor
;; TODO: alias (q ..) to query ?
(defrecord Query [effect hat])

(defn query
  "Returns a representation of the causal effect query.
  e.g. (query [:y_1 :y_2] :do [:x]) => P(y_1, y_2 | do(x))"
  [effect & {:keys [do] :or {do []}}]
  (Query. (set effect) (set do)))



(defn parents
  "Returns Pa(x)_m
  i.e. the parents of the nodes in x for model m."
  [m x]
  (apply union
         (for [node x]
           (get (:pa m) node))))


(defn ancestors
  "Returns An(x)_m
  i.e. the ancestors of the nodes x for model m, inclusive."
  [m x]
  (loop [frontier (set x)
         visited #{}]
  (if (empty? frontier)
    visited
    (recur (parents m frontier)
           (union visited frontier)))))


(defn verticies
  "Returns ver(m), i.e. the verticies of model m."
  [m]
  (set (keys (:pa m))))


(defn graph-cut
  "Returns a new graph where all keys in x now map to #{}.
  Graphs are assumed to be of the form {nodes #{nodes}}."
  [g x]
  (let [new-kv (for [k x] [k #{}])]
    (into g new-kv)))


;; TODO: improve efficiency?
;; Consider using transients: https://clojuredocs.org/clojure.core/disj%21
(defn pair-cut
  "Returns a new set of pairs (sets) such that all pairs that contained an
  element in x have been removed."
  [pairs x]
  (let [to-remove (filter #(or (contains? x (first %))
                               (contains? x (second %)))
                          pairs)]
    (apply disj pairs to-remove)))


(defn cut-incoming
  "Returns G_{\\overline{x}}
  i.e. a model where all incoming edges to nodes in x have been severed."
  [m x]
  (let [pa (graph-cut (:pa m) x)
        bi (pair-cut (:bi m) x)]
    (Model. pa bi)))


;; TODO: analyze efficiency
(defn subgraph
  "Returns G_{x}
  i.e. a model containing only the verticies in (set) x and edges between those
  verticies, including the bidirected edges."
  [m x]
  (let [to-remove (difference (verticies m) x)
        bi (pair-cut (:bi m) to-remove)
        pa (into {}
                 (for [[k v] (:pa m)
                       :when (contains? x k)]
                   [k (intersection v x)]))]
    (Model. pa bi)))


;; TODO: REFACTOR CONTINUES FROM HERE

;; efficient?
(defn adjacent
  "Helper function..."
  [pairs node]
  (disj (apply union
               (filter #(contains? % node) pairs))
        node))


;; efficient? rename?
(defn connected-component
  "Helper function... Assumes edges are set of set of multiedges, n is node"
  [pairs node]
  (loop [frontier (list node)
         visited #{}]
    (if (empty? frontier)
      visited
      (let [current (peek frontier)]
        (if (contains? visited current)
          (recur (pop frontier)
                 visited)
        ;else    
          (recur (into (pop frontier) (adjacent pairs current))
                 (conj visited current)))))))


;; TODO: test more thoroughly, cleanup
(defn c-components
  "Returns the confounded components of m as set of sets of verticies."
  [m]
  (loop [nodes (verticies m)
         components #{}]
    (if (empty? nodes)
      components
      (let [current-node (first nodes)
            current-component (connected-component (:bi m) current-node)]
        (recur (difference nodes current-component)
               (conj components current-component))))))


;; helper function
(defn find-superset
  "Given a collection of sets, return a set that is a superset of s,
  or nil if no such superset exists"
  [coll s]
  (first (filter #(subset? s %) coll)))


(defn sources
  "Return all nodes from dag g which have zero in-degree"
  [g]
  (set
    (filter #(empty? (get g %))
            (keys g))))


;; rename/restructure?
;; probably inefficient
(defn kahn-cut
  [g x]
  (into {}
        (for [[k v] g
              :when (not (contains? x k))]
          [k (difference v x)])))


;; TODO: test more thoroughly; restructure?
;; Should be able to take advantage of d-seperation structure (Tikka paper),
;; but can save that problem for later
;; ?? is a vector the right type for result?
(defn topological-sort
  "Given a model m, return a topological sort"
  [m]
  (loop [remaining (:pa m)
         result (empty [])]
    (if (empty? remaining)
      result
      (let [frontier (sources remaining)]
        (if (empty? frontier)
          (throw (Error. "Not a DAG"))
          (recur (kahn-cut remaining frontier)
                 (into result frontier)))))))


;; WARN: will return entire ordering if v not in ordering
(defn predecessors
  "Set of nodes preceding v in topological ordering"
  [ordering v]
  (set (take-while #(not= % v) ordering)))


;; TODO: determine structure of Formula
;; name field(s)?
(defrecord Formula [f])


;;; TODO: cleanup/restructure
(defn node->latex
  [n]
  (if (keyword? n)
    (name n)
    (str n)))


;; TODO: cleanup/restucture
(defn marginalize
  "\\sum_{sub} p
   p is the current distribution; sub is a set of vars"
  [p sub]
  (if (empty? sub)
    p
    {:sub sub :sum p}))


;; TODO: cleanup/restucture
(defn product
  "Temporary representation of \\sum_{sub} p
  coll - collection of probability expressions"
  [coll]
  (let [exprs (set coll)]
    (if (= (count exprs) 1)
      (first exprs)
      {:prod exprs})))


;; TODO: zID;
;; TODO: more testing, restructure?
(defn id
  "y set
   x set
   p formula type; for now, initially call with {:p #{vars}}
   g model"
  [y x p g]
  (let [v (verticies g)]
    (cond
      ;; line 1
      (empty? x)
      (marginalize p (difference v y))

      ;; line 2, refactor? (don't need the recur?)
      (not (empty? (difference v (ancestors g y))))
      (id y
          (intersection x (ancestors g y))
          {:p (ancestors g y)
           :where (marginalize p (difference v (ancestors g y)))}
          (subgraph g (ancestors g y)))

      ;; line 3
      :else
      (let [w (difference (difference v x) (ancestors (cut-incoming g x) y))]
        (if (not (empty? w))
          (id y (union x w) p g)
          
          ;; line 4
          (let [cg-x (c-components (subgraph g (difference v x)))]
            (if (> (count cg-x) 1)
              (marginalize
                (product
                  (for [si cg-x]
                    (id si
                        (difference v si)
                        p
                        g)))
                (difference v (union y x)))

              ;; line 5 (cgx should be singleton)
              (let [s (first cg-x)
                    cg (c-components g)]
                (if (= (first cg) (verticies g))
                  {:fail g :hedge s} ;; restructure!

                  ;; line 6
                  (if (contains? cg s)
                    (let [pi (topological-sort g)]
                      (marginalize
                        (product
                          (for [vi s]
                            (if (:where p)
                              {:where (:where p)
                               :p #{vi} :given (predecessors pi vi)}
                              {:p #{vi} :given (predecessors pi vi)})))
                        (difference s y)))

                    ;; line 7;
                    (if-let [s-prime (find-superset cg s)]
                      (let [pi (topological-sort g)
                            p-prime (product 
                                      (for [vi s]
                                        (if (:where p)
                                          {:where (:where p)
                                           :p #{vi}
                                           :given (predecessors pi vi)}
                                          {:p #{vi}
                                           :given (predecessors pi vi)})))]
                        (id y
                            (intersection x s-prime)
                            {:p s-prime :where p-prime}
                            (subgraph g s-prime)))

                      ;; fall-through
                      (throw (Error. "Should be unreachable")))))))))))))
                      

  
(defn identify
  "zID(C?) algorithm
   By default, assume P(v) as data [m q d]; [m q]"
  [y x m]
  (let [p {:p (verticies m)}]
    (id y x p m)))


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


(def kidney
  (model 
    {:recovery [:treatment :size]
     :size []
     :treatment [:size]}))


(def identifiable-a
  (model
    {:y [:x]
     :x []}))


(def identifiable-b
  (model
    {:x []
     :y [:x :z]
     :z [:x]}
    #{:z :y}))
     

(def identifiable-c
  (model
    {:x [:z]
     :y [:x :z]
     :z []}
    #{:z :y}))
     

(def identifiable-d
  (model
    {:x [:z]
     :y [:x :z]
     :z []}
    #{:x :z}))


(def identifiable-e
  (model
    {:x []
     :y [:z]
     :z [:x]}
    #{:x :y}))

(def identifiable-f
  (model
    {:x []
     :z_1 [:x]
     :z_2 [:z_1]
     :y [:x :z_1 :z_2]}
    #{:x :z_2}
    #{:z_1 :y}))

(def identifiable-g
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


(def blood-pressure
  (model 
    {:recovery [:blood-pressure :treatment]
     :blood-pressure [:treatment]
     :treatment []}))


(comment 

(view-model kidney)

(identify #{:y} #{:x} identifiable-a)

(identify #{:x} #{:y} identifiable-a)
(identify #{:y} #{:x} identifiable-b)
(identify #{:y} #{:x} identifiable-c)
(identify #{:y} #{:x} identifiable-d)
(identify #{:y} #{:x} identifiable-e)
(identify #{:y} #{:x} identifiable-f)

(identify #{:y} #{:x} identifiable-g)

(p/pprint (identify #{:y_1 :y_2} #{:x} hedge-less))

(p/pprint (identify #{:recovery} #{:treatment} kidney))

(p/pprint (identify #{:recovery} #{:treatment} blood-pressure))

(p/pprint (identify #{:y} #{:x} identifiable-g))

)


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

(comment

(identify #{:y} #{:x} non-a)
(identify #{:y} #{:x} non-b)
(identify #{:y} #{:x} non-c)
(identify #{:y} #{:x} non-d)
(identify #{:y} #{:x} non-e)
(identify #{:y} #{:x} non-f)
(identify #{:y} #{:x} non-g)
(identify #{:y} #{:x} non-h)

)





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


(comment

(view-model m2)

(difference #{:recovery :size :treatment} #{:treatment})

(cut-incoming kidney #{:treatment})

(ancestors (cut-incoming kidney #{:treatment}) #{:recovery})

(id #{:recovery} #{:treatment} ["P(v)"] kidney)

(id #{:recovery} #{} [] kidney)

)
