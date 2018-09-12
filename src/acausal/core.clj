(ns acausal.core
  (:refer-clojure :exclude [ancestors parents])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [difference intersection subset? union]]
            [clojure.string :as string]
            [clojupyter.protocol.mime-convertible :as mc]
            [rhizome.viz]))


(defmacro error
  "Throws a RuntimeException."
  [& more]
  `(throw (ex-info (str ~@more) {})))


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
    (apply rhizome.viz/graph->svg
           (keys children)
           children
           rhizome-options)))


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



;; TODO: make private?
(defn adjacent
  "Returns the nodes adjacent to node (via the bidirected edges in pairs)."
  [pairs node]
  (disj (apply union
               (filter #(contains? % node) pairs))
        node))


;; TODO: make private?
(defn connected-component
  "Returns the c-component of node."
  [pairs node]
  (loop [frontier (list node)
         visited #{}]
    (if (empty? frontier)
      visited
    ;else
      (let [current (peek frontier)]
        (if (contains? visited current)
          (recur (pop frontier)
                 visited)
        ;else    
          (recur (into (pop frontier) (adjacent pairs current))
                 (conj visited current)))))))


;; TODO: analyze efficiency
(defn c-components
  "Returns the confounded components of m as a set of sets of verticies."
  [m]
  (loop [nodes (verticies m)
         components #{}]
    (if (empty? nodes)
      components
      (let [current-node (first nodes)
            current-component (connected-component (:bi m) current-node)]
        (recur (difference nodes current-component)
               (conj components current-component))))))


(defn find-superset
  "Returns a superset of s or nil if no such superset exists.
  coll is a collection of sets."
  [coll s]
  (first (filter #(subset? s %) coll)))


(defn sources
  "Returns a set of all nodes in dag g which have zero in-degree.
  Assumes that g is of the form {nodes #{parents}}."
  [g]
  (set
    (filter #(empty? (get g %))
            (keys g))))


;; TODO: make private?
(defn kahn-cut
  "Returns a dag g where all edges to and from x have been removed."
  [g x]
  (into {}
        (for [[k v] g
              :when (not (contains? x k))]
          [k (difference v x)])))


;; TODO: make more efficienct
(defn topological-sort
  "Returns a topological sort of verticies in model m.
  Ties are broken by (sort ...); this ensures a unique sort."
  [m]
  (loop [remaining (:pa m)
         result (empty [])]
    (if (empty? remaining)
      result
      (let [frontier (sources remaining)]
        (if (empty? frontier)
          (error "Not a dag")
          (recur (kahn-cut remaining frontier)
                 (into result (sort frontier))))))))


(defn predecessors
  "Returns the set of items before v in ordering.
  Throws an exception if v is not in ordering."
  [ordering v]
  (let [before (set (take-while #(not= % v) ordering))]
    (if (= (count before) (count ordering))
      (error "Not in ordering")
      before)))


;; TODO: refactor design of formulas
;; Currently, formulas can be:
;; :prod #{formulas}
;; :sum formula, :sub #{vars}
;; :p #{vars}, :given #{vars} | nil, :where formula | nil
(defrecord Formula [])

(defn marginalize
  "Returns \\sum_{sub} p.
   p is the current probability function, sub is a set of vars."
  [p sub]
  (if (empty? sub)
    p
    {:sub sub :sum p}))


(defn product
  "Returns \\prod_i p_i.
  coll is a collection of probability functions."
  [coll]
  (let [exprs (set coll)]
    (if (= (count exprs) 1)
      (first exprs)
      {:prod exprs})))


;; TODO: refactor
;; TODO: implement zID
(defn id
  "Shpitser's ID algorithm
  set y
  set x
  formula p, initially {:p #{vars}}
  model g"
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



;; TODO: validate arguments of constructor
;; TODO: rename arguments of constructor?
(defrecord Query [effect do])

(defn query
  "Returns a representation of the causal effect query.
  e.g. (query [:y_1 :y_2] :do [:x]) => P(y_1, y_2 | do(x))"
  [effect & {:keys [do] :or {do []}}]
  (Query. (set effect) (set do)))

;; TODO: remove alias?
(def q
  "Alias for acausal.core/query."
  query)


;; TODO: validate arguments of constructor
;; TODO: rename arguments of constructor (joint surrogate)?
;; TODO: keep explicit i-map argument?
(defrecord Data [vars surrogate i-map])

(defn data
  "Returns a representation of the known joint probability function.
  i.e. P(v | do(z')) \\forall z' \\subseteq z"
  [v & {:keys [do* i-map] :or {do* [] i-map nil}}]
  (Data. (set v) (set do*) i-map))

;; TODO: remove alias?
(def p
  "Alias for acausal.core/data"
  data)


;; TODO: properly implement (identify m q d)
(defn identify
  "Returns a formula that computes query q from data d in model m.
  Data defaults to P(v)."
  ([m q]
   (let [p {:p (verticies m)}]
     (into (->Formula)
           (id (:effect q) (:do q) p m))))
  ([m q d]
   (if (and (= (:vars d) (verticies m))
            (empty? (:surrogate d)))
     (identify m q)
     (error "Unimplemented"))))


;; Jupyter integration
;; TODO: seperate into new namespace?
;; TODO: render Query and Data types?

(extend-protocol mc/PMimeConvertible
  Model
  (to-mime [this]
    (mc/stream-to-string
      {:image/svg+xml (model->svg this)})))


(extend-protocol mc/PMimeConvertible
  Formula
  (to-mime [this]
    (mc/stream-to-string
      {:text/plain (pprint this)})))


(comment

(extend-protocol mc/PMimeConvertible
  Formula
  (to-mime [this]
    (mc/stream-to-string
      {:text/latex (str "$" (str this) "$")})))

)


;; Example models
;; TODO: move to dedicated test namespace

(def kidney
  (model 
    {:recovery [:treatment :size]
     :size []
     :treatment [:size]}))


(def blood-pressure
  (model 
    {:recovery [:bp :treatment]
     :bp [:treatment]
     :treatment []}))


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


;; Other models

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


;; TODO: remove

(comment 

(view-model kidney)
(view-model ident-b)
(view-model ident-c)

(def yx (query [:y] :do [:x]))

(identify ident-a yx)
(identify ident-b yx)
(identify ident-c yx)
(identify ident-d yx)
(identify ident-e yx)
(identify ident-f yx)
(identify ident-g yx)

(pprint
  (identify
    ident-b
    (q [:y] :do [:x])
    (p [:x :y :z])))

(pprint
  (identify
    hedge-less
    (q [:y_1 :y_2] :do [:x])))


(pprint
  (identify
    kidney
    (q [:recovery] :do [:treatment])))

(pprint
  (identify
    blood-pressure
    (q [:recovery] :do [:treatment])))

(identify non-a yx)
(identify non-b yx)
(identify non-c yx)
(identify non-d yx)
(identify non-e yx)
(identify non-f yx)
(identify non-g yx)
(identify non-h yx)

)

