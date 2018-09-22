(ns acausal.core
  (:refer-clojure :exclude [ancestors parents])
  (:require [better-cond.core :as b]
            [clojure.pprint :refer [pprint]]
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


(defmacro defmodel
  "Define a model, as with (def ...), but return the value of the model.
  Useful for notebook usage."
  [name docstring? & args]
  (if (string? docstring?)
    `(do
       (def ~name
         ~docstring?
         (model ~@args))
       ~name)
    `(do
       (def ~name
         (model
           ~docstring? ~@args))
       ~name)))


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

;; hedge structure
;; g is a model that is a c-component
;; s is a subset of verticies such that G, G \intersect S is a hedge
;; TODO: Hedges should be renderable (highlight the subset?)
(defrecord Hedge [g s])


(defn formula?
  "Returns true iff f is a Formula."
  [f]
  (instance? acausal.core.Formula f ))

;; reorder args?
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
;; Using exceptions seems like a bit of a hack.
;; Premature optimization? Consider just returning a :hedge in the pre-formula
(defmacro fail
  "Throws an exception representing a failure to identify.
  g is a causal diagram. s is a c-component."
  [g s]
  `(throw (ex-info "fail" {:hedge (->Hedge ~g ~s)})))


;; NOTE: returns a 'pre-formula'
;; NOTE: assumed to be called with p = {:p (verticies g)}
;; TODO: set single topological ordering?
;; TODO: can line 2 be simplified?
;; TODO: restructure line 5 ?
;; TODO: restructure line 6 and 7, predecessors, :p, :given shouldn't be empty
;; TODO: restructure fail (make optional?)
(defn id
  "Shpitser's ID algorithm."
  [y x p g]
  (b/cond
    :let [v (verticies g)]
    
    ;line 1
    (empty? x)
    (marginalize p (difference v y))

    ;line 2
    :let [an-y (ancestors g y)]
    (not (empty? (difference v an-y)))
    (id y
        (intersection x an-y)
        {:p an-y :where (marginalize p (difference v an-y))}
        (subgraph g an-y))

    ;line 3
    :let [w (difference (difference v x) (ancestors (cut-incoming g x) y))]
    (not (empty? w))
    (id y (union x w) p g)
    
    ;line 4
    :let [c-x (c-components (subgraph g (difference v x)))]
    (> (count c-x) 1)
    (marginalize
      (product (for [si c-x]
                 (id si (difference v si) p g)))
      (difference v (union y x)))

    ;line 5
    :let [s (first c-x)
          c (c-components g)]
    (= c #{v})
    {:hedge [g s]}

    ;line 6
    :let [pi (topological-sort g)]
    (contains? c s)
    (marginalize
      (product (for [vi s]
                 (if (:where p)
                   {:where (:where p)
                    :p #{vi} :given (predecessors pi vi)}
                   {:p #{vi} :given (predecessors pi vi)})))
      (difference s y))

    ;line 7
    :let [s-prime (find-superset c s)
          p-prime (product (for [vi s-prime]
                             (if (:where p)
                               {:where (:where p)
                                :p #{vi} :given (predecessors pi vi)}
                               {:p #{vi} :given (predecessors pi vi)})))]
    (not (nil? s-prime))
    (id y
        (intersection x s-prime)
        {:p s-prime :where p-prime}
        (subgraph g s-prime))

    :else
    (error "ID preconditions failed")))


;; TODO: rename?
(defn extract-hedge
  "Walk the pre-formula and return the (first) hedge as [g s].
  Returns nil if no hedge exists."
  [pre-form]
  (cond
    (:hedge pre-form)
    (:hedge pre-form)

    (:sum pre-form)
    (extract-hedge (:sum pre-form))

    (:where pre-form)
    (extract-hedge (:where pre-form))

    (:prod pre-form)
    (first (remove nil? (map extract-hedge (:prod pre-form))))

    :else
    nil))


(comment

  (if-let [[g s] (extract-hedge pre-form)]
    (->Hedge g s)
    )

)

;; TODO: rename?
(defn compile-formula
  "Returns a valid formula, given a pre-formula, i.e. result of (id ...)"
  [pre-form]
  (cond
    (:sum pre-form)
    {:sum (compile-formula (:sum pre-form)) :sub (:sub pre-form)}

    (:prod pre-form)
    {:prod (set (map compile-formula (:prod pre-form)))}

    (and (:where pre-form) (nil? (:given pre-form)))
    (compile-formula (:where pre-form))

    (and (:where pre-form) (:given pre-form))
    (let [where (compile-formula (:where pre-form))]
      {:numer where
       :denom (marginalize where (:p pre-form))})

    :else
    pre-form))


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
;; NOTE returns either a Formula or a Hedge
(defn identify
  "Returns a formula that computes query q from data d in model m.
  Data defaults to P(v)."
  ([m q]
   (let [pre-form (id (:effect q) (:do q) {:p (verticies m)} m)]
     (if-let [hedge (extract-hedge pre-form)]
       (apply ->Hedge hedge)
       (into (->Formula)
             (compile-formula pre-form)))))
  ([m q d]
   (if (and (= (:vars d) (verticies m))
            (empty? (:surrogate d)))
     (identify m q)
     (error "Unimplemented"))))


;; BORK
(defn identifiable?
  "True iff q is identifiable in m from P(v)"
  ([m q]
   (formula? (identify m q)))
  ([m q d]
   (formula? (identify m q d))))


;; TODO: improve
(defn node->str
  "..."
  [n]
  (let [raw-str (if (keyword? n) (name n) (str n))]
    (cond
      (string/includes? raw-str "_") raw-str
      (= (count raw-str) 1) raw-str
      :else (format "\\text{%s}" raw-str))))


(defn set->str
  "..."
  [s]
  (string/join ", " (map node->str (sort s))))


;;; BORK
(defn formula->latex
  "'Compile' a formula to a valid LaTeX math string."
  [formula]
  (cond
    (:sum formula)
    (format "(\\sum_{%s} %s)"
            (set->str (:sub formula))
            (formula->latex (:sum formula)))

    (:prod formula)
    (string/join " " (map formula->latex (:prod formula)))

    (:numer formula)
    (format "\\frac{%s}{%s}"
            (formula->latex (:numer formula))
            (formula->latex (:denom formula)))

    ;; This is a hack; TODO: fix id so it doesn't return empty :given sets
    (and (:given formula) (not (empty? (:given formula))))
    (format "P(%s \\mid %s)"
            (set->str (:p formula))
            (set->str (:given formula)))

    (:p formula)
    (format "P(%s)" (set->str (:p formula)))

    :else
    (error "Unable to compile to LaTeX")))


;; Jupyter integration
;; TODO: seperate into new namespace?
;; TODO: render Query and Data types? (probably not)
;; TODO: render Hedge

(extend-protocol mc/PMimeConvertible
  Model
  (to-mime [this]
    (mc/stream-to-string
      {:image/svg+xml (model->svg this)})))


(extend-protocol mc/PMimeConvertible
  Formula
  (to-mime [this]
    (mc/stream-to-string
      {:text/latex (str "\\[" (formula->latex this) "\\]")})))


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


(def napkin
    "Napkin model"
    (model
        {:z_1 [:z_2]
         :x [:z_1]
         :y [:x]
         :z_2 []}
        #{:x :z_2}
        #{:z_2 :y}))

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


;; TODO: move/refactor into tests

(comment 

(view-model kidney)
(view-model ident-b)
(view-model ident-c)


(identify ident-a (q [:y] :do [:x]))
(identify ident-b (q [:y] :do [:x]))
(identify ident-c (q [:y] :do [:x]))
(identify ident-d (q [:y] :do [:x]))
(identify ident-e (q [:y] :do [:x]))
(identify ident-f (q [:y] :do [:x]))
(identify ident-g (q [:y] :do [:x]))

(->
  (identify
    ident-g
    (q [:y] :do [:x]))
  formula->latex)

(pprint
  (identify
    ident-b
    (q [:y] :do [:x])
    (p [:x :y :z])))


(pprint
  (identify
    ident-g
    (q [:y] :do [:x])))


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


(identify non-a (q [:y] :do [:x]))
(identify non-b (q [:y] :do [:x]))
(identify non-c (q [:y] :do [:x]))
(identify non-d (q [:y] :do [:x]))
(identify non-e (q [:y] :do [:x]))
(identify non-f (q [:y] :do [:x]))
(identify non-g (q [:y] :do [:x]))
(identify non-h (q [:y] :do [:x]))

)

;; Experiments

(defn gen-nodes
  "Generate n keywords indexed :n_i, from 1"
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


;; temp
(defn cross-pairs
  [coll1 coll2]
  (for [i coll1
        j coll2
        :when (not= i j)]
    #{i j}))


;; TODO: refactor, check, efficiency (transients?); may be broken
(defn make-latent
  "Latent project one node"
  [m x]
  (let [pa-x (get (:pa m) x)
        ch-x (set (map first (filter #(contains? (second %) x) (:pa m))))
        new-pa (into {}
                     (for [[k v] (:pa m)
                           :when (not= k x)]
                       (if (contains? ch-x k)
                           [k (disj (into v pa-x) x)]
                           [k v])))
        bi-remove (set (filter #(contains? % x) (:bi m)))
        bi-ends (map #(first (disj % x)) bi-remove)
        bi-add (union (set (cross-pairs bi-ends ch-x))
                      (pairs-of ch-x)) ;; TODO: correct?
        new-bi (union (difference (:bi m) bi-remove) bi-add)]
    (->Model new-pa new-bi)))


(defn latent-projection
  "Latent-project x in m."
  [m x]
  (reduce #(make-latent %1 %2) m x))


(defn erdos-renyi-model
  "Produce a random model (dag) with n nodes and probability p of edge"
  [n p]
  (model (transpose (erdos-renyi-dag n p))))


(comment

  (def tmp
    (model
      {:q []
       :w [:q]
       :p [:w]
       :x [:w]
       :y [:x]
       :z [:x]}))

(view-model tmp)

(view-model (latent-projection tmp [:w]))

  (view-model tmp)

(let [n 10]
  (view-model (erdos-renyi-model n (/ (java.lang.Math/log 10) 10))))

(let [n 10]
  (view-model (erdos-renyi-model n (/ 1 n))))

)

;; need incanter.charts/scatter-plot
;; (-> thechart (.createBufferedImage width height))
;; https://github.com/aria42/clojupyter/blob/mime-improvements/examples/incanter-demo.ipynb

