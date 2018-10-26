(ns acausal.core
  (:refer-clojure :exclude [ancestors parents])
  (:require [acausal.viz :as viz]
            [better-cond.core :as b]
            [clojupyter.protocol.mime-convertible :as mc]
            [clojure.core.matrix.dataset :as md]
            [clojure.core.matrix.impl.dataset]
            [clojure.data.csv :as csv]
            [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]
            [clojure.set :refer [difference intersection subset? union]]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint print-table]]))


(defmacro error
  "Throws a RuntimeException with optional additional data."
  [msg & keyvals]
  `(throw (ex-info (str ~msg) (hash-map ~@keyvals))))


;; TODO: consider a deformula macro instead to run identfy and bind it?
(defmacro define
  "Define a symbol as with def, but return the value."
  [symbol value]
  `(do
     (def ~symbol
       ~value)
     ~symbol))


;; OPTIMIZE (currently creates two copies of every pair)
(defn pairs-of
  "Returns all pairs of elements of coll as a set of sets."
  [coll]
  (set
    (for [i coll
          j coll
          :when (not= i j)]
      #{i j})))


;; TODO: validate arguments to constructor
(defrecord Model [pa bi])

(defn model
  "Returns a new causal diagram."
  [dag & confounding]
  (let [bi (apply union (map pairs-of confounding))
        pa (into {} (for [[k v] dag] [k (set v)]))]
    (->Model pa bi)))


(defmacro defmodel
  "Define a model, as with (def ...), but return the value of the model."
  [name docstring? & args]
  (if (string? docstring?)
    `(do
       (def ~name
         ~docstring?
         (model ~@args))
       ~name)
    `(do
       (def ~name
         (model ~docstring? ~@args))
       ~name)))


(defn cross-pairs
  "Returns the set of all unordered pairs #{i j} such that i != j
  where i is in coll1 and j is in coll2."
  [coll1 coll2]
  (set
    (for [i coll1
          j coll2
          :when (not= i j)]
      #{i j})))


;; OPTIMIZE (use transients?)
(defn make-latent
  "Returns a model where (single node) x is latent in m."
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
        bi-add (union (cross-pairs bi-ends ch-x)
                      (pairs-of ch-x))
        new-bi (union (difference (:bi m) bi-remove) bi-add)]
    (->Model new-pa new-bi)))


(defn latent-projection
  "Latent-project x in m."
  [m x]
  (reduce #(make-latent %1 %2) m x))


(defn parents
  "Returns Pa(x)_m
  i.e. the parents of the nodes in x for model m."
  [m x]
  (apply union
         (for [node x]
           (get (:pa m) node))))


(defn ancestors
  "Returns An(x)_m
  i.e. the ancestors of the nodes in x for model m, inclusive."
  [m x]
  (loop [frontier (set x)
         visited #{}]
  (if (empty? frontier)
    visited
    (recur (parents m frontier)
           (union visited frontier)))))


(defn vertices
  "Returns ver(m), i.e. the vertices of model m."
  [m]
  (set (keys (:pa m))))


(defn graph-cut
  "Returns a new graph where all keys in x now map to #{}.
  Graphs are assumed to be of the form {nodes #{nodes}}."
  [g x]
  (let [new-kv (for [k x] [k #{}])]
    (into g new-kv)))


;; OPTIMIZE (use transients to improve performance?)
(defn pair-cut
  "Returns a new set of pairs (2-sets) such that all pairs that
  contained an element in x have been removed."
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
    (->Model pa bi)))


(defn subgraph
  "Returns G_{x}
  i.e. a model containing only the vertices in set x and edges between those
  vertices, including the bidirected edges."
  [m x]
  (let [to-remove (difference (vertices m) x)
        bi (pair-cut (:bi m) to-remove)
        pa (into {}
                 (for [[k v] (:pa m)
                       :when (contains? x k)]
                   [k (intersection v x)]))]
    (->Model pa bi)))



(defn adjacent
  "Returns the nodes adjacent to node (via the bidirected edges in pairs)."
  [pairs node]
  (disj (apply union
               (filter #(contains? % node) pairs))
        node))


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


(defn c-components
  "Returns the confounded components of m as a set of sets of vertices."
  [m]
  (loop [nodes (vertices m)
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


(defn kahn-cut
  "Returns a dag g where all edges to and from x have been removed."
  [g x]
  (into {}
        (for [[k v] g
              :when (not (contains? x k))]
          [k (difference v x)])))


;; OPTIMIZE (kahn-cut generates a new graph each time it is called)
(defn topological-sort
  "Returns a topological sort of vertices in model m.
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


;; A formula is a recursive type of:
;; :prod #{formulas}
;; :sum formula, :sub #{vars}
;; :numer formula, :denom formula
;; :p #{vars}, :given #{vars}
;; :p #{vars}
(defrecord Formula [])

(defn formula?
  "Returns true iff f is a Formula."
  [f]
  (instance? Formula f))


(defn fail [g s]
  "Identification failure."
  {:hedge [g s]})


;; TODO: optimizations (collapse nested sums, marginalize)
(defn sum
  "Returns \\sum_{sub} p"
  [sub p]
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


(defn free
  "Returns the set of free variables in formula."
  [form]
  (cond
    (:given form)
    (union (:given form) (:p form))

    (:p form)
    (:p form)

    (:prod form)
    (apply union (map free (:prod form)))

    (:sum form)
    (difference (free (:sum form)) (:sub form))

    (:numer form)
    (union (free (:numer form)) (free (:denom form)))
    
    :else
    (error "free preconditions failed")))


;; TODO: optimizations (return probability expressions in terms of :given)
(defn given-pi
  "Returns P(vi \\mid v_{pi}^(i-1)), in terms of probability distribution p.
  pi is a topological order of nodes in G"
  [p vi pi]
  (let [pred (predecessors pi vi)
        unbound (difference (free p) (conj pred vi))
        numer (sum unbound p)
        denom (sum #{vi} numer)]
    {:numer numer
     :denom denom}))


(defn id
  "Shpitser's ID algorithm. Call with p = {:p (vertices g)}.
  Returns a formula, with any hedges inline."
  [y x p g]
  (b/cond
    :let [v (vertices g)]
    
    ;line 1
    (empty? x)
    (sum (difference v y) p)

    ;line 2
    :let [ancestors-y (ancestors g y)]
    (not (empty? (difference v ancestors-y)))
    (id y
        (intersection x ancestors-y)
        (sum (difference v ancestors-y) p)
        (subgraph g ancestors-y))

    ;line 3
    :let [w (difference (difference v x) (ancestors (cut-incoming g x) y))]
    (not (empty? w))
    (id y (union x w) p g)
    
    ;line 4
    :let [c-x (c-components (subgraph g (difference v x)))]
    (> (count c-x) 1)
    (sum (difference v (union y x))
         (product
           (for [si c-x]
             (id si (difference v si) p g))))

    ;line 5
    :let [s (first c-x)
          c (c-components g)]
    (= c #{v})
    (fail g s)

    ;line 6
    :let [pi (topological-sort g)]
    (contains? c s)
    (sum (difference s y)
         (product
           (for [vi s]
             (given-pi p vi pi))))

    ;line 7
    :let [s-prime (find-superset c s)
          p-prime (product
                    (for [vi s-prime]
                      (given-pi p vi pi)))]
    (not (nil? s-prime))
    (id y
        (intersection x s-prime)
        p-prime
        (subgraph g s-prime))

    :else
    (error "ID preconditions failed")))


(defn extract-hedges
  "Walk the formula and return the set of hedges inline."
  [form]
  (cond
    (:hedge form)
    (hash-set (:hedge form))

    (:sum form)
    (extract-hedges (:sum form))

    (:prod form)
    (reduce union (map extract-hedges (:prod form)))

    (:numer form)
    (union
      (extract-hedges (:numer form))
      (extract-hedges (:denom form)))

    (:p form)
    #{}

    :else
    (error "Unsupported formula type")))


(defrecord Query [p do given])

(defn query
  "Returns a representation of the causal effect query.
  e.g. (query [:y_1 :y_2] :do [:x]) => P(y_1, y_2 | do(x))"
  [effect & {:keys [do given] :or {do [] given []}}]
  (->Query (set effect) (set do) (set given)))

(def q
  "Alias for acausal.core/query."
  query)


(defrecord Data [joint])

(defn data
  "Returns a representation of the known joint probability function."
  [v]
  (->Data (set v)))

(def p
  "Alias for acausal.core/data"
  data)


;; A hedge is composed of
;; :g - a model that is a c-component
;; :s - a subset of vertices such that G, G \cap S is a hedge
(defrecord Hedge [g s])

;; A Fail record represents being unable to identify a query
(defrecord Fail [])

(defn fail?
  "Returns true iff f is an instance of Fail."
  [f]
  (instance? Fail f))


;; TODO: add support for IDC, zID, IDC*
(defn identify
  "Returns a formula that computes query q from data d in model m.
  Data defaults to P(v)."
  ([m q]
   (let [form (id (:p q) (:do q) {:p (vertices m)} m)
         hedges (map #(apply ->Hedge %) (extract-hedges form))]
     (if (empty? hedges)
       (into (->Formula) form)
       (assoc (->Fail) :hedges hedges))))
  ([m q d]
   (if (= (:joint d) (vertices m))
     (identify m q)
     (let [latents (difference (vertices m) (:joint d))
           projected-m (latent-projection m latents)]
       (identify projected-m q)))))


(defn identifiable?
  "True iff q is identifiable in m from P(v)"
  ([m q]
   (if (formula? (identify m q))
     true
     false))
  ([m q d]
   (if (formula? (identify m q d))
     true
     false)))


;; I/O

;; TODO: add options for header, processing options
;; wrap semantic-csv?
(defn read-csv
  "Reads CSV data into a core.matrix dataset.
  Assumes that the first row is column names."
  [filename]
  (with-open [reader (io/reader filename)]
    (let [data (csv/read-csv reader)]
      (md/dataset (map keyword (first data)) (rest data)))))


(defn head
  "Returns the first part of a dataset (default 10)."
  [dataset & {:keys [n] :or {n 10}}]
  (md/dataset (take n (md/row-maps dataset))))


(defn tail
  "Returns the last part of a dataset (default 10)."
  [dataset & {:keys [n] :or {n 10}}]
  (md/dataset (take-last n (md/row-maps dataset))))


(defn map-vals
  "Map a function over the values of a persistent map."
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))


;; Distributions

;; TODO: add laplace smoothing (Jeffrey prior smoothing?)
;; TODO: refactor as a protocol
;; Refactor design?
;; NOTE: support should be a map
;; make support optional second argument?
(defn categorical
  "Estimate a categorical distribution"
  [dataset & {:keys [support]}]
  {:samples (md/row-maps dataset)
   :support (map-vals set support)})


;; TODO: refactor into protocol?
;; expects full bindings
;; NOTE: especially for continuous variables, would like to have option
;; to get the density estimate, with some variables free.
(defn estimate-categorical-query
  "Estimate query from categorical distribution."
  [distribution expr bindings]
  (cond
    (not (empty? (:do expr)))
    (error "Cannot estimate causal query")

    (not (subset? (union (:p expr) (:given expr)) (set (keys bindings))))
    (error "Unbound variables in query")

    :else
    (loop [samples (:samples distribution), matching 0, total 0]
      (cond
        (nil? (first samples))
        (/ matching total)

        ; bindings don't match given => skip
        (not= (select-keys (first samples) (:given expr))
              (select-keys bindings (:given expr)))
        (recur (rest samples) matching total)
      
        ; bindings don't match p => increment total
        (not= (select-keys (first samples) (:p expr))
              (select-keys bindings (:p expr)))
        (recur (rest samples) matching (inc total))

        :else
        (recur (rest samples) (inc matching) (inc total))))))


(defn all-bindings
  "Given a map of vals -> collections, return a seq of maps that represents
  every possible instantiation."
  [m]
  (let [original (into (sorted-map) m)
        cart (apply combo/cartesian-product (vals original))]
    (map #(zipmap (keys original) %) cart)))


;; helper function for estimate-categorical-formula
;; TODO: write tests
(defn estimate-categorical-point
  [distribution expr bindings]
  (let [support (:support distribution)]
    (cond
      (:p expr)
      (estimate-categorical-query distribution expr bindings)

      (:prod expr)
      (reduce *
              (map #(estimate-categorical-point distribution % bindings)
                   (:prod expr)))

      (:numer expr)
      (/ (estimate-categorical-point distribution (:numer expr) bindings)
         (estimate-categorical-point distribution (:denom expr) bindings))
      
      ;TODO: test
      (:sum expr)
      (let [sum-support (all-bindings (select-keys support (:sub expr)))
            new-bindings (map #(merge bindings %) sum-support)]
        (reduce +
                (map #(estimate-categorical-point distribution (:sum expr) %)
                     new-bindings)))

      :else
      (error "Unsupported formula type"))))


;; TODO: rename?
;; TODO: add some way of getting confidence intervals
;; (probably based on number of samples from the original)
(defrecord EstimatedCategorical [pmf])


;; TODO: refactor as protocol
;; TODO: return a *distribution*
;; how to handle bindings contianing all vars?
;; ^ Technically, that's an error, but allow returning single probability val?
;; test
(defn estimate-categorical
  [distribution expr bindings]
  (let [support (:support distribution)
        all-vars (free expr)
        bound-vars (set (keys bindings))
        free-vars (difference all-vars bound-vars)
        new-bindings (all-bindings (select-keys support free-vars))]
    (->EstimatedCategorical
      (into {}
            (for [b new-bindings]
              {b (estimate-categorical-point
                   distribution expr (merge bindings b))})))))


;; TODO: refactor as protocol
;; (identify model data query)
;; (estimate distribution expr
;; The uber-function that calls protocols estimate-query or estimate-formula
;; Effectively double dispatch? Unneeded for categorical, but useful for
;; continuous valued random variables?
;; Consider defining the protocol to *only* support estimate,
;; maybe the summary stats as well? (seperate protocol?)
;; TODO: refactor/update/test
(defn estimate
  "Estimate expression expr with bindings, from distribution."
  [distribution expr bindings]
  nil)






;; LaTeX 'compilation'

(defn node->str
  "Convert a node to a LaTeX string."
  [n]
  (let [raw-str (if (keyword? n) (name n) (str n))]
    (cond
      (string/includes? raw-str "_") raw-str
      (= (count raw-str) 1) raw-str
      :else (format "\\text{%s}" raw-str))))


(defn set->str
  "Convert a set of nodes to a LaTeX string."
  [s]
  (string/join ", " (map node->str (sort s))))


(defn formula->latex
  "'Compile' a formula to a valid LaTeX math string."
  [formula]
  (cond
    (:sum formula)
    (format "\\left[ \\sum_{%s} %s \\right]"
            (set->str (:sub formula))
            (formula->latex (:sum formula)))

    (:prod formula)
    (string/join " " (map formula->latex (:prod formula)))

    (:numer formula)
    (format "\\frac{%s}{%s}"
            (formula->latex (:numer formula))
            (formula->latex (:denom formula)))

    (not (empty? (:given formula)))
    (format "P(%s \\mid %s)"
            (set->str (:p formula))
            (set->str (:given formula)))

    (:p formula)
    (format "P(%s)" (set->str (:p formula)))

    :else
    (error "Unable to compile to LaTeX")))


;; Jupyter integration
;; TODO: move to dedicated namespaces?
;; TODO: create a 'live' namespace


(extend-protocol mc/PMimeConvertible
  Model
  (to-mime [this]
    (mc/stream-to-string
      {:image/svg+xml (viz/model->svg this)})))


(extend-protocol mc/PMimeConvertible
  Formula
  (to-mime [this]
    (mc/stream-to-string
      {:text/latex (str "$$" (formula->latex this) "$$")})))


(extend-protocol mc/PMimeConvertible
  Hedge
  (to-mime [this]
    (mc/stream-to-string
      {:image/svg+xml (viz/hedge->svg this)})))


;; TODO: pretty-printed HTML tables?
(extend-protocol mc/PMimeConvertible
  clojure.core.matrix.impl.dataset.DataSet
  (to-mime [this]
    (mc/stream-to-string
      {:text/plain
        (with-out-str
          (print-table (md/row-maps this)))})))

