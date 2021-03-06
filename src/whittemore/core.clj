(ns whittemore.core
  (:refer-clojure :exclude [ancestors parents])
  (:use [whittemore.protocols])
  (:require [whittemore.graphviz :as viz]
            [whittemore.util :refer [warn error map-vals]]
            [better-cond.core :as b]
            [clojupyter.protocol.mime-convertible :as mc]
            [clojure.core.matrix.dataset :as md]
            [clojure.math.combinatorics :as combo]
            [clojure.set :refer [difference intersection subset? union]]
            [clojure.string :as string]
            [gorilla-renderable.core :as gr]))


(defn transpose
  "Alpha - subject to change.
  Returns the transpose of directed graph g.
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


(defmacro define
  "Alpha - subject to change.
  Define a symbol as with def, but return the value."
  [symbol & init?]
  (if (= (count init?) 2)
    (let [docstring (first init?)
          init (second init?)]
      `(do
         (def ~symbol ~docstring ~init)
         ~init))
    `(do
       (def ~symbol ~@init?)
       ~@init?)))


;; OPTIMIZE (currently creates two copies of every pair)
(defn pairs-of
  "Returns all pairs of elements of coll as a set of sets."
  [coll]
  (set
    (for [i coll
          j coll
          :when (not= i j)]
      #{i j})))


;; Model

;; TODO: validate arguments to constructor
(defrecord Model [pa bi])

(defn model
  "Returns a new causal diagram.
  model does not currently check that its arguments are valid."
  [dag & confounding]
  (let [bi (apply union (map pairs-of confounding))
        pa (into {} (for [[k v] dag] [k (set v)]))]
    (->Model pa bi)))


(defmacro defmodel
  "Alpha - subject to change.
  Define a model, as with (def ...), but return the value of the model."
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


(defn- cross-pairs
  "Returns the set of all unordered pairs #{i j} such that i != j
  where i is in coll1 and j is in coll2."
  [coll1 coll2]
  (set
    (for [i coll1
          j coll2
          :when (not= i j)]
      #{i j})))


;; OPTIMIZE (use transients?)
(defn- make-latent
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
  "Latent-project (set) x in m."
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


(defn- graph-cut
  "Returns a new graph where all keys in x now map to #{}.
  Graphs are assumed to be of the form {nodes #{nodes}}."
  [g x]
  (let [new-kv (for [k x] [k #{}])]
    (into g new-kv)))


;; OPTIMIZE (use transients to improve performance?)
(defn- pair-cut
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


;; TODO: rename?
(defn- adjacent
  "Returns the nodes adjacent to node (via the bidirected edges in pairs)."
  [pairs node]
  (disj (apply union
               (filter #(contains? % node) pairs))
        node))


;; TODO: refactor?
(defn- connected-component
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


(defn sources
  "Returns a set of all nodes in dag g which have zero in-degree.
  Assumes that g is of the form {nodes #{parents}}."
  [g]
  (set
    (filter #(empty? (get g %))
            (keys g))))


;; TODO: rename?
(defn- kahn-cut
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


;; Utility functions for ID

(defn find-superset
  "Returns a superset of s or nil if no such superset exists.
  coll is a collection of sets."
  [coll s]
  (first (filter #(subset? s %) coll)))

(defn predecessors
  "Returns the set of items before v in ordering.
  Throws an exception if v is not in ordering."
  [ordering v]
  (let [before (set (take-while #(not= % v) ordering))]
    (if (= (count before) (count ordering))
      (error "Not in ordering")
      before)))


;; A 'form' is a recursive map of:
;; {:prod #{forms}}
;; {:sum forms, :sub #{vars}}
;; {:numer form, :denom form
;; {:p #{vars}, :given #{vars}}
;; {:p #{vars}}


;; TODO: simplifications (e.g. collapse nested sums, marginalize out)
(defn sum
  "Returns \\sum_{sub} p"
  [sub p]
  (if (empty? sub)
    p
    {:sub sub :sum p}))

;;; FIXME: possible bug in definition (should :prod not be a set?)
(defn product
  "Returns \\prod_i p_i.
  coll is a collection of probability functions."
  [coll]
  (let [exprs (set coll)]
    (if (= (count exprs) 1)
      (first exprs)
      {:prod exprs})))


(defn free
  "Returns the set of free variables in a form."
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


;; TODO: refactor?
;; TODO: simplifications
(defn- given-pi
  "Returns P(vi \\mid v_{pi}^(i-1)), in terms of probability distribution p.
  pi is a topological order of nodes in G"
  [p vi pi]
  (let [pred (predecessors pi vi)
        unbound (difference (free p) (conj pred vi))
        numer (sum unbound p)
        denom (sum #{vi} numer)]
    {:numer numer
     :denom denom}))


(defn hedge [g s]
  "ID failure."
  {:hedge g :s s})


(defn id
  "Shpitser's ID algorithm. Call with p = {:p (vertices g)}.
  Returns a form, with any hedges inline."
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
    (hedge g s)

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
  "Walk the form and return the set of hedges inline."
  [form]
  (cond
    (:hedge form)
    (hash-set form)

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


(defn- vars-of
  [coll]
  (if (map? coll)
    (set (keys coll))
    (set coll)))


;; form is map of {:p #{vars}, :do #{vars}, :given #{vars}}
(defrecord Query [form])

(defn query?
  "Returns true iff f is an instance of Query."
  [f]
  (instance? Query f))

(defn unbound-query
  "Returns a representation of a causal effect query without variable bindings.
  Formulas returned from (identify ...) will be unbound."
  [effect do given]
  (->Query {:p (vars-of effect) :do (vars-of do) :given (vars-of given)}))

(defn bound-query
  "Returns a representation of a causal effect query with variable bindings.
  do and given should be maps. effect should be a set or vector.
  Formulas returned from (identify ...) will be bound, i.e. suitable as a
  parameter for (estimate ...)"
  [effect do given]
  (assoc 
    (->Query {:p (vars-of effect) :do (vars-of do) :given (vars-of given)})
    :bindings (merge do given)))

(defn event-query
  "Returns a representation of a causal effect query with variable bindings.
  do and given should be maps. effect should be a map, and will be bound to
  the :event key of the resulting query."
  [effect do given]
  (assoc 
    (->Query {:p (vars-of effect) :do (vars-of do) :given (vars-of given)})
    :bindings (merge do given)
    :event effect))


(defn q
  "Query. Returns a representation of the causal effect query.
  Prefer calling this over the *-query constructors.
  q does not currently check that the bindings are consistent."
  [effect & {:keys [do given] :or {do {} given {}}}]
  (cond
    (and (map? effect) (map? do) (map? given))
    (event-query effect do given)

    (and (map? do) (map? given))
    (bound-query effect do given)

    (or (and (empty? do) (not (map? given)))
        (and (empty? given) (not (map? do)))
        (and (not (map? given)) (not map? do)))
    (unbound-query effect do given)

    :else
    (error "Unsupported query type (check bindings to q)")))


(defrecord Data [joint])

(defn data
  "Returns a representation of the known joint probability function."
  [v]
  (->Data (set v)))

(defn data?
  "Returns true iff f is an instance of Fail."
  [f]
  (instance? Data f))


;; Formula wraps a 'form'
(defrecord Formula [form])

(defn formula?
  "Returns true iff f is a Formula."
  [f]
  (instance? Formula f))


;; A Fail record represents being unable to identify a query
(defrecord Fail [cause])

(defn fail?
  "Returns true iff f is an instance of Fail."
  [f]
  (instance? Fail f))


;; TODO: refactor/cleanup
(defn simplify-marginalize
  "Rule: marginalize, if possible, else return"
  [form]
  (if (and (:sum form) (:p (:sum form)))
    {:p (difference (:p (:sum form)) (:sub form))}
    form))


;; TODO: refactor/cleanup
;; FIXME: check type of the :prod (vector? set?)
(defn simplify-form
  "Janky implementation of form(ula) simplification.
  Alpha version."
  [form]
  (cond
    ; form is a probability expression, return
    (:p form) form 

    ; form is a prod, recursively simplify
    (:prod form)
    {:prod (map simplify-form (:prod form))}

    ; form is a fraction, return
    (:numer form)
    (let [simplified-numer (simplify-form (:numer form))
          simplified-denom (simplify-form (:denom form))]
      (if (and (:p simplified-numer) (:p simplified-denom)
               (subset? (:p simplified-denom) (:p simplified-numer)))
        {:p (difference (:p simplified-numer) (:p simplified-denom))
         :given (:p simplified-denom)}
      ;else
        {:numer simplified-numer
         :denom simplified-denom}))

    ; form is a sum, recurse on :sum, then try to marginalize out
    (:sum form)
    (let [simplified-sum (simplify-form (:sum form))]
      (if (:p simplified-sum) ; :sum part reduces to probability expression
        {:p (difference (:p simplified-sum) (:sub form))}
        {:sum simplified-sum :sub (:sub form)}))

    ; form is a hedge, return the hedge
    (:hedge form)
    form

    ; unknown type
    :else
    (error "Unsupported formula type")))


;; TODO: add support for zID, IDC, IDC*
;; TODO: update 'simplification pipeline'
(defn identify
  "Returns a formula that computes query q from data d in model m.
  Data defaults to P(v), i.e. joint distribution over all variables in m.
  
  Note that formulas are not currently reduced to simpler, equivalent forms."
  ([model query]
   (let [q (:form query)]
     (cond
       (not (query? query)) (error "Unsupported Query type (check args)")
       (not (empty? (:given q))) (error "Unsupported Query type (:given)")
     :else
       (let [raw-form (id (:p q) (:do q) {:p (vertices model)} model)
             form (simplify-form raw-form)
             hedges (extract-hedges form)]
         (cond
           (not (empty? hedges))
           (->Fail hedges)

           (:event query)
           (assoc (->Formula form) :event (:event query)
                                   :bindings (:bindings query))

           (:bindings query)
           (assoc (->Formula form) :bindings (:bindings query))

           :else
           (->Formula form))))))
  ([model data query]
   (cond
     (not (data? data)) (error "Unsupported Data type (check args)")
     (= (:joint data) (vertices model)) (identify model query)
   :else
     (let [latents (difference (vertices model) (:joint data))
           projected-model (latent-projection model latents)]
       (identify projected-model query)))))


(defn identifiable?
  "True iff q is identifiable in m from P(v)"
  ([model query]
    (formula? (identify model query)))
  ([model data query]
    (formula? (identify model data query))))


;; Distributions


(defn bind
  "Associate bindings with unbound formula, returning a formula suitable
  as an argument for (estimate ...).
  bind does not currently check that the bindings are valid."
  [formula bindings]
  (if (:bindings formula)
    (warn "WARNING: formula is already bound:" (:bindings formula)
          ", being replaced by:" bindings))
  (assoc formula :bindings bindings))


(defn measure
  "Returns the probability of the given event.
  Options are passed to the underlying protocol."
  [distribution event & {:as options}]
  (measure-probability distribution event options))


(defn estimate
  "Estimate the result of applying the bound formula to distribution.
  Returns a new distribution. Options are passed to the underlying protocol.

  If bindings for :event are included in the formula, then estimate acts
  as 'syntatic sugar' for (measure (estimate distribution bindings) event).
  
  Note that estimate does not currently check that bindings are valid;
  improper use may yield nonsensical estimated distributions."
  [distribution formula & {:as options}]
  (cond
    (nil? (:bindings formula))
    (error "Unbound formula, cannot estimate")

    (:event formula)
    (-> distribution
        (estimate (dissoc formula :event))
        (measure (:event formula)))

    :else
    (estimate-distribution distribution formula options)))


(defn all-vals
  "Alpha - subject to change.
  Given a collection of maps, each with the same keys, return a map of
  keys to set of possible values."
  [maps]
  (let [init (map-vals (fn [_] (hash-set)) (first maps))]
    (reduce #(merge-with conj %1 %2) init maps)))


;; TODO: add optional laplace smoothings
;; TODO: accept plain map (of pmf) for constructor
(defrecord Categorical [pmf])
(defrecord EmpiricalCategorical [samples support])

(defn categorical
  "Estimate a categorical distribution, where dataset is one of:
  - a clojure.core.matrix dataset
  - a seq of maps of random variables to values"
  [dataset & {:as options}]
  (let [samples (if (md/dataset? dataset) (md/row-maps dataset) dataset)
        support (all-vals samples)]
    (->EmpiricalCategorical samples support)))


(defn- empirical-categorical-p
  "Helper function. Estimate the probability of an expression of form,
  {:p _, :given _}, from an EmpiricalCategorical distribution."
  [distribution expr bindings]
  (cond
    (not (empty? (:do expr)))
    (error "Unable to estimate causal query (:do)")

    (not (subset? (union (:p expr) (:given expr)) (set (keys bindings))))
    (error "Unbound variables in query")

    :else
    (loop [samples (:samples distribution),
           matching 0.0, total 0.0] ; force Double precision
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


(defn- all-bindings
  "Given a map of vals -> collections, return a seq of maps that represents
  every possible instantiation."
  [m]
  (let [original (into (sorted-map) m)
        cart (apply combo/cartesian-product (vals original))]
    (map #(zipmap (keys original) %) cart)))


(defn- empirical-categorical-f
  "Helper function. Evaluate a formula over a EmpiricalCategorical
  distribution, given bindings for all variables.
  Returns a single probability."
  [distribution expr bindings]
  (let [support (:support distribution)]
    (cond
      (:p expr)
      (empirical-categorical-p distribution expr bindings)

      (:prod expr)
      (reduce *
              (map #(empirical-categorical-f distribution % bindings)
                   (:prod expr)))

      (:numer expr)
      (/ (empirical-categorical-f distribution (:numer expr) bindings)
         (empirical-categorical-f distribution (:denom expr) bindings))

      ; generate all bindings for variables in :sub
      ; note the "lexical scoping" w/ merge
      (:sum expr)
      (let [sum-support (all-bindings (select-keys support (:sub expr)))
            new-bindings (map #(merge bindings %) sum-support)]
        (reduce +
                (map #(empirical-categorical-f distribution (:sum expr) %)
                     new-bindings)))

      :else
      (error "Unsupported formula type"))))


;; OPTIMIZE (implementation is naive)
(extend-type EmpiricalCategorical
  Distribution
  (estimate-distribution [distribution formula options]
    (let [form (:form formula)
          bindings (:bindings formula)
          support (:support distribution)
          all-vars (free form)
          bound-vars (set (keys bindings))
          free-vars (difference all-vars bound-vars)
          new-bindings (all-bindings (select-keys support free-vars))]
      (->Categorical
        (into {}
              (for [b new-bindings]
                {b (empirical-categorical-f
                     distribution form (merge bindings b))})))))
  (measure-probability [distribution event options]
    (measure-probability ; polymorphic recursion on Categorical
      (estimate-distribution distribution (q (keys event)) options)
      event {}))
  (signature [distribution]
    (-> distribution :support keys data)))


(defn- exact-categorical-marginal
  "Helper function. Estimate the marginal probability of a Categorical
  distribution evaluated at bindings."
  [distribution bindings]
  (reduce
    (fn [total mv]
      (let [m (first mv)
            v (second mv)]
        (if (= (select-keys m (keys bindings)) bindings)
          (+ total v)
          total)))
    0
    (:pmf distribution)))


(defn- exact-categorical-p
  "Helper function. Estimate the probability of an expression of form,
  {:p _, :given _}, from an Categorical distribution."
  [distribution expr bindings]
  (cond
    (not (empty? (:do expr)))
    (error "Unable to estimate causal query (:do)")

    (not (subset? (union (:p expr) (:given expr)) (set (keys bindings))))
    (error "Unbound variables in query")

    (:given expr)
    (/
     (exact-categorical-p
       distribution
       {:p (union (:p expr) (:given expr))}
       bindings)
     (exact-categorical-p distribution {:p (:given expr)} bindings))

    (:p expr)
    (exact-categorical-marginal distribution (select-keys bindings (:p expr)))

    :else
    (error "Unsupported formula type")))


(defn- exact-categorical-f
  "Helper function. Evaluate a formula over a Categorical
  distribution, given bindings for all variables.
  Returns a single probability."
  [distribution expr bindings]
  (let [support (:support distribution)] ; hack, refactor
    (cond
      (:p expr)
      (exact-categorical-p distribution expr bindings)

      (:prod expr)
      (reduce *
              (map #(exact-categorical-f distribution % bindings)
                   (:prod expr)))

      (:numer expr)
      (/ (exact-categorical-f distribution (:numer expr) bindings)
         (exact-categorical-f distribution (:denom expr) bindings))

      ; generate all bindings for variables in :sub
      ; note the "lexical scoping" w/ merge
      (:sum expr)
      (let [sum-support (all-bindings (select-keys support (:sub expr)))
            new-bindings (map #(merge bindings %) sum-support)]
        (reduce +
                (map #(exact-categorical-f distribution (:sum expr) %)
                     new-bindings)))

      :else
      (error "Unsupported formula type"))))


;; OPTIMIZE (implementation is naive)
;; TODO: refactor, deduplicate code from EmpiricalCategorical
(extend-type Categorical
  Distribution
  (estimate-distribution [distribution formula options]
    (let [form (:form formula)
          bindings (:bindings formula)
          support (all-vals (keys (:pmf distribution)))
          distribution (assoc distribution :support support) ; hack
          all-vars (free form)
          bound-vars (set (keys bindings))
          free-vars (difference all-vars bound-vars)
          new-bindings (all-bindings (select-keys support free-vars))]
      (->Categorical
        (into {}
              (for [b new-bindings]
                {b (exact-categorical-f
                     distribution form (merge bindings b))})))))
  (measure-probability [distribution event options]
    (get (:pmf distribution) event))
  (signature [distribution]
    (-> distribution :pmf keys first keys data)))


;; infer

;; TODO: add more options, macro form with _
(defn infer
  "Syntactic sugar for identify and estimate."
  [model distribution query & {:as options}]
  (let [data (signature distribution)
        formula (identify model data query)]
    (estimate distribution formula)))


;; LaTeX

(defn node->str
  "Convert a node to a LaTeX string."
  [n]
  (let [raw-str (if (keyword? n) (name n) (str n))]
    (cond
      (string/includes? raw-str "_") raw-str
      (= (count raw-str) 1) raw-str
      :else (format "\\text{%s}" raw-str))))

(defn val->str
  "Convert a value to a LaTeX string."
  [v]
  (cond
    (number? v) (str v)
    :else (format "\\text{\"%s\"}" v)))
;   :else (format "\\text{$\"$%s$\"$}" v)))

(defn set->str
  "Convert a set of nodes to a LaTeX string."
  [s]
  (string/join ", " (map node->str (sort s))))

(defn map->str
  "Convert a map of vars to vals to a LaTeX string."
  [m]
  (string/join
    ", "
    (map #(str (node->str (first %)) "=" (val->str (second %))) m)))


(defn form->latex
  "Helper function to compile form to a valid LaTeX math string."
  [form]
  (cond
    (:sum form)
    (format "\\left[ \\sum_{%s} %s \\right]"
            (set->str (:sub form))
            (form->latex (:sum form)))

    (:prod form)
    (string/join " " (map form->latex (:prod form)))

    (:numer form)
    (format "\\frac{%s}{%s}"
            (form->latex (:numer form))
            (form->latex (:denom form)))

    (not (empty? (:given form)))
    (format "P(%s \\mid %s)"
            (set->str (:p form))
            (set->str (:given form)))

    (:p form)
    (format "P(%s)" (set->str (:p form)))

    :else
    (error "Unable to compile to LaTeX")))


(defn formula->latex
  [formula]
  (str
    (form->latex (:form formula))
    " \\\\ \\text{where: } "
    (if (:bindings formula)
      (map->str (:bindings formula))
      "\\text{(unbound)}")
    (if (:event formula)
      (format "; %s" (map->str (:event formula))))))


;; Jupyter protocols

(extend-protocol mc/PMimeConvertible
  Model
  (to-mime [this]
    (mc/stream-to-string
      {:text/html (viz/model->img this)})))


(extend-protocol mc/PMimeConvertible
  Formula
  (to-mime [this]
    (mc/stream-to-string
      {:text/latex (str "$$" (formula->latex this) "$$")})))


;; Gorilla protocols

(extend-protocol gr/Renderable
  Model
  (render [self]
    {:type :html
     :content (viz/model->img self)
     :value (pr-str self)}))


(extend-protocol gr/Renderable
  Formula
  (render [self]
    {:type :latex
     :content (formula->latex self)
     :value (pr-str self)}))


