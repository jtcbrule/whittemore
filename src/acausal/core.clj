(ns acausal.core
  (:refer-clojure :exclude [ancestors parents])
  (:require [acausal.viz :as viz]
            [better-cond.core :as b]
            [clojupyter.protocol.mime-convertible :as mc]
            [clojure.core.matrix.dataset :refer [row-maps dataset]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set :refer [difference intersection subset? union]]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))


(defmacro error
  "Throws a RuntimeException with optional additional data."
  [msg & keyvals]
  `(throw (ex-info (str ~msg) (hash-map ~@keyvals))))


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
  "Returns a new model from dag with confounding."
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



;; TODO: require :given #{vars} to be non-empty?
;; A formula is a recursive type of:
;; :prod #{formulas}
;; :sum formula, :sub #{vars}
;; :numer formula, :denom formula
;; :p #{vars}, :given #{vars}
;; :p #{vars}
(defrecord Formula [])

;; hedge structure
;; g is a model that is a c-component
;; s is a subset of vertices such that G, G \intersect S is a hedge
;; TODO: Hedges should be renderable (highlight the subset?)
(defrecord Hedge [g s])


(defn formula?
  "Returns true iff f is a Formula."
  [f]
  (instance? acausal.core.Formula f))


;; TODO: optimizations (collapse nested sums)
(defn sum
  "Returns \\sum_{sub} p"
  [sub p]
  (if (empty? sub)
    p
    {:sub sub :sum p}))


;; TODO: optimizations
(defn product
  "Returns \\prod_i p_i.
  coll is a collection of probability functions."
  [coll]
  (let [exprs (set coll)]
    (if (= (count exprs) 1)
      (first exprs)
      {:prod exprs})))


(defn fail [g s]
  {:hedge [g s]})


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
    (error "(free ...) preconditions failed")))


;; TODO: refactor, opportunity for optimizations
;; Note: this is intricate, because we're calculating P(vi \\mid pi)
;; in terms of the probability distribution p
(defn given-pi
  "Returns P(vi \\mid v_{pi}^(i-1))
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
  "Walk the formula and return the set of hedges it refers to."
  [form]
  (cond
    (:hedge form)
    (hash-set (apply ->Hedge (:hedge form)))

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


(defrecord Data [joint surrogate])

(defn data
  "Returns a representation of the known joint probability function.
  i.e. P(v | do(z')) \\forall z' \\subseteq z"
  [v & {:keys [do*] :or {do* []}}]
  (->Data (set v) (set do*)))

(def p
  "Alias for acausal.core/data"
  data)


;; TODO: implement (identify m q d)
;; NOTE returns either a Formula or a Hedge
(defn identify
  "Returns a formula that computes query q from data d in model m.
  Data defaults to P(v)."
  ([m q]
   (let [form (id (:p q) (:do q) {:p (vertices m)} m)
         hedges (extract-hedges form)]
     (if (empty? hedges)
       (into (->Formula) form)
       (first hedges))))
  ([m q d]
   (if (and (= (:joint d) (vertices m))
            (empty? (:surrogate d)))
     (identify m q)
     (error "Unimplemented"))))


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


;; TODO: refactor (especially the :given)
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

    ;; refactor?
    (not (empty? (:given formula)))
    (format "P(%s \\mid %s)"
            (set->str (:p formula))
            (set->str (:given formula)))

    (:p formula)
    (format "P(%s)" (set->str (:p formula)))

    :else
    (error "Unable to compile to LaTeX")))


;; Distribution protocol idea:
;; support a sample/row function
;; support a support function
;; sum over?
;; TODO: how to supply 'support' information?

(defn read-csv-dataset
    "Reads CSV-data into a core.matrix dataset"
    [filename]
    (with-open [reader (io/reader filename)]
        (let [data (csv/read-csv reader)]
            (dataset (map keyword (first data)) (rest data)))))

(def data (assoc (read-csv-dataset "kidney.csv")
                 :support {:success ["yes" "no"]
                           :treatment ["surgery" "nephrolithotomy"]
                           :size ["small" "large"]}))


;; TODO: refactor, test
;; Unify queries and the {:p ...} terms?
;; broken if there are unbound variables in assignments.
(defn eval-prob
  ""
  [rows prob assignments]
  (loop [samples rows, matching 0, total 0]
    (cond
      (nil? (first samples))
      (/ matching total)

      ; :given doesn't match? skip
      (not= (select-keys (first samples) (:given prob))
            (select-keys assignments (:given prob)))
      (recur (rest samples) matching total)
      
      ; doesn't match the :p? increase total, but continue
      (not= (select-keys (first samples) (:p prob))
            (select-keys assignments (:p prob)))
      (recur (rest samples) matching (inc total))

      ; matches all? inc matching and total
      :else
      (recur (rest samples) (inc matching) (inc total)))))


;; TODO: Fix (is broken)
;; TODO: turn this into a protocol?
;; explicit strategy argument?
;; broken if assignments leaves variables in formula unbound
(defn estimate
  [distribution formula assignments]
  (let [rows (row-maps distribution)
        support (:support distribution)]
    (cond
      (:p formula)
      (eval-prob rows formula assignments)

      (:prod formula)
      (reduce *
              (map #(estimate distribution % assignments)
                   (:prod formula)))

      (:sum formula)
      (error "Unimplemented")
      ; instatiation all assignments, send them into the 
      ; current assignments, recur, add them up
      ; also, need to support :numer :denom
      
      
      :else
      (error "Unsupported formula type"))))




;; Jupyter integration
;; TODO: seperate into new namespace?
;; TODO: render Query and Data types? (probably not)
;; TODO: render Hedge

(extend-protocol mc/PMimeConvertible
  Model
  (to-mime [this]
    (mc/stream-to-string
      {:image/svg+xml (viz/model->svg this)})))


;; TODO: fix this hack
(defn trim-brackets
  [s]
  (if (and (string/starts-with? s "\\left[")
           (string/ends-with? s "\\right]"))
    (subs s 7 (- (count s) 8))
    s))


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



;; Example models
;; TODO: move to test namespace (but keep here for dev)

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

(view-model m4)

(ancestors m4 [:z])

(view-model hedge-less)

(c-components hedge-less)

(view-model kidney)
(view-model ident-b)
(view-model ident-g)

(topological-sort ident-g)

(id #{:y} #{:x} {:p (vertices ident-a)} ident-a)

(identify ident-a (q [:y] :do [:x]))
(identify ident-b (q [:y] :do [:x]))
(identify ident-c (q [:y] :do [:x]))
(identify ident-d (q [:y] :do [:x]))
(identify ident-e (q [:y] :do [:x]))
(identify ident-f (q [:y] :do [:x]))
(identify ident-g (q [:y] :do [:x]))

(identifiable? ident-a (q [:y] :do [:x]))
(identifiable? ident-b (q [:y] :do [:x]))
(identifiable? ident-c (q [:y] :do [:x]))
(identifiable? ident-d (q [:y] :do [:x]))
(identifiable? ident-e (q [:y] :do [:x]))
(identifiable? ident-f (q [:y] :do [:x]))
(identifiable? ident-g (q [:y] :do [:x]))


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

(->
  (identify napkin (q [:y] :do [:x]))
  formula->latex)


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

(identifiable? non-a (q [:y] :do [:x]))
(identifiable? non-b (q [:y] :do [:x]))
(identifiable? non-c (q [:y] :do [:x]))
(identifiable? non-d (q [:y] :do [:x]))
(identifiable? non-e (q [:y] :do [:x]))
(identifiable? non-f (q [:y] :do [:x]))
(identifiable? non-g (q [:y] :do [:x]))
(identifiable? non-h (q [:y] :do [:x]))


)

;; TODO: document
(defn cross-pairs
  [coll1 coll2]
  (for [i coll1
        j coll2
        :when (not= i j)]
    #{i j}))


;; TODO: refactor, check implementation, efficiency (transients?)
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

(view-model (latent-projection tmp [:w :x]))

  (view-model tmp)
)


