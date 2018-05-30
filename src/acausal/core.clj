(ns acausal.core
  (:require [clojure.string]
            [rhizome.viz]
            [clojupyter.protocol.mime-convertible :as mc]))


(defn transpose
  "Compute the transpose of directed graph g
   Returns a map of sets (adjacency list representation)
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

(def q "Alias for query" query)


(deftype SVG [s]

  mc/PMimeConvertible
  (to-mime [_]
    (mc/stream-to-string
      {:image/svg+xml s})))


(defn model->svg
  "Model m as SVG
  TODO: refactor dot and rendering steps, better label formatting"
  [m]
  (SVG.
    (rhizome.viz/graph->svg
      (keys (:children m))
      (:children m)
      :vertical? false
      :node->descriptor (fn [n]
                          (if (contains? (:latents m) n)
                            {:label "", :shape "none", :width 0, :height 0}
                            {:label (name n)})) ; :shape "circle"
      :edge->descriptor (fn [i j]
                          (if (contains? (:latents m) i)
                            {:style "dotted"}
                            {})))))



;; LaTeX rendering

(defrecord Latex [math])

(extend-protocol mc/PMimeConvertible
  Latex
  (to-mime [this]
    (mc/stream-to-string
      {:text/latex (str "$" (:math this) "$")})))



;; TODO: figure out how to distinguish information sets from queries
(defn query->latex
  "Query as latex
   TODO: fix broken string stuff"
  [q]
  (Latex. (str
            "P("

            (clojure.string/join ", " (map name (:effect q)))

            " \\mid "
            
            "do("
            (clojure.string/join ", " (map name (:do q)))

            ")"

            (if (empty? (:given q))
              ""
              (str ", " (clojure.string/join ", " (map name (:given q)))))

            ")")))



;; For use outside Jupyter

(defn view-model
  "View model m
  TODO: refactor dot and rendering steps, better label formatting"
  [m]
  (rhizome.viz/view-graph
    (keys (:children m))
    (:children m)
    :vertical? false
    :node->descriptor (fn [n]
                        (if (contains? (:latents m) n)
                          {:label "", :shape "none", :width 0, :height 0}
                          {:label (name n)})) ; :shape "circle"
    :edge->descriptor (fn [i j]
                        (if (contains? (:latents m) i)
                          {:style "dotted"}
                          {}))))



;; Note that it's possible to seperate jupyter code from core
;; TODO: importing acausal.jupyter should auto-pretty print models, queries, etc
;; e.g. acausal.viz

(comment

(ns acausal.viz
  (:require [acausal.core]
    [clojupyter.protocol.mime-convertible :as mc]))

(extend-protocol mc/PMimeConvertible
  acausal.core.Latex
  (to-mime [this]
    (mc/stream-to-string
      {:text/latex (str "$" (:s this) "$")})))
                         
)


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
