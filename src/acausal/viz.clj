(ns acausal.viz
  (:require [clojure.string]
            [rhizome.viz]
            [clojupyter.protocol.mime-convertible :as mc]))




(extend-protocol mc/PMimeConvertible
  Formula
  (to-mime [this]
    (mc/stream-to-string
      {:text/latex (str "$" (:s this) "$")})))


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


(defn data->latex
  "Query as latex
   TODO: Fix broken"
  [d]
  (Latex.
    (str
      "P("

      (clojure.string/join ", " (map name (:vars d)))
      
      " \\mid "
      "do^*("
      (clojure.string/join ", " (map name (:surrogate d)))
      ")"

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




(defn latex [math-string]
  (Latex. math-string))



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

