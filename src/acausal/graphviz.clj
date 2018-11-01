(ns acausal.graphviz
  (:require [clojure.string :as string]
            [dorothy.core :as dot]))


;; TODO: fallback to viz.cljc
(defn dot->svg
  "Render dot spec as svg."
  [dot-str]
  (dot/render dot-str {:format :svg}))


;; NOTE: unused; subscripts do not render well with SVG
;; Consider using dot2tex --texmode=math instead
(defn format-keyword
  "Returns an html subscripted string, given a keyword with single underscore."
  [kword]
  (let [s (string/split (name kword) #"_")]
    (if (= (count s) 2)
      (str "<FONT>" (first s) "<SUB>" (second s) "</SUB>" "</FONT>")
      (name kword))))


(def digraph-options
  {:rankdir "LR"})

(def bi-options
  {:style "dashed", :dir "both", :arrowhead "empty", :arrowtail "empty",
   :constraint "true"})

(defn model->dot
  "Convert a model to a dot spec."
  [m]
  (dot/dot
    (dot/digraph
      (concat
        [digraph-options]
        ; nodes (unused, but can add node options)
        (for [n (keys (:pa m))]
          [n])
        ; edges from parents to children
        (for [[ch pa-set] (:pa m)
              pa pa-set]
          [pa ch])
        ; bidirectional edges
        (for [pair (:bi m)
              :let [pair (sort pair)]]
          [(first pair) (second pair) bi-options])))))


(defn view-model
  "View a model.
  
  Requires dot to be on your PATH, i.e. install graphviz."
  [m]
  (-> m model->dot dot/show!))


(defn model->svg
  "Convert a model as an svg string."
  [m]
  (-> m model->dot dot->svg))


;; NOTE: unused
(defn dot->png
  "Render dot spec as png."
  [dot-str]
  (dot/render dot-str {:format :png}))


; from acausal.core
(comment

(extend-protocol mc/PMimeConvertible
  Model
  (to-mime [this]
    (mc/stream-to-string
      {:image/png (-> this viz/model->dot viz/dot->png)})))

)


(comment

(require '[acausal.core :refer [model]])

(def napkin
  (model
    {:z_1 [:z_2]
     :x [:z_1]
     :y [:x]
     :z_2 []}
    #{:x :z_2}
    #{:z_2 :y}))

(def wainer
  (model
    {:z_0 []
     :b [:z_0]
     :z_1 [:z_0]
     :x [:z_0]
     :z_2 [:z_1 :x]
     :z_3 [:z_2 :b]
     :y [:x :z_2 :z_3]}))


)

