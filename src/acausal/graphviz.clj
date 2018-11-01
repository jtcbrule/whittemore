(ns acausal.graphviz
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [dorothy.core :as dot]))

(declare dot-renderer)

(try
  (sh "nodot" "-V")
  (def dot-renderer :dorothy)

  (catch Exception e
    (def dot-renderer :viz-cljc)))

(defn warn! [msg] (.println *err* msg))

;
(def dot->svg
  (if (= dot-renderer :dorothy)
    (ns-resolve 'dorothy.core 'render)
    (do
      (warn! "WARNING: dot not in PATH")
      ;; try catch, warning if fail, rebind as empty, plus warning on call
      (require '[viz.core])
      (ns-resolve 'viz.core 'image))))



(comment
(defn dot->svg
  [dot-str]
  (dot/render dot-str {:format :svg}))
)

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

(model->svg napkin)


(spit "tmp.svg" (viz.core/image (model->dot wainer)))

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

