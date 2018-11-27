(ns acausal.graphviz
  (:require [acausal.util :refer [warn]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [dorothy.core :as dot]))


;; attempt fallback to viz-cljc if Graphviz is not installed
(declare dot-renderer)

(try
  (sh "dot" "-V")
  (def dot-renderer :dorothy)
  (catch Exception e
    (def dot-renderer :viz-cljc)))


(defn error-svg [_]
  "<svg width='300' height='30'>
  <text x='0' y='15' font-family='monospace'>
  ERROR: unable to render SVG
  </text>
  </svg>")


(def dot->svg
  "Returns an SVG string that renders the given grapvhiz string."
  (if (= dot-renderer :dorothy)
    (fn [dot-str]
      (@(ns-resolve 'dorothy.core 'render) dot-str {:format :svg}))
    (do
      (warn "WARNING: \"dot\" (Graphviz) not found in PATH,"
            "attempting fallback to viz-cljc")
      (try
        (require '[viz.core])
        @(ns-resolve 'viz.core 'image)
        (catch Exception e
          (warn "WARNING: \"viz-cljc\" not found in CLASSPATH,"
                "unable to render SVG")
          error-svg)))))


;; NOTE: unused
;; HTML label subscripts do not render well with SVG
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


(defn model->svg
  "Convert a model as an svg string."
  [m]
  (-> m model->dot dot->svg))


(defn view-model
  "Render and view a model, displaying it in a JFrame.
  
  Requires \"dot\" to be in PATH, i.e. graphviz must be installed."
  [m]
  (-> m model->dot dot/show!))


;; NOTE: unused
(defn dot->png
  "Render dot spec as png."
  [dot-str]
  (dot/render dot-str {:format :png}))


;; alternative Jupyter protocol for rendering models as png
(comment

(extend-protocol clojupyter.protocol.mime-convertible/PMimeConvertible
  acausal.core.Model
  (to-mime [this]
    (clojupyter.protocol.mime-convertible/stream-to-string
      {:image/png (-> this
                      acausal.graphviz/model->dot
                      acausal.graphviz/dot->png)})))

)

