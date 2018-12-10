(ns whittemore.graphviz
  (:require [whittemore.util :refer [warn]]
            [clojure.data.codec.base64 :as b64]
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
  "<svg width='300' height='30'
  xmlns='http://www.w3.org/2000/svg'
  xmlns:xlink='http://www.w3.org/1999/xlink'>
  <text x='0' y='15' font-family='monospace' font-size='14px'>
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


;; UNUSED
;; Note that HTML label subscripts do not render well with SVG
;; Future code may optionally use dot2tex --texmode=math
(defn- format-keyword
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


(def img-str
  "<img src=\"data:image/svg+xml;base64,%s\">")

(defn svg->img
  "Returns the svg formatted as an html img tag."
  [svg]
  (format img-str
          (String. (b64/encode (.getBytes svg "UTF-8")) "UTF-8")))

(defn model->img
  "Convert a model to an img tag."
  [m]
  (-> m model->dot dot->svg svg->img))


(defn view-model
  "Alpha - subject to change.
  Render and view a model, displaying it in a JFrame.
  Requires \"dot\" to be in PATH, i.e. graphviz must be installed."
  [m]
  (-> m model->dot dot/show!))


;; UNUSED
(defn dot->png
  "Render dot spec as png.
  Requires \"dot\" to be in PATH, i.e. graphviz mus be installed."
  [dot-str]
  (dot/render dot-str {:format :png}))


;; alternative Jupyter protocols for rendering models
(comment

(extend-protocol clojupyter.protocol.mime-convertible/PMimeConvertible
  whittemore.core.Model
  (to-mime [this]
    (clojupyter.protocol.mime-convertible/stream-to-string
      {:image/png (-> this
                      whittemore.graphviz/model->dot
                      whittemore.graphviz/dot->png)})))

(extend-protocol mc/PMimeConvertible
  Model
  (to-mime [this]
    (mc/stream-to-string
      {:image/svg+xml (viz/model->svg this)})))

)

