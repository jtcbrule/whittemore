(ns acausal.plot
  "Plotting; currently wrappers around incanter.charts functions."
  (:require [clojupyter.protocol.mime-convertible :as mc]
            [incanter.core :refer [view save]]
            [incanter.charts :as chart])
  (:import [org.jfree.chart JFreeChart]))


;; TODO: remove? (import from acausal.core?)
(defmacro error
  "Throws a RuntimeException with optional additional data."
  [msg & keyvals]
  `(throw (ex-info (str ~msg) (hash-map ~@keyvals))))

(def ^:dynamic *chart-size* {:width 500, :height 310})

(defn set-chart-size!
  "Set the width and height for rendered JFreeChart."
  [width height]
  (alter-var-root #'*chart-size*
                  (fn [_] {:width width, :height height})))

(extend-protocol mc/PMimeConvertible
  JFreeChart
  (to-mime [this]
    (mc/to-mime (.createBufferedImage
                  this (:width *chart-size*) (:height *chart-size*)))))


;; TODO: refactor
;; Add options for title
(defn plot-univariate-categorical
  "Returns a chart, given a map of vars to probability."
  [m]
  (let [k (keys m)
        v (vals m)
        bar (chart/bar-chart k v)]
    (do
      (-> bar .getCategoryPlot .getRangeAxis (.setUpperBound 1.0))
      bar)))


;; TODO: refactor as protocol?
;; only for categorical at moment
;; FIXME: inefficient
(defn plot-distribution
  "Returns a chart, plotting a distribution."
  [distribution]
  (let [pmf (:pmf distribution)
        dimension (-> pmf keys first keys count)]
    (if (not= dimension 1)
      (error "Unsupported: cannot currently plot multivariate categorical")
      (let [rv (-> pmf keys first keys first)
            flattened-pmf (into (sorted-map)
                            (for [[k v] pmf]
                              [(rv k) v]))]
        (plot-univariate-categorical flattened-pmf)))))


(comment

(def tmp {:a 0.2 :b 0.5 :c 0.3})

(view
  (plot-univariate-categorical tmp))

(-> tmp plot-categorical .getCategoryPlot .getRangeAxis (.setUpperBound 1.0))

(.getCategoryPlot (plot-categorical tmp))

(view
  (incanter.charts/bar-chart [:a :b :c] [1 2 3]
                             :title "HI"))

(def x (vec (range 10 50)))
(def y (vec (map #(* % %) x)))

(view
      (incanter.charts/xy-plot x y))

(view
      (incanter.charts/scatter-plot x y))


)
