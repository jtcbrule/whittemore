(ns whittemore.plot
  (:require [whittemore.core :refer [estimate q]]
            [whittemore.util :refer [error]]
            [clojupyter.protocol.mime-convertible :as mc]
            [incanter.charts :as charts])
  (:import (org.jfree.chart JFreeChart ChartFrame)))


(def ^:dynamic *plot-size* {:width 500, :height 310})

(defn set-plot-size!
  "Set the width and height for rendered plot."
  [width height]
  (alter-var-root #'*plot-size*
                  (fn [_] {:width width, :height height})))

(extend-protocol mc/PMimeConvertible
  JFreeChart
  (to-mime [this]
    (mc/to-mime (.createBufferedImage
                  this (:width *plot-size*) (:height *plot-size*)))))

(defn view-plot
  "View a plot in a new window."
  [chart]
  (let [frame (ChartFrame. "JFreeChart" chart)]
    (doto frame
      (.setSize (:width *plot-size*) (:height *plot-size*))
      (.setVisible true))
    frame))


(defn- sortable-map?
  [m]
  (every? #(instance? java.lang.Comparable %)
          (keys m)))


(defn plot-p-map
  "Alpha - subject to change.
  Returns a plot of the p-map, a map of categories to probabilities."
  [p-map]
  (let [m (cond
            (sorted? p-map) p-map
            (sortable-map? p-map) (into (sorted-map) p-map)
            :else p-map)
        k (map str (keys m))
        v (vals m)
        bar (charts/bar-chart k v :x-label "" :y-label "")]
    (do
      (-> bar .getCategoryPlot .getRangeAxis (.setUpperBound 1.0))
      bar)))


(defn marginal-pmf
  "Alpha - subject to change.
  Returns the marginal distribution of variable x from multivariate pmf,
  where pmf is a map of (map of variable to value) to probability."
  [pmf x]
  (let [f (fn [m k v]
            (merge-with + m {(get k x) v}))]
   (reduce-kv f {} pmf)))


;; TODO: cleanup / refactor as protocol?
(defn plot-univariate
  "Alpha - subject to change.
  Plot a univariate probability distribution."
  ([distribution]
   (let [dist (if (:samples distribution) ; EmpiricalCategorical
                (estimate distribution (q (-> distribution :support keys)))
                distribution)
         dimension (-> dist :pmf keys first count)
         variable (-> dist :pmf keys first keys first)]
     (if (not= dimension 1)
       (error "dimension of pmf is not 1, unable to plot")
       (plot-univariate dist variable))))
  ([distribution variable]
   (let [dist (if (:samples distribution) ; EmpiricalCategorical
                 (estimate distribution (q (-> distribution :support keys)))
                 distribution)]
   (plot-p-map (marginal-pmf (:pmf dist) variable)))))


;; test code

(comment

(view-plot
  (plot-p-map {:a 0.3 :b 0.2 :c 0.5}))

(view-plot
  (plot-p-map {:b 0.3 :a 0.2 :c 0.5}))

(view-plot
  (plot-p-map {{:a 0} 0.3 {:a 1} 0.7}))

)

