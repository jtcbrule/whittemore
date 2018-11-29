(ns acausal.plot
  (:require [acausal.util :refer [error]]
            [clojupyter.protocol.mime-convertible :as mc]
            [incanter.charts :as charts])
  (:import [org.jfree.chart JFreeChart ChartFrame]))


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
      (.setSize (:width *chart-size*) (:height *chart-size*))
      (.setVisible true))
    frame))


(defn- sortable-map?
  [m]
  (every? #(instance? java.lang.Comparable %)
          (keys m)))


(defn plot-pmf
  "Returns a plot of the pmf, a map of categories to probability."
  [pmf]
  (let [m (cond
            (sorted? pmf) pmf
            (sortable-map? pmf) (into (sorted-map) pmf)
            :else pmf)
        k (map str (keys m))
        v (vals m)
        bar (charts/bar-chart k v :x-label "" :y-label "")]
    (do
      (-> bar .getCategoryPlot .getRangeAxis (.setUpperBound 1.0))
      bar)))


;; test code

(comment

(view-plot
  (plot-pmf {:a 0.3 :b 0.2 :c 0.5}))

(view-plot
  (plot-pmf {:b 0.3 :a 0.2 :c 0.5}))

(view-plot
  (plot-pmf {{:a 0} 0.3 {:a 1} 0.7}))

)

