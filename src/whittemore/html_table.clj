(ns whittemore.html-table
  (:require [clojure.core.matrix.dataset :refer [row-maps column-names]]
            [clojure.core.matrix.impl.dataset]
            [clojure.string :refer [join]]
            [clojupyter.protocol.mime-convertible :as mc]
            [gorilla-renderable.core :as gr]))

(defn to-string
  "Custom to-string for dataset html-table.
  Strings are wrapped with double quotes; nils are 'nil'."
  [o]
  (cond
    (string? o) (str "\"" o "\"")
    (nil? o) "nil"
    :else (str o)))

(defn dataset->html-str
  "Returns an HTML table representation of dataset d."
  [d]
  (let [ks (column-names d)]
    (str
      "<table class=\"dataframe\">\n"
      "<thead>\n"
      "<tr>"
      (join
        (for [k ks]
          (str "<th>"
               (to-string k)
               "</th>")))
      "</tr>\n"
      "</thead>\n"
      "<tbody>\n"
      (join
        (for [row (row-maps d)]
          (str "<tr>"
               (join
                 (for [e (map row ks)]
                   (str "<td>"
                        (to-string e)
                        "</td>")))
               "</tr>\n")))
      "</tbody>\n"
      "</table>\n")))


(extend-protocol mc/PMimeConvertible
  clojure.core.matrix.impl.dataset.DataSet
  (to-mime [this]
    (mc/stream-to-string
      {:text/html (dataset->html-str this)})))


(extend-protocol gr/Renderable
  clojure.core.matrix.impl.dataset.DataSet
  (render [self]
    {:type :html
     :content (dataset->html-str self)
     :value (pr-str self)}))

