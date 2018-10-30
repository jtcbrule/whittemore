(ns acausal.html-table
  (:require [clojure.core.matrix.dataset :refer [row-maps]]
            [clojure.core.matrix.impl.dataset]
            [clojure.string :refer [join]]
            [clojupyter.protocol.mime-convertible :as mc]))


(defn dataset->html
  "Returns an HTML table representation of dataset d."
  [d]
  (let [ks (:column-names d)]
    (str
      "<table class=\"dataframe\">\n"
      "<tr>"
      (join
        (for [k ks]
          (str "<th>"
               (if (string? k)
                 (str "\"" k "\"")
                 k)
               "</th>")))
      "</tr>\n"
      (join
        (for [row (row-maps d)]
          (str "<tr>"
               (join
                 (for [e (map row ks)]
                   (str "<td>"
                        (if (string? e)
                          (str "\"" e "\"")
                          e)
                        "</td>")))
               "</tr>\n")))
      "</table>\n")))


(extend-protocol mc/PMimeConvertible
  clojure.core.matrix.impl.dataset.DataSet
  (to-mime [this]
    (mc/stream-to-string
      {:text/html (dataset->html this)})))

