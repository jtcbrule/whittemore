(ns acausal.io
  "Alpha - subject to change."
  (:require [acausal.html-table] ; Clojupyter protocol
            [acausal.util :refer [map-vals]]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [clojure.core.matrix.dataset :as md]
            [clojure.java.io :as io]
            [semantic-csv.core :as sc]))


(def cast-fns
  {Long sc/->long,
   Double sc/->double,
   Float sc/->float,
   Integer sc/->int})

(defn- types->cast-fns
  [types]
  (map-vals #(get cast-fns % identity) types))


(defn read-csv
  "Alpha - subject to change.
  Reads CSV data into a core.matrix dataset.

  Specify :types as a map from columns to types,
  e.g. :types {:foo Long, :bar Double}, default type is String.

  Other options are passed to semantic-csv/process
  Note that :cast-fns will override :types.

  See also incanter.io/read-dataset"
  [filepath & {:as options}]
  (let [defaults {:comment-re #"^$|^\#" ; skip blank lines and comments
                  :cast-fns (if (:types options)
                              (types->cast-fns (:types options))
                              nil)}
        options (->> (dissoc options :types)
                     (into defaults)
                     (apply concat))]
    (with-open [reader (io/reader filepath)]
      (let [data (transform-keys ->kebab-case-keyword
                   (apply sc/parse-and-process reader options))
            col-names (-> (first data) keys sort)]
        (md/dataset col-names data)))))


(defn head
  "Alpha - subject to change.
  Returns the first part of a dataset (default 10)."
  [dataset & {:keys [n] :or {n 10}}]
  (md/dataset (take n (md/row-maps dataset))))


(defn tail
  "Alpha - subject to change.
  Returns the last part of a dataset (default 10)."
  [dataset & {:keys [n] :or {n 10}}]
  (md/dataset (take-last n (md/row-maps dataset))))

