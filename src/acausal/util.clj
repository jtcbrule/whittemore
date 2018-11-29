(ns acausal.util)

(defmacro error
  "Throws a RuntimeException with optional additional data."
  [msg & keyvals]
  `(throw (ex-info (str ~msg) (hash-map ~@keyvals))))


(defn warn
  "Print the warning(s) to *err*."
  [& more]
  (binding [*out* *err*]
    (apply print more)
    (println)))


(defn map-vals
  "Map f over the values of map m, returning a new map."
  [f m]
  (reduce-kv
    (fn [m k v]
      (assoc m k (f v)))
    {}
    m))

