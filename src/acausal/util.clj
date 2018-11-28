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

