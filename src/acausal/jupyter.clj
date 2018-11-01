(ns acausal.jupyter
  (:require [acausal.core]))


(comment

(intern 'acausal.jupyter 'model acausal.core/model)

;; see also
;; https://github.com/overtone/overtone/blob/master/src/overtone/helpers/ns.clj

(model
  {:x [:y]
   :y []})

(defmacro defmodel [& args]
  `(a/defmodel ~@args))

(def model a/model)

(def query a/query)
(def q query)

(def data a/data)
(def p data)

(def identify a/identify)

)

