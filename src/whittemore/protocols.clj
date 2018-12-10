(ns whittemore.protocols)

(defprotocol Distribution
  (estimate-distribution [distribution formula options])
  (measure-probability [distribution event options])
  (signature [distribution]))

;; TODO: add protocols for summary statistics

