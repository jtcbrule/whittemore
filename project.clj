(defproject whittemore "0.1.0"
  :description "Causal programming in Clojure"
  :url "https://github.com/jtcbrule/whittemore"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojars.didiercrunch/clojupyter "0.1.5"]
                 [net.mikera/core.matrix "0.62.0"]
                 [better-cond "1.0.1"]
                 [camel-snake-kebab "0.4.0"]
                 [dorothy "0.0.6"]
                 [semantic-csv "0.1.0"]
                 [incanter/incanter-charts "1.9.3"]]
  :plugins [[lein-jupyter "0.1.16"]])
