(defproject explore_overtone "0.0.0"
  :description "Explorations while learning Overtone."
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.trace "0.7.3"]
                 [overtone "0.8.0"]
                 [quil "1.6.0"]
                 [oversampler "0.3.0"]
                 [leipzig "0.2.0"]]
  :plugins [ [lein-swank "1.4.4"] ]
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  )
