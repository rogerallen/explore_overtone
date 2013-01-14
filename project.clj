(defproject explore_overtone "0.0.0"
  :description "Explorations while learning Overtone."
  :dependencies [ [org.clojure/clojure "1.3.0"]
                  [org.clojure/tools.trace "0.7.3"]
                  [overtone "0.8.0-RC15"]
                  [quil "1.6.0"]
                  [oversampler "0.2.0-SNAPSHOT"]
                  [leipzig "0.2.0-RALLEN"]
                  ]
  :plugins [ [lein-swank "1.4.4"] ]
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  )
