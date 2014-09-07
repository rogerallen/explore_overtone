(defproject explore_overtone "0.0.0"
  :description "Explorations while learning Overtone."
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [quil                "2.2.1"]
                 [overtone            "0.9.1"]
                 [leipzig             "0.7.0" :exclusions [org.clojure/clojure]]
                 [oversampler         "0.3.0" :exclusions [org.clojure/clojure]]
                 [shadertone          "0.2.3"]
                 [persi               "0.2.0"]]
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  )
