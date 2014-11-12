(defproject explore_overtone "0.0.0"
  :description  "Explorations while learning Overtone."
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [overtone            "0.10-SNAPSHOT"]
                 [quil                "2.2.2"]
                 [leipzig             "0.8.1" :exclusions [org.clojure/clojure]]
                 ;; careful -- adding this makes oversampler re-download everything
                 [oversampler         "0.3.0" :exclusions [org.clojure/clojure]]
                 [shadertone          "0.2.3"]
                 [persi               "0.2.1"]]
  :jvm-opts     ["-Dfile.encoding=UTF-8"]
  )
