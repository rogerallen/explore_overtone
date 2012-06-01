(defproject explore_overtone "0.0.0"
  :description "Explorations while trying to learn overtone."
  :repositories {"local" ~(str (.toURI (java.io.File. "maven_repository")))}
  :dependencies [ [org.clojure/clojure "1.3.0"]
                  [org.clojure/tools.trace "0.7.3"]
                  ;;[overtone "0.6.0"]
                  [overtone "0.7.0-LOCALSNAPSHOT" ]
                  ]
  :plugins [ [lein-swank "1.4.4"] ]
  )
