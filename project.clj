(require 'leiningen.core.eval)

(def LWJGL-CLASSIFIER
  "Per os native code classifier"
  {:macosx  "natives-osx"
   :linux   "natives-linux"
   :windows "natives-windows"})

(defn lwjgl-classifier
  "Return the os-dependent lwjgl native-code classifier"
  []
  (let [os (leiningen.core.eval/get-os)]
    (get LWJGL-CLASSIFIER os)))

(defproject explore_overtone "0.0.0"
  :description "Explorations while learning Overtone."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.trace "0.7.5"]
                 [overtone "0.9.0-SNAPSHOT"]
                 [quil "1.6.0"]
                 [oversampler "0.3.0"]
                 [leipzig "0.5.0-SNAPSHOT"]
                 [org.lwjgl.lwjgl/lwjgl "2.8.5"]
                 [org.lwjgl.lwjgl/lwjgl_util "2.8.5"]
                 [org.lwjgl.lwjgl/lwjgl-platform "2.8.5"
                  :classifier    ~(lwjgl-classifier)
                  :native-prefix ""]
                 [shadertone "0.1.0"]
                 [persi "0.1.0-SNAPSHOT"]]
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  )
