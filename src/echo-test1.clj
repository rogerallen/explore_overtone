#_(defdeps
    [[org.clojure/clojure "1.3.0"]
     [overtone "0.7.1"]])

(ns echo-test
  (:use [overtone.live]))

(definst bing [freq 440 amp 0.5 decay 0.6 ]
  (* (env-gen (perc 0.01 decay) 1 1 0 1 FREE)
     (sin-osc freq) amp))

(defsynth fx-my-echo
  [bus 0 max-delay 10.0 delay-time 0.1 decay-time 10.0]
  (let [source (in bus)
        echo (comb-c source max-delay delay-time decay-time)]
    (replace-out bus (pan2 (+ echo source) 0))))

(def my-fx (inst-fx! bing fx-my-echo))

(recording-start "/Users/rallen/Documents/Devel/Overtone/rallen/explore_overtone/echo-test1.wav")
(println "echo test")

(def m (metronome 60))

;; decay time of 2 seconds.  hear bing*6 (as expected)
(at (m 1)  (bing :freq 300 :amp 1.0 :decay 0.5))
(at (m 6.5)  (ctl my-fx :decay-time 5.0))
(at (m 7)  (bing :freq 300 :amp 1.0 :decay 0.5))
(at (m 12.5) (ctl my-fx :decay-time 2.0))
(at (m 13) (bing :freq 300 :amp 1.0 :decay 0.5))
(at (m 18.5) (ctl my-fx :decay-time 1.0))
(at (m 19) (bing :freq 300 :amp 1.0 :decay 0.5))

(println "sleeping...")
(Thread/sleep 21000)
(println "done sleeping.")
(recording-stop)
(println "done recording.")






