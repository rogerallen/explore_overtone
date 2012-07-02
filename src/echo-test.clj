#_(defdeps
    [[org.clojure/clojure "1.3.0"]
     [overtone "0.7.1"]])

(ns echo-test
  (:use [overtone.live])
  (:use [overtone.inst.sampled-piano]))

;; 1/10 delay gives every 10Hz resonance?
(defsynth fx-my-echo
  [bus 0 max-delay 10.0 delay-time 0.1 decay-time 10.0]
  (let [source (in bus)
        echo (comb-c source max-delay delay-time decay-time)]
        ;;echo (allpass-c source max-delay delay-time decay-time)]  -- really quiet ?!?
    (replace-out bus (pan2 (+ echo source) 0))))

(def my-fx (inst-fx! sampled-piano fx-my-echo))

(println "echo test")

(def m (metronome 60))
(recording-start "/Users/rallen/Documents/Devel/Overtone/rallen/explore_overtone/echo-test.wav")

(at (m 1)  (sampled-piano :note 50 :level 0.9 :attack 0.05))
(at (m 6.9)  (ctl my-fx :decay-time 5.0))
(at (m 7)  (sampled-piano :note 50 :level 0.9 :attack 0.05))
(at (m 12.9) (ctl my-fx :decay-time 2.0))
(at (m 13) (sampled-piano :note 50 :level 0.9 :attack 0.05))
(at (m 18.9) (ctl my-fx :decay-time 1.0))
(at (m 19) (sampled-piano :note 50 :level 0.9 :attack 0.05))

(println "sleeping...")
(Thread/sleep 23000)
(println "done sleeping.")
(recording-stop)
(println "done recording.")





