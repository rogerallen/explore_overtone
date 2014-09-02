(ns explore-overtone.morse
  [:use [overtone.live]])

(def metro (metronome 140))

(defsynth morse
  [duration 1.0 out-bus 0]
  (let [env  (env-gen (lin 0.01 (- duration 0.02) 0.01) :action FREE)
        osc  (sin-osc 800)]
    (out out-bus (pan2 (* 0.5 env osc)))))

(defn play-morse
  [beat dur]
  (let [dur-s (* dur (/ (metro-tick metro) 1000))]
    (at (metro beat) (morse :duration dur-s))))

(defn seq-morse
  [beat ds]
  (when-not (empty? ds)
    (let [dur (if (= (first ds) :dot) 0.25 0.75)]
      (play-morse beat dur)
      (apply-by (metro (+ beat dur))
                #'seq-morse [(inc beat) (rest ds)]))))

(comment
  (recording-start "morse.wav")
  (seq-morse (metro) [:dot :dash :dot :dot :dash])
  (recording-stop)
  )
