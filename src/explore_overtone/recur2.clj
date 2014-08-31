(ns explore-overtone.recur1
  (:use [overtone.live :except [overtone.music.pitch]]
        [explore-overtone.pitch]
        [oversampler.piano.inst]))

(def Ω {:metro (metronome 110)
        :root  (atom 7r40)
        :incr  (atom 3)
        :mode  (scale->field :C :major)})

(defn play
  "for synths that have :note, :level and :gate, play a note at a
  certain beat & turn it off after the duration in beats."
  [beat synth pitch level dur]
  (let [cur-synth (at ((:metro Ω) beat) (synth :note pitch :level level))]
    (at ((:metro Ω) (+ beat dur)) (ctl cur-synth :gate 0))))

(defn pitchv
  "given a scale degree find midi pitch value from the global scale
  (hint: use base 7 for common scales 7r20=2nd octave 1st note)"
  [degree]
  (deg->pitch (:mode Ω) degree))

(defn constrain
  [minv maxv v]
  (if (> v maxv)
    (- v 7r10)
    (if (< v minv)
      (+ v 7r10)
      v)))

(defn left-hand
  [beat ps vs ds]
  (let [dur (first ds)
        ps (if (= 0 (mod beat 16))
             (rand-nth [[2 4 6 4 6 2 4 6]
                        [2 2 4 4 6 6 8 8]
                        [6 6 4 4 6 2 6 2]
                        ])
             ps)]
    (when (= 0 (mod beat 8))
      (swap! (:root Ω) (fn [v] (constrain 7r30 7r40 (+ v @(:incr Ω))))))
    (play beat sampled-piano (pitchv @(:root Ω)) (first vs) dur)
    (play (+ 0.667 beat) sampled-piano (pitchv (+ @(:root Ω) (first ps))) (first vs) dur)
    (play (+ 0.333 beat) sampled-piano (pitchv (+ @(:root Ω) (+ 2 (first ps)))) (first vs) dur)
    ;;(play (+ 0.5 beat) sampled-piano (pitchv @(:root Ω)) (first vs) dur)
    ;;(play (+ 0.75 beat) sampled-piano (pitchv @(:root Ω)) (first vs) dur)
    (apply-by ((:metro Ω) (+ beat dur))
              #'left-hand [(+ beat dur) (rotate 2 ps) (rotate 1 vs) (rotate 1 ds)])))

;; melody stuff is a bit weak, but oh well...
(defn right-hand
  [beat ps vs ds]
  (let [dur (first ds)
        ps  [(constrain 7r50 7r100 (+ (first ps) (- (rand-int 4) 2)))]]
    (play beat sampled-piano (pitchv (first ps)) (first vs) dur)
    (apply-by ((:metro Ω) (+ beat dur))
              #'right-hand [(+ beat dur) (rotate 2 ps) (rotate 1 vs) (rotate 1 ds)])))

(defn next-measure []
  (* 4 (metro-bar (:metro Ω))))

(comment

  (left-hand (next-measure) [2 4 6 4 6 2 4 6] [0.3] [1])
  (right-hand (next-measure) [1] [0.8] [2 1 1 4])
  (reset! (:incr Ω) 3)
  (reset! (:root Ω) 7r40)

  @(:root Ω)
  @(:incr Ω)

  (stop)
)
