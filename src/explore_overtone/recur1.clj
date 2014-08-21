(ns explore-overtone.recur1
  (:use [overtone.live]
        [oversampler.piano.inst]
        [overtone.synth.stringed]))

;; some original doodling with recursive temporal pattern.

;; globals
(def Ω {:metro (metronome 80)
        :root  (atom 7r40)
        :scale (scale-field :E :major)})

(defn play
  "for synths that have :note, :level and :gate, play a note at a
  certain beat & turn it off after the duration in beats."
  [beat synth pitch level dur]
  (let [cur-synth (at ((:metro Ω) beat) (synth :note pitch :level level))]
    (at ((:metro Ω) (+ beat dur)) (ctl cur-synth :gate 0))))

(defn pitchv
  "given a scale index find midi pitch value
  (hint: use 7r20 for septatonic scale 2nd octave 1st note)"
  [v]
  (nth (:scale Ω) v))
;; (map pitchv (range 7r40 7r50))
;; (52 54 55 57 59 60 62) & :e3 is 52

(defn left-hand
  "beat, pitches, velocities and durations"
  [beat ps vs ds]
  (let [dur (first ds)
        ps  (if (= 0 (mod beat 8))
              ;;(vec (repeatedly 5 (fn [](rand-int 12))))
              (rand-nth [[7 3 5 9 4]
                         [1 2 3 4 5]
                         [2 7 5 3 4]
                         ;[0]
                         ])
              ps)];))
    (when (= 0 (mod beat 8))
      (reset! (:root Ω) (rand-nth (remove #(= @(:root Ω) %) [7r40 7r43 7r44]))))
    (play beat sampled-piano (pitchv (+ @(:root Ω) (first ps))) (first vs) dur)
    (play (+ 0.5 beat) sampled-piano (pitchv @(:root Ω)) (first vs) dur)
    (apply-by ((:metro Ω) (+ beat dur))
              #'left-hand [(+ beat dur) (rotate 1 ps) (rotate 1 vs) (rotate 1 ds)])))

(defn root-chord []
  (let [rt @(:root Ω)
        chords [[0 2 4 7r10 7r12 7r14]
                [0 2 5 7r10 7r12 7r15]
                [0 2 4 6 7r12 7r14]]]
    (map #(pitchv (+ rt % -7)) (rand-nth chords))))

(defn rhythm-guitar
  [g beat ds]
  (let [dur (first ds)]
    (strum-strings {:A [0 0 0 0 0 0] :X [-1 -1 -1 -1 -1 -1]}
                   (root-chord)
                   g
                   (rand-nth [:A :A :A :X :X])
                   (rand-nth [:up :down])
                   0.01 ; (* 6 0.333)
                   ((:metro Ω) beat))
    (apply-by ((:metro Ω) (+ beat dur))
              #'rhythm-guitar [g (+ beat dur) (rotate 1 ds)])))

(defn next-measure []
  (* 4 (metro-bar (:metro Ω))))

(comment
  (left-hand (next-measure) [7 3 5 9 4] [0.5 0.25 0.4] [1])
  (def g1 (guitar :amp 0.2 :pan  0.5 :distort 0.3 :rvb-mix 0.3 :rvb-room 0.8 :lp-freq 8000))
  (def g2 (guitar :amp 0.2 :pan -0.5 :distort 0.5 :rvb-mix 0.3 :rvb-room 0.4 :lp-freq 2000))
  (rhythm-guitar g1 (next-measure) [0.25])
  (rhythm-guitar g2 (next-measure) [1])

  (ctl g1 :amp 0.3 :pan  0.5 :distort 0.3 :rvb-mix 0.3 :rvb-room 0.8 :lp-freq 8000)
  (ctl g2 :amp 0.3 :pan -0.5 :distort 0.5 :rvb-mix 0.3 :rvb-room 0.25 :lp-freq 2000)
  (stop)
)
