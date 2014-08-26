(ns explore-overtone.recur1
  (:use [overtone.live :except [overtone.music.pitch]]
        [explore-overtone.pitch]
        [oversampler.piano.inst]
        [overtone.synth.stringed]))

;; some original doodling with recursive temporal pattern.

;; globals
(def Ω {:metro (metronome 110)
        :root  (atom 7r40)
        :mode (scale->field :C :major)})

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
;; (map pitchv (range 7r40 7r50))
;; (52 54 55 57 59 60 62) & :e3 is 52

(defn chordv
  "basic chords.  num-notes gives modad, dyad, triad, tetrad, etc,
  correlating to Chords 5th, 7th, 9th, 11th, etc.  Scale degree gives
  the starting note."
  [num-notes degree]
  (deg->chord num-notes (:mode Ω) degree))
(comment
  (dotimes [i 7]
    (let [indexv (first (chordv 3 i))
          cvs    (map #(- % indexv) (chordv 3 i))]
      (println i (chordv 3 i) cvs)))
  )

(defn left-hand-fini
  [beat]
  ;(play (+ 2 beat) sampled-piano (pitchv 7r30) 0.75 4)
  (play (+ 2 beat) sampled-piano (pitchv 7r35) 0.75 4)
  (play (+ 6 beat) sampled-piano (pitchv 7r30) 0.9  8) ;; 7r20 has some wacky delay bug
  (play (+ 6 beat) sampled-piano (pitchv 7r25) 0.9  8))
;;(left-hand-fini ((:metro Ω)))

(defn left-hand
  "beat, pitches, velocities and durations"
  [beat ps vs ds]
  (let [dur (first ds)
        ps  (if (= 0 (mod beat 8))
              ;;(vec (repeatedly 5 (fn [](rand-int 12))))
              (rand-nth [
                         [7 3 5 9 4]
                         [5 4 3 2 1]
                         [1 2 3 4 5]
                         [2 7 5 3 4]
                         ;[5 3]
                         ;[1 2]
                         ;[0]
                         ])
              ps)]
    (when (= 0 (mod beat 8))
      (reset! (:root Ω) (rand-nth (remove #(= @(:root Ω) %) [7r40 7r43 7r44]))))
    (play beat sampled-piano (pitchv (+ @(:root Ω) (first ps))) (first vs) dur)
    ;(play (+ 0.5 beat) sampled-piano (pitchv @(:root Ω)) (first vs) dur)
    ;(left-hand-fini beat)))
    (apply-by ((:metro Ω) (+ beat dur))
              #'left-hand [(+ beat dur) (rotate 1 ps) (rotate 1 vs) (rotate 1 ds)])))

(defn root-chord []
  (let [rt @(:root Ω)
        chords [[0 2 4 7r10 7r12 7r14] ;; major triad
                [0 2 5 7r10 7r12 7r15] ;; m7 triad
                ;;[0 2 4 6 7r12 7r14]
                ]]
    (map #(pitchv (+ rt % -7)) (rand-nth chords))))

(defn rhythm-guitar
  [g beat ds]
  (let [dur (first ds)]
    ;; how to play a random chord on the guitar
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
  (left-hand (next-measure) [7] [0.5 0.25 0.4] [1])
  ;; the guitar is interesting, but not together with left-hand
  (def g1 (guitar :amp 0.2 :pan  0.5 :distort 0.3 :rvb-mix 0.3 :rvb-room 0.8 :lp-freq 8000))
  (def g2 (guitar :amp 0.2 :pan -0.5 :distort 0.5 :rvb-mix 0.3 :rvb-room 0.4 :lp-freq 2000))
  (rhythm-guitar g1 (next-measure) [0.25 0.5])
  (rhythm-guitar g2 (next-measure) [0.25 0.25 1.0])

  (ctl g1 :amp 0.3 :pan  0.5 :distort 0.3 :rvb-mix 0.3 :rvb-room 0.8 :lp-freq 8000)
  (ctl g2 :amp 0.3 :pan -0.5 :distort 0.5 :rvb-mix 0.3 :rvb-room 0.25 :lp-freq 2000)
  (stop)
)
