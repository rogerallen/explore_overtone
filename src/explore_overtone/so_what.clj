(ns explore-overtone.so-what
  (:use [overtone.live :except [overtone.music.pitch]]
        [explore-overtone.pitch]
        [oversampler.piano.inst]
        [overtone.synth.stringed]))

;; globals
(def Ω {:metro (metronome 134)
        :mode (atom (scale->field :D :dorian))})

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
  (deg->pitch @(:mode Ω) degree))
;; (map pitchv (range 7r40 7r50))
;; (52 54 55 57 59 60 62) & :e3 is 52

(defn chordv
  "basic chords.  num-notes gives modad, dyad, triad, tetrad, etc,
  correlating to Chords 5th, 7th, 9th, 11th, etc.  Scale degree gives
  the starting note."
  [num-notes degree]
  (deg->chord num-notes @(:mode Ω) degree))

;; FIXME – add this to pitch.clj  chords should be built with this instead
(defn harmonize
  [Δintervals field deg]
  (let [intervals (reductions + 0 Δintervals)
        pitches (map #(deg->pitch field (+ deg %)) intervals)]
    pitches))

;; swing rhythm
(defn linear-map
  "given x0 -> y0.  x1 -> y1.  x maps linearly to y"
  [x0 x1 y0 y1 x]
  (let [dydx (/ (- y1 y0) (- x1 x0))
        dx (- x x0)]
    (+ y0 (* dydx dx))))

;; eigth/eight -> dotted-eigth/sixteenth: (swing-time 3/4 1 t)
(defn swing-time
  "swing a time"
  [first-dur swing-dur t]
  (let [i (int t)
        f (mod t swing-dur)
        h (/ swing-dur 2)]
    (if (<= f h)
      (+ i (linear-map 0 h         0         first-dur f))
      (+ i (linear-map h swing-dur first-dur swing-dur f)))))

(defn swing-dur
  "swing a duration, starting at t"
  [first-dur swing-dur t dur]
  (let [t0 (swing-time first-dur swing-dur t)
        t1 (swing-time first-dur swing-dur (+ t dur))]
    (- t1 t0)))

;; actually went into audacity to see what they were doing
;; samples: (7744 11680) (7136 12512) (7328 12864) (5696 14080)
;; 19760.0 samples per beat = 134 bpm
;; 0.36-0.39 for first beat.  smallest is at most .288
(def this-swing-time (partial swing-time 5/8 1))
(def this-swing-dur (partial swing-dur 5/8 1))

;; 32-bar AABA song form and were in D Dorian for the A sections and
;; modulated a half step up to E-flat Dorian for the B section.
;; beat/4 = bar.  8 bars/section = 32 beats/section.  32*4=128 beats/AABA
;; beat mod 128 = 0,32,96 = D dorian.  @64 = E-flat Dorian
;; pass pattern + count?
(defn adjust-bar
  [beat bar pat-keys pat-bars]
  (if (= 0.0 (mod beat 4))
    (let [cur-key (first pat-keys)
          cur-bar (first pat-bars)
          bar (inc bar)
          ;_ (println beat bar cur-key cur-bar)
          ]
      (if (= bar cur-bar)
        [0 (rotate 1 pat-keys) (rotate 1 pat-bars)]
        [bar pat-keys pat-bars]))
    [bar pat-keys pat-bars]))

(defn left-hand
  "beat, pitches, velocities and durations"
  [beat bar is vs ds pat-keys pat-bars]
  (let [dur (first ds)
        [bar pat-keys pat-bars] (adjust-bar beat bar pat-keys pat-bars)
        ;;_ (println "LH" bar (first pat-keys))
        ]
    (if (= 0.0 (mod beat 4))
      (if (= :A (first pat-keys))
        (reset! (:mode Ω) (scale->field :D  :dorian))   ;; :A
        (reset! (:mode Ω) (scale->field :D# :dorian)))) ;; :B
    (when-not (nil? (first is))
      ;; Chord by Bill Evans!  A combination of quartal harmony and
      ;; tertiary harmony in D Dorian mode. (Quartal means chords built in 4th
      ;; intervals. Tertiary means chords built in 3rds.)
      ;; "D2" "E2" "F2" "G2" "A2" "B2" "C3" "D3" "E3" "F3" "G3" "A3" "B3" "C4" "D4"
      ;;       ^              ^              ^              ^         ^
      ;;  ^              ^              ^              ^         ^
      (let [ps (harmonize [3 3 3 2] @(:mode Ω) (first is))]
        (dorun (doseq [p ps] (play beat sampled-piano p (first vs) dur)))))
    (apply-by ((:metro Ω) (+ beat dur))
              #'left-hand [(+ beat dur) bar (rotate 1 is) (rotate 1 vs) (rotate 1 ds)
                           pat-keys pat-bars])))

(defn bass
  [beat ps vs ds]
  (let [dur        (first ds)
        swing-beat (this-swing-time beat)
        swing-beat (+ swing-beat (/ (- (rand) 0.5) 10)) ;; humanize +/- .05
        swing-dur  (this-swing-dur beat dur)
        next-beat  (min (+ beat dur -0.05) (+ swing-beat swing-dur))]
    (when-not (nil? (first ps))
      (play swing-beat sampled-piano (pitchv (first ps)) (first vs) swing-dur))
    (apply-by ((:metro Ω) next-beat)
              #'bass [(+ beat dur) (rotate 1 ps) (rotate 1 vs) (rotate 1 ds)])))

(defn next-measure []
  (* 4 (metro-bar (:metro Ω))))

(comment
  ;; See the sheet music animated.
  ;; https://www.youtube.com/watch?v=Rhv8iOY08TY

  (do
    (left-hand (next-measure) 0
               [nil 7r41 7r40 nil]
               [nil 0.6  0.7  nil]
               [6.0 1.5  0.25 0.25]
               [:A :A :B :A]
               [8  8  8  8])
    (bass (next-measure)
          [nil 7r30 7r34 7r35 7r36 7r40 7r41 7r36 7r40 nil
           nil 7r30 7r34 7r35 7r36 7r40 7r41 7r36 7r40 7r34 nil
           nil 7r30 7r34 7r35 7r36 7r40 7r41 7r36 7r40 nil
           nil 7r41 7r41 7r41 7r40 7r34 nil]
          [0.1 0.6  0.7  0.3  0.4  0.5  0.6  0.5  0.4  0.1
           0.1 0.6  0.7  0.3  0.4  0.5  0.6  0.5  0.4  0.6  0.1
           0.1 0.6  0.7  0.3  0.4  0.5  0.6  0.5  0.4  0.1
           0.1 0.7  0.7  0.7  0.5  0.6  0.1]
          [1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2 2 2
           1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2 3/2 2
           1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2 2 2
           1/2 3/2 1   1   3/2 1/2 2]))

  (stop)

)
