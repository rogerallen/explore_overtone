(ns explore-overtone.sorensen-oscon-2014
  (:use [overtone.live]
        [overtone.inst.sampled-piano]
        [overtone.inst.drum]
        [overtone.inst.synth]))
;; ======================================================================
;; A work derived & inspired by Andrew Sorensen OSCON 2014 Keynote:
;; "The Concert Programmer" coded for Overtone by Roger Allen
;;
;; See the original here: https://www.youtube.com/watch?v=yY1FSsUV-8c
;;
;; also used this code as a short-cut for the translation
;;   https://github.com/allenj12/jam1/blob/master/src/jam1/core.clj
;; ======================================================================

;; tempo of the piece.  used as a global def
(def metro (metronome 110))

;; ======================================================================
;; fmsynth instrument seems close to what Andrew has, but it needs work.
(defsynth fmsynth
  [note 60 divisor 1.0 depth 1.0
   attack 0.05 release 0.05 ;; envelope times
   duration 1.0 level 1.0 out-bus 0]
  (let [carrier   (midicps note)
        modulator (/ carrier divisor)
        S         (- duration attack release)
        mod-env   (env-gen (lin attack 0 (+ S release)))
        amp-env   (env-gen (lin attack S release) :action FREE)
        osc1      (* mod-env (* carrier depth) (sin-osc modulator))
        ]
    (out out-bus (pan2 (* amp-env (sin-osc (+ carrier osc1)))))))

;; Use play for sampled-piano, play1 for fmsynth
;; here's a fmsynth variant for the right-hand part
;; FIXME – this insn't quite there yet
(def fmsynth1 (partial fmsynth :attack 0.01 :release 0.1 :divisor 0.15 :depth 0.5))
;;(fmsynth1 :note 79 :duration 0.2)

(defn play
  "for synths that have :note, :level and :gate, play a note at a
  certain beat & turn it off after the duration in beats."
  [beat synth pitch level dur]
  (let [cur-synth (at (metro beat) (synth :note pitch :level level))]
    (at (metro (+ beat dur)) (ctl cur-synth :gate 0))))

(defn play1
  "for synths that have :note, :level and :duration, play a note at a
  certain beat & allow it to turn off after the duration in beats."
  [beat synth pitch level dur]
  (let [dur-s (* dur (/ (metro-tick metro) 1000))]
    (at (metro beat) (synth :note pitch :duration dur-s :level level))))

;; ======================================================================
;; left hand

;; I want to think in notes, not midi pitches like 52
(def root (atom :e3))

(defn left-hand
  "temporal recursion pattern"
  [beat ps ds]
  (let [dur (first ds)];)) ;; uncomment to stop
    ;; add this 3rd
    (when (= 0 (mod beat 8))
      (reset! root (rand-nth (remove #(= @root %) [:e2 :d2 :c2]))))
    (play beat sampled-piano (note (first ps)) 0.5 dur)
    ;; add this 2nd
    (play (+ 0.5 beat) sampled-piano (note @root) 0.45 dur)
    (apply-by (metro (+ beat dur))
              #'left-hand [(+ beat dur) (rotate 1 ps) (rotate 1 ds)])))

;;(left-hand (metro) [:g3 :g3 :a3 :b3] [1])
;;(left-hand (metro) [:g3 :g3 :a3 :b3] [1 0.5 1.5 1])
;;(stop)

;; ======================================================================
;; right hand

(defn quantize
  "given a sorted seq in s, find the item in the seq closest to n.  n
  can be any type that note converts (string, keyword or integer) of a
  floating-point value."
  [n s]
  (let [nt         (if (float? n) n (note n))
        split-seq  (split-with #(<= % nt) s)
        nt-below   (last (first split-seq))
        nt-above   (first (last split-seq))
        ;; handle ends of the sequence
        nt-below   (if (nil? nt-below) (first s) nt-below)
        nt-above   (if (nil? nt-above) (last s) nt-above)
        Δ-nt-below (- nt nt-below)
        Δ-nt-above (- nt-above nt)]
    (if (> Δ-nt-above Δ-nt-below)
      nt-below
      nt-above)))

;; main scale of the piece
(def scale0 (scale-field :E :aeolian))

;; extempore cosr is (cosr center range freq) and beat is an assumed parameter
;; overtone cosr is (cosr index range centre period)

(defn right-hand
  [beat dur]
  (play beat
        sampled-piano
        (quantize (cosr beat (cosr beat 3 5 2) (+ (note @root) 24) 3/7) scale0)
        (cosr beat 0.15 0.5 3/7)
        (* 2.0 dur))
  ;; add this 2nd
  (if (> (rand) 0.6)
    (play1 beat
           fmsynth1
           (quantize (+ 7 (cosr beat (cosr beat 3 5 2) (+ (note @root) 24) 3/7)) scale0)
           (cosr beat 0.15 0.6 3/7)
           (* 0.2 dur)))
  (apply-by (metro (+ beat dur)) #'right-hand [(+ beat dur) dur]))

;;(right-hand (metro) 1/4)
;;(stop)

;; ======================================================================
;; bass line
(defn bassline
  [beat ps ds]
  (let [dur (first ds)];)) ;; uncomment to stop
    (play1 beat fmsynth (note @root) 0.8 (* (first ps) (first ds)))
    (apply-by (metro (+ beat dur))
              #'bassline [(+ beat dur) (rotate 1 ps) (rotate 1 ds)])))

;; ======================================================================
;; kick drum
(defn kick-drum
  [beat dur]
  (let [f (midi->hz (- (note @root) 4))];)) ;; uncomment to stop
    (at (metro (- beat 1/4)) (kick4 :freq f :amp 0.6 :attack 0.04 :decay dur))
    (at (metro beat)         (kick4 :freq f :amp 0.8 :attack 0.04 :decay dur))
    (apply-by (metro (+ beat (* 0.5 dur))) #'kick-drum [(+ beat dur) dur])))

;; ======================================================================
;; hats
(def hat0 (partial closed-hat :low 10000 :hi 8000))
(def hat1 (partial closed-hat2))
(defn hats
  [beat dur]
  (let [hat (rand-nth [hat0 hat1])];)) ;; uncomment to stop
    (at (metro beat) (hat) (cosr beat 0.4 0.5 (rand-nth [3/7 2/5])))
    (apply-by (metro (+ beat dur)) #'hats [(+ beat dur) dur])))

;; ======================================================================
;; start on 1st beat of the bar (default is 4 beats/measure)
(left-hand  (* 4 (metro-bar metro)) [:g3 :g3 :a3 :b3] [1])
(right-hand (* 4 (metro-bar metro)) 1/4)
(bassline   (* 4 (metro-bar metro)) [0.25 0.25 0.6] [3/2 1 3/2])
(kick-drum  (* 4 (metro-bar metro)) 1)
(hats       (* 4 (metro-bar metro)) 1/4)

(stop)
