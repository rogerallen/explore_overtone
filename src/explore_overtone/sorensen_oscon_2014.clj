(ns explore-overtone.sorensen-oscon-2014
  (:use [overtone.live]
        [overtone.inst.sampled-piano]
        [overtone.inst.drum]
        [overtone.inst.synth]))

;; inspired by Andrew Sorensen OSCON 2014 Keynote: "The Concert Programmer"
;;   https://www.youtube.com/watch?v=yY1FSsUV-8c
;; also used this code for a short-cutting the translation
;;   https://github.com/allenj12/jam1/blob/master/src/jam1/core.clj

;; ======================================================================
;; left hand

;; tempo of the piece
(def metro (metronome 110))
;; I want to think in notes, not midi pitches like 52
(def root (atom :e3))

(defn play
  "for synths that have :note, :level and :gate, play a note at a
  certain beat & turn it off after the duration in beats."
  [beat synth pitch level dur]
  (let [cur-synth (at (metro beat) (synth :note pitch :level level))]
    (at (metro (+ beat dur)) (ctl cur-synth :gate 0))))

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

(left-hand (metro) [:g3 :g3 :a3 :b3] [1])
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
  (at (metro beat)
      (play beat
            sampled-piano
            (quantize (cosr beat (cosr beat 3 5 2) (+ (note @root) 24) 3/7) scale0)
            (cosr beat 0.15 0.5 3/7)
            (* 2.0 dur))
      (apply-by (metro (+ beat dur)) #'right-hand [(+ beat dur) dur])))

;;(right-hand (metro) 1/4)
;;(stop)

;; start on 1st beat of the bar (default is 4 beats/measure)
(left-hand (* 4 (metro-bar metro)) [:g3 :g3 :a3 :b3] [1])
(right-hand (* 4 (metro-bar metro)) 1/4)

;; ======================================================================
;; CODE IN PROGRESS BELOW THIS...


(defn hats
  [beat dur]
  (at (metro beat)
      (closed-hat2 :amp 0.2 :decay (rand-nth '(0.3 0.1))))
  (apply-by (metro (+ beat (* 0.5 dur))) hats (+ beat dur) dur []))

(defn kick-drum
  [beat dur]
  (at (metro (- beat 1/4))
      (kick4 :freq 150 :amp 0.8 :attack 0.04 :decay dur))
  (at (metro beat)
      (kick4 :freq 250 :amp 0.9 :attack 0.04 :decay dur))
  (apply-by (metro (+ beat (* 0.5 dur))) kick-drum (+ beat dur) dur []))


;; left hand pitches
(def lpitches [:G3 :G3 :A3 :B3])

(do
  (left-hand (metro) (cycle lpitches) 1)
  (right-hand (metro) 1/4)
  (hats (metro) 1/4)
  (kick-drum (metro) 1/2)
)

(stop)

;; from https://github.com/overtone/overtone/blob/master/src/overtone/examples/synthesis/fm.clj
(o/env-gen (o/adsr 1.5 1.5 0.8 1.5) :gate gate :action FREE)
(defsynth fmsynth
  [carrier 550 modulator 220 depth 1.0 out-bus 0]
  (let [modulator (/ carrier divisor)
        mod-env   (env-gen (lin 1 0 3))
        amp-env   (env-gen (lin 1 1 2) :action FREE)]
    (out out-bus
         (pan2 (* 0.5 amp-env (sin-osc
                               (+ carrier
                                  (* mod-env (* carrier depth) (sin-osc modulator)))))))))

(defsynth fmsynth
  [carrier 550 modulator 220 depth 1.0 out-bus 0]
  (let [o1

(fmsynth 800 12 2)
(stop)
