(ns explore-overtone.atlas-of-consonance
  (:require [overtone.live :as o]
            [quil.core :as q]))

;; ======================================================================
;; ideas from Norman Sohl's Atlas of Consonance
;; http://www.sohl.com/mt/maptone.html

(defn overtone-seq
  "create an infinite sequence of overtones, given a tonic freq f"
  [f]
  (map #(* % f) (iterate inc 1)))

(defn octave-seq
  "given sequence of overtones, create sequence of octaves"
  [fs]
  (map #(* % 2) fs))

(defn between-seq
  [a b i]
  "given the ends of an octave range [a,b] and an increment i, find
   the notes that lie between [a,b]"
  (range (+ a i) b i))

(defn per-octave-seqs
  "for each octave range (f 2f) (2f 4f) (3f 6f) ... find the base frequency multiples in that range.  (e.g. 3f is in the 2nd octave range)"
  [f]
  (let [overtones (overtone-seq f)
        octaves   (octave-seq overtones)]
    (map (fn [f1 o1] (between-seq f1 o1 f))
         overtones
         octaves)))

(defn take-per-octave-seqs
  "filter per-octave-seqs to be within the range of n octaves.  So,
  n=7 would filter out some notes in the 6..12 octave range"
  [f n]
  (map #(filter (fn [x] (< x (* (inc n) f))) %)
       (take n (per-octave-seqs f))))

(defn take-norm-per-octave-seqs
  "Normalize per-octave-seqs to the tonic octave range (e.g. 3f -> 3f/2)."
  [f n]
  (let [overtones (overtone-seq f)]
    (map (fn [f1 s]
           (map (fn [s1] (* f (/ s1 f1))) s))
         overtones
         (take-per-octave-seqs f n))))

(defn sorted-freq-map
  "take a sequence of frequency sequences into a sorted histogram map"
  [sfs]
  (let [f-histogram (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} (flatten sfs))]
    (into (sorted-map-by (fn [key1 key2]
                           (compare [(get f-histogram key2) key2]
                                    [(get f-histogram key1) key1])))
          f-histogram)))

;; ======================================================================
;; overtone synth that plays a tonic and several overtones, plus
;; another note with it's overtones.
;;
;; FIXME – dynamically use number of octaves to add overtones to sin-osc?
(o/defsynth cons-synth
  [tonic-freq 200
   note-freq  300
   gate       1]
  (let [num-octaves 5
        a (o/mix (o/sin-osc (map #(* (+ 1 %) tonic-freq) (range num-octaves))))
        b (o/mix (o/sin-osc (map #(* (+ 1 %) note-freq) (range num-octaves))))
        e (o/env-gen (o/asr 0.1 1.0 0.5) :gate gate :action o/FREE)]
  (o/out 0 (o/pan2 (* e (o/mix [a b])) 0))))

;; ======================================================================
;; "public" state to play with
(defonce tonic-freq-atom (atom 0))
(defonce note-freq-atom (atom 0))
(defonce freq-histo-atom (atom ()))
(defonce synth-atom (atom nil))

(defn set-tonic-freq [f]
  (swap! tonic-freq-atom (fn [_] f))
  (when @synth-atom
    (o/ctl @synth-atom :tonic-freq f))
  nil)

(defn set-note-freq [f]
  (swap! note-freq-atom (fn [_] f))
  (when @synth-atom
    (o/ctl @synth-atom :note-freq f))
  nil)

(defn set-num-octaves [n]
  (swap! freq-histo-atom
         (fn [x] (sorted-freq-map (take-norm-per-octave-seqs
                                  @tonic-freq-atom n))))
  nil)

(defn start-synth []
  (swap! synth-atom (fn [_] (cons-synth @tonic-freq-atom @note-freq-atom))))
(defn stop-synth []
  (when @synth-atom
    (o/ctl @synth-atom :gate 0)
    (swap! synth-atom (fn [_] nil))))

;; ======================================================================
;; Quil routines
(defn setup []
  (set-tonic-freq  200)
  (set-note-freq   200)
  (set-num-octaves 13)
  (q/smooth)
  (q/frame-rate 30))

(defn draw-diatonic-hatches
  [b h2]
  (q/stroke-weight 1.5)
  (q/stroke 0 0 0)
  (q/fill 0 0 0)
  (let [note-str ["c" "c♯" "d" "d♯" "e" "f" "f♯" "g" "g♯" "a" "a♯" "b" "c"]]
    (dotimes [i 13]
      (let [x (- (Math/pow 2 (/ i 12)) 1.0)
            x (q/lerp b (- (q/width) b) x)
            s (note-str i)
            sw2 (/ (q/text-width s) 2)]
        (q/line x h2 x (+ h2 20))
        (q/text s (- x sw2) (- (q/height) (/ b 2)))))))

(defn draw-consonance-hatches
  [b h2 max-freq]
  (dorun
   (doseq [k (keys @freq-histo-atom)]
     (let [w  (@freq-histo-atom k)
           nw (/ w max-freq)
           x  (q/lerp b (- (q/width) b)
                      (/ (- k @tonic-freq-atom) @tonic-freq-atom))
           a  (q/lerp 0 255 nw)
           sw (+ 1 (* 2 nw))
           sh (+ 10 (* 30 nw))
           ;;x  (- x (/ sw 2))
           ]
       (q/stroke 200 0 0 a)
       (q/stroke-weight 1.5)
       (q/line x (- h2 sh) x h2))))
  (q/stroke 200 0 0 255)
  (q/stroke-weight 3)
  (q/line b (- h2 50) b h2)
  (q/line (- (q/width) b) (- h2 50) (- (q/width) b) h2))

(defn draw-x-axis
  [b h2]
  (q/stroke 0 0 0)
  (q/stroke-weight 1.5)
  (q/line b h2 (- (q/width) b) h2))

(defn draw-note [b]
  (when @synth-atom
    (let [x (q/lerp b (- (q/width) b)
                    (/ (- @note-freq-atom @tonic-freq-atom) @tonic-freq-atom))]
      (q/stroke 0 0 240)
      (q/stroke-weight 3)
      (q/line x b x (- (q/height) b))
      (q/line b b b (- (q/height) b)))))

(defn draw []
  (let [b 50
        h2 (/ (q/height) 2)
        max-freq (apply max (vals @freq-histo-atom))]
    (q/background 250)
    (draw-consonance-hatches b h2 max-freq)
    (draw-diatonic-hatches b h2)
    (draw-x-axis b h2)
    (draw-note b)))

(defn get-closest-freq*
  [x fs]
  (let [b 50
        f (q/lerp @tonic-freq-atom (* 2 @tonic-freq-atom)
                  (/ (- x b) (- (q/width) (* 2 b))))
        v (apply min-key
                 (fn [x] (Math/abs (- x f)))
                 fs)]
    v))

(defn get-closest-diatonic-freq
  [x]
  (get-closest-freq* x (map #(* @tonic-freq-atom (Math/pow 2 (/ % 12)))
                            (range 13))))

(defn get-closest-consonance-freq
  [x]
  (get-closest-freq* x (concat (list @tonic-freq-atom)
                               (keys @freq-histo-atom)
                               (list (* 2 @tonic-freq-atom)))))

(defn get-closest-freq
  [x y]
  (if (> y (/ (q/height) 2))
    (get-closest-diatonic-freq x)
    (get-closest-consonance-freq x)))

(defn mouse-button []
  (let [x (q/mouse-x)
        y (q/mouse-y)
        snap-freq (get-closest-freq x y)]
    (set-note-freq snap-freq)
    (if (q/mouse-state)
      (start-synth)
      (stop-synth))))

(defn run []
  (q/defsketch doodle
    :title          "atlas-of-consonance"
    :setup          setup
    :draw           draw
    :mouse-pressed  mouse-button
    :mouse-released mouse-button
    :size           [800 150]))

;; exec this to start the window
(run)

;; click on the diagram to play notes.
;; - in the top area, it will snap to the nearest consonance note
;; - in the bottom area, it will snap to the nearest diatonic note

;; use (o/stop) if it gets stuck playing

;; (set-tonic-freq 110)
;; (set-num-octaves 7)

;; (set-num-octaves 50)
