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
  "filter per-octave-seqs to be within the range of n octaves.  So, n=7 would filter out some notes in the 6..12 octave range"
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
;; "public" state to play with
(defonce tonic-freq-atom (atom 0))
(defonce freq-histo-atom (atom ()))
(defn set-tonic-freq [f]
  (swap! tonic-freq-atom (fn [_] f))
  nil)

(defn set-num-octaves [n]
  (swap! freq-histo-atom
         (fn [x] (sorted-freq-map (take-norm-per-octave-seqs @tonic-freq-atom n))))
  nil)

;; ======================================================================
;; Quil routines
(defn setup []
  (set-tonic-freq 100)
  (set-num-octaves 6)
  (q/smooth)
  (q/frame-rate 30))

(defn draw-diatonic-hatches
  [b h2]
  (q/stroke-weight 1.5)
  (q/stroke 0 0 0)
  (dotimes [i 13]
    (let [x (- (Math/pow 2 (/ i 12)) 1.0)
          x (q/lerp b (- (q/width) b) x)]
      (q/line x h2 x (+ h2 20)))))

(defn draw-consonance-hatches
  [b h2 max-freq]
  (dorun
   (doseq [k (keys @freq-histo-atom)]
     (let [w (@freq-histo-atom k)
           nw (/ w max-freq)
           x (q/lerp b (- (q/width) b) (/ (- k 100) 100))
           a (q/lerp 0 255 nw)
           sw (+ 1 (* 2 nw))
           sh (+ 10 (* 30 nw))
           x (- x (/ sw 2))]
       (q/stroke 200 0 0 a)
       (q/stroke-weight sw)
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

(defn draw []
  (let [b 50
        h2 (/ (q/height) 2)
        max-freq (apply max (vals @freq-histo-atom))]
    (q/background 250)
    (draw-consonance-hatches b h2 max-freq)
    (draw-diatonic-hatches b h2)
    (draw-x-axis b h2)))

(defn run []
  (q/defsketch doodle
    :title "consonance"
    :setup setup :draw draw
    :size [800 150]))

;; (run)

;; (set-num-octaves 6)
;; (set-num-octaves 7)
;; (set-num-octaves 11)
;; (set-num-octaves 20)
;; (set-num-octaves 40)
;; (set-num-octaves 200)

;; TODO
;; - add pick-nearest-note-to-mouse (either consonance or diatonic)
;; - play tonic and that note in overtone when clicked
;; - play a vector of notes for each overtone
