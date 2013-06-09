(ns explore-overtone.song-algorithm
  (:require [overtone.live           :as o]
            [overtone.synth.stringed :as strings]
            [oversampler.piano.inst  :as piano]
            [leipzig.live   :as ll]
            [leipzig.melody :as lm]
            [leipzig.scale  :as ls]
            [leipzig.canon  :as lc]
            [leipzig.chord  :as lch]))

;; This code inspired by this question/comment on Reddit:
;;   http://www.reddit.com/r/musictheory/comments/1fe8y9/adding_chords_to_melody/
;;   http://www.reddit.com/r/musictheory/comments/1fe8y9/adding_chords_to_melody/ca9gqtp
;; that comment really broke down the algorithm to accomplish this simply

;; First setup some instruments to play with
;; guitar plays melody, piano plays accompaniment

(strings/gen-stringed-synth ektara 1 true)
(defn pick [distort pan {midi :pitch, start :time, length :duration, amp :dynamic}]
    (let [synth-id (o/at start
                     (ektara midi :distort distort :amp amp :gate 1 :pan pan
                             ;;:rvb-mix 0.35 :rvb-room 0.85 :rvb-damp 0.85
                             ;;:rvb-mix 0.0 :rvb-room 0.0 :rvb-damp 0.0
                             ;;:lp-freq 4000 :lp-rq 0.5
                             ))]
      (o/at (+ start length) (o/ctl synth-id :gate 0))))

(strings/gen-stringed-synth string3 3 true)
(defn pick3 [distort pan {pitches :pitch, start :time, length :duration, amp :dynamic}]
    (let [[n0 n1 n2] pitches
          synth-id   (o/at start
                           (string3 n0 n1 n2
                                    1  1  1
                                    :distort distort :amp amp))]
      (o/at (+ start length) (o/ctl synth-id :gate-0 0 :gate-1 0 :gate-2 0))))

(defn piano1 [{pitch :pitch, start :time, length :duration, amp :dynamic}]
    (let [synth-id  (o/at start (piano/sampled-piano :note pitch :level amp))]
      (o/at (+ start length) (o/ctl synth-id :gate 0))))

(defn piano3 [{pitches :pitch, start :time, length :duration, amp :dynamic}]
    (let [[n0 n1 n2] pitches
          synth-id0  (o/at start (piano/sampled-piano :note n0 :level amp))
          synth-id1  (o/at start (piano/sampled-piano :note n1 :level amp))
          synth-id2  (o/at start (piano/sampled-piano :note n2 :level amp))]
      (o/at (+ start length) (o/ctl synth-id0 :gate 0))
      (o/at (+ start length) (o/ctl synth-id1 :gate 0))
      (o/at (+ start length) (o/ctl synth-id2 :gate 0))))

(defmethod ll/play-note :melody [note]
  ;;(piano1 note))
  (pick 0.2 -0.55 note))

(defmethod ll/play-note :accompaniment [notes]
  (piano3 notes))
  ;;(pick3 0.3 -0.55 notes))

;; I think you could either start with chord structure & create melody
;; Or vice-versa.
;; This code currently creates both independently since that is easiest.

;; Some googling found advice from
;; http://www.wikihow.com/Compose-a-Melody and
;; http://smu.edu/totw/melody.htm
;;
;; - mainly use runs along the scale
;; - use jumps sparingly, not back-to-back
;; - after a jump change direction back towards where you came from
;; - reach a "peak" in the 2nd half of the melodic sequence
;;   - "peak" can be a "trough"
;;   - peak can be at 50% all the way to 100% through.
;;   - I think you could call this area part the "response" to the "call"
;; - tones on the beat should be chordal tones
;;
;; - bass line can mirror melody contour for interest
;;
;; FIXME hardly any of the above is in the code below

;; this is the workhorse random function for this whole file
(defn wrand
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

;; I like dynamic phrases
(defn dynamic-phrase
  "Translates a sequence of durations, pitches and dynamics into a melody.
  e.g. (phrase [1 1 2] [7 6 4] [0.6 0.9 0.8])"
  [durations pitches dynamics]
  (->> (lm/phrase durations pitches) (lm/having :dynamic dynamics)))

(defn make-durations
  "given a dist vector and a number of beats to fill, return a vector of random durations that add up to beats"
  [dist beats]
  (let [durs (drop 1 (map first
                          (take-while #(<= (second %) beats)
                                      (reductions #(vector %2 (+ (second %1) %2))
                                                  [0 0]
                                                  (repeatedly #(wrand dist))))))
        delta (- beats (apply + durs))
        durs (concat (butlast durs) (list (+ (last durs) delta)))]
    durs))
;; (make-durations [0 1 3 1 0.5] 16)

(defn rand-melody-pitch-step
  "return a plausible random step for a melody"
  []
  ;; normally increment by +/-1, but try other variants with some frequency
  (let [pitch-step-base -8;-7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8
        pitch-step-dist [2  1  3  1  3  1  3 15  3 15  3  1  3  1  3  1  2]
        pitch-step (+ pitch-step-base (wrand pitch-step-dist))]
    pitch-step))

(defn make-call-phrase
  "create a random phrase suitable for the call part of a call/response"
  [num-beats duration-dist]
  (let [start-pitch (wrand [1 1 1 1 1 1 1 1])
        durations   (make-durations duration-dist (* 2 num-beats))
        durations   (map #(/ % 2) durations)
        pitches     (take (count durations)
                         (iterate (fn [x] (+ x (rand-melody-pitch-step))) start-pitch))
        dynamics    (take (count durations)
                          (repeatedly #(/ (+ (wrand [1 2 2 2 2]) 5) 10)))]
    (dynamic-phrase durations pitches dynamics)))

;; FIXME make proper response. add "peak" in here somewhere
(defn make-response-phrase
  [num-beats duration-dist]
  (make-call-phrase num-beats duration-dist))

;; types of transposition to consider:
;; * pitch transposition (up a third, etc.)
;; * retrograde (reverse the melody)
;; o time augmentation (double, halve, etc.)
;; * inversion (upside down melody)
;; * combinations of all the above

(defn alter-phrase
  [the-phrase]
  (let [r (wrand [3 1 2])]
    (case r
      0 ((lc/interval (rand-melody-pitch-step)) the-phrase)
      1 ((lc/simple 8) (lc/crab the-phrase)) ;; FIXME lc/simple may go away per liepzig issue #8
      2 ((lc/interval (rand-melody-pitch-step)) (lc/mirror the-phrase))
      )))
;; (def foo (make-call-phrase 8 [0 1 1]))
;; (alter-phrase foo)

;; Got the idea for adding an overall structure to the melody with this
;; interesting breakdown of a tune http://www.youtube.com/watch?v=I6fjqw0FAQQ
;;   main motif / answer / main / transposed answer = 8 bars
;;   second motif / transposed second motif = 4 bars
;;   2x length phrase to finish = 4 bars
;;   total = 16 bars
;; another way to break it down:
;;   abaBcCd  where abc are all 2 bars each and d is 4 bars
;; my own analysis would break it down slightly differently since I don't like
;; that "d" is 4 bars.  the first part of d is like b I think.
;; So, my own quick take on this is:
;;   ab aB cC B'd
;; you need 4 parts a,b,c,d and each are 2 bars.
;; B and B' are alterations of b.  C is an alteration of c
;; a and c are "calls" and don't need peaks
;; b and d are "responses" and need peaks
;;
;; FIXME this is just one type structure.  there are probably an infinite variety
(defn make-melody
  [duration-dist num-beats melody-keyword]
  (let [num-phrase-beats (/ num-beats 8)
        melody-a  (make-call-phrase     num-phrase-beats duration-dist)
        melody-b  (make-response-phrase num-phrase-beats duration-dist)
        melody-c  (make-call-phrase     num-phrase-beats duration-dist)
        melody-d  (make-response-phrase num-phrase-beats duration-dist)
        melody-b1 (alter-phrase melody-b)
        melody-b2 (alter-phrase melody-b)
        melody-c1 (alter-phrase melody-c)]
    (->> melody-a
         (lm/then melody-b)
         (lm/then melody-a)
         (lm/then melody-b1)
         (lm/then melody-c)
         (lm/then melody-c1)
         (lm/then melody-b2)
         (lm/then melody-d)
         (lm/where :part (lm/is melody-keyword)))))

;; After you do that, assign diatonic chord types to each note in the
;; scale.  [This is theory-speak for take 3 notes, starting at each
;; note in the scale and every-other-note after that. Ex.  Cmajor is
;; CDEFGAB, The first (I) diatonic chord is CEG.  The 2nd (ii) is DFA
;; see http://en.wikipedia.org/wiki/Triad_(music)]
;; see http://en.wikipedia.org/wiki/Diatonic_function]
;;
;; [Not mentioned is the idea of using extended chords (more than 3
;; notes).  This is something to also consider.  See
;; http://en.wikipedia.org/wiki/Extended_chord]
;;
;; Now your next step is to figure out where you want the
;; chords. Whenever that is, take that note and play the chord that
;; corresponds to it (or you can play 2 chords or 4 chords below it if it
;; flows better, but generally the one that directly corresponds is your
;; go-to chord. it takes some experimenting).
;;
;; What your aiming for is a chord progression that both fits with your
;; melody and makes harmonic sense, i.e. if you were to play the chords
;; by themselves, it sounds very natural. and yeah, that's pretty much
;; how it goes.
;; [What this means is that chord progressions are not completely random.  There are certain paths that are far more well travelled.
;; http://www.hooktheory.com/blog/i-analyzed-the-chords-of-1300-popular-songs-for-patterns-this-is-what-i-found/
;; http://www.hooktheory.com/trends
;; ]
;;
;; FIXME you may notice quite a bit of similarity with make-melody.  This should change
;;
(defn make-accompaniment
  [duration-dist num-beats accompaniment-keyword]
  (make-melody duration-dist num-beats accompaniment-keyword))

(def inc2 (comp inc inc))
(defn triad
  "translates to a chord with tonic at start-index"
  [key start-index]
  (take 3 (map key (iterate inc2 start-index))))

;; a song has a melody along with chords as accompaniment
;; at this point, the chords are just the tonic of the chord
(defn make-song []
  (let [num-beats 64]
    (->> (make-melody
          ;;0 0.5 1 1.5 2 2.5 3 durations
          [ 0  8  2  4  2  4  1]
          num-beats
          :melody)
         (lm/with (make-accompaniment
                   ;; 0 .5 1 1.5 2 2.5 3 3.5 4  longer durations than melody
                   [  0  0 1  3  4  3  1  1  1]
                   num-beats
                   :accompaniment)))))

(defn play-song [speed key song]
  (->> song
       (lm/where :time speed)
       (lm/where :duration speed)
       (lm/wherever (fn [x] (= :melody (:part x))) :pitch key)
       (lm/wherever (fn [x] (= :accompaniment (:part x))) :pitch (partial triad key))
       ll/play))

;; Figure out what key your melody is in, give it a tempo, a key
(def the-song (make-song))
(def song-bpm (lm/bpm 180))
(def song-key (comp ls/low ls/D ls/major))
;; then play it
(play-song song-bpm song-key the-song)

(comment
  (o/recording-start "random_access_melodies.wav")
  ;; or enjoy some random tunes
  (play-song (lm/bpm (+ 80 (rand-int 100)))
             (comp ls/D ls/major)
             (make-song))
  (o/recording-stop)
  )
