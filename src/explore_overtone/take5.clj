;; A bit of "Take 5" by Paul Desmond. All hail The Dave Brubeck Quartet!
(use 'overtone.live)
(use 'oversampler.piano.inst)
(use 'oversampler.cello.inst)

;; FIXME - build this from the chords, not just raw notes.
(def piano-seq1
  [5 ;; duration of sequence in beats
   ;; tuple is beat, duration, level, notes.
   ;; (duration is in beats)
   [0   1   0.3 :Db2 ]
   [1/2 1/2 0.6 :Gb2 :Bb2 :Eb3]
   [3/2 1/4 0.3 :Db2]
   [2   1   0.6 :Gb2 :Bb2 :Eb3]
   [3   1   0.3 :Bb2]
   [4   1   0.6 :F2 :Ab2 :Db2]])

(def sax-melody1
  [20
   [0    3/4 0.6 :Bb2]  ;; actually picks up at beat 3
   [0.75 1/4 0.8 :Eb3]  ;; my "dynamics" are a baseless hack.
   [1    3/4 0.6 :Gb3]
   [1.75 1/4 0.8 :Ab3]
   
   [2     1/2 0.6 :A3]
   [2.5   1/2 0.8 :Bb3]
   [3     1/2 0.6 :A3]
   [3.5   1/2 0.8 :Ab3]
   [4     1   0.8 :Gb3]
   [5     1   0.8 :Bb2]
   [5.5   1/4 0.8 :B2]
   [5.75  1/4 0.6 :C3]
   [6     1   0.8 :D3]
   
   [7     3   0.7 :Eb3]
   [10    1/4 0.6 :F3]
   [10.25 1/4 0.8 :Gb3]
   [10.5  1/4 0.6 :F3]
   [10.75 1/4 0.8 :Eb3]
   [11    1   0.8 :Db3]
   
   [12    3   0.7 :Eb3]
   [15    1/4 0.6 :Db3]
   [15.25 1/4 0.8 :Eb3]
   [15.5  1/4 0.6 :Db3]
   [15.75 1/4 0.8 :Bb2]
   [16    1   0.8 :Ab2]

   [17    3   0.7 :Bb2]])
  
(defn play-notes
  [at-beat m n the-inst]
  (let [start (+ at-beat (first n))
        end (+ start (second n))
        level (nth n 2)
        notes (doall (drop 3 n))]
    (doseq [x notes]
      (let [nx (at (m start) (the-inst :note (note x) :level level))]
        (at (m end) (ctl nx :gate 0))))))

(defn play-a-seq
  [count m at-beat the-seq the-inst]
  (let [seq-beats (first the-seq)
        the-notes (rest the-seq)
        nxt-count (dec count)
        nxt-beat (+ at-beat seq-beats)]
    (doseq [a-note the-notes]
      (play-notes at-beat m a-note the-inst))
    (if (> nxt-count 0)
      (recur nxt-count m nxt-beat the-seq the-inst))))

(let [m (metronome 156)] ;; normally 176
  (play-a-seq  3 m  0 piano-seq1 sampled-piano)
  (play-a-seq  9 m 15 piano-seq1 sampled-piano)
  (play-a-seq  2 m 18 sax-melody1 sampled-cello))
