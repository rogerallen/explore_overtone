(ns explore-overtone.euclidian)

;; Found inspriration on this blog.
;;   http://ruinwesen.com/blog?id=216
;; which references this paper
;;   http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf
;; which describes the idea generating rhythms via an algorithm first used
;; for spallation neutron source accelerators.  This algorithm is related
;; to an algorithm first described by Euclid.  Rhythms produced via this
;; method are very similar to historic rhythms used across the world.

;; using this method, we can also create the standard scales/modes of western music
;; see below...

(defn- seq-split-remainder
  [s]
  (partition-by #(= (first s) %1) s))

(defn- interleave-seqs
  [fs rs]
  (let [int-seq (map #(concat %1 %2) fs rs)
        rem-fs (drop (count rs) fs)
        rem-rs (drop (count fs) rs)]
    (concat int-seq rem-fs rem-rs)))

(defn- bjorklund
  "Recursively distribute remainder sequence over the real sequence via algorithm
  Bjorklund used for the timing generation in neutron accelerators."
  [s]
  (loop [ss s]
    (let [ssr (seq-split-remainder ss)
          real (first ssr)
          remainder (second ssr)]
      (if (<= (count remainder) 1)
        (vec (flatten (concat real remainder)))
        (recur (interleave-seqs real remainder))))))

(defn- rotate
  "rotate a sequence s by n, returning a vector"
  [s n]
  (let [c (count s)]
    (vec (take c (drop (mod n c) (cycle s))))))

(defn euclidian-pattern
  "distribute count k of constant k0 in a sequence of length n, distributing them in a most even way.
  With the remainder of the sequence filled with constant k1.  Finally, rotate the result sequence by r steps."
  [k n r k0 k1]
  (let [k0s (repeat k k0)
        k1s (repeat (- n k) k1)]
    (rotate (bjorklund (map list (concat k0s k1s))) r)))

(defn euclidian-rhythm
  "distribute k ones in a sequence of length n, distributing them in a most-even way.
  an optional rotation by r is provided."
  ([k n]
   (euclidian-rhythm k n 0))
  ([k n r]
   (euclidian-pattern k n r 1 0)))

;; ======================================================================
;; helper routines to generate all the historical modes
;; Historical scales or modes have 7 steps between the notes.
;; 5 of them are whole steps and 2 are half-steps.  Normalizing
;; to half-steps, the standard scale steps can be generated via the
;; euclidian pattern generator with the following parameters.

(def modes {:aeolian 0, :locrian 1, :ionian 2, :dorian 3,
            :phrygian 4, :lydian 5, :mixolydian 6,
            :major 2, :minor 0})

(def chromatic-scale [:a :a# :b :c :c# :d :d# :e :f :f# :g :g#])

(defn get-scale-intervals
  "take the intervals between notes provided by the euclidian pattern and reduce
  them to create a sequence of indexes from 0.  Offset changes the rotation of
  the euclidean pattern"
  [rotation]
  (reduce #(concat %1 [(+ (last %1) %2)])
          [0]
          (euclidian-pattern 5 7 rotation 2 1)))

(defn get-scale
  "return the scale given the tonic keyword (e.g. :d) and the mode (e.g. :dorian)"
  [tonic mode]
  (map #(chromatic-scale (mod (+ %1 (.indexOf chromatic-scale tonic))
                              (count chromatic-scale)))
       (get-scale-intervals (modes mode))))

;; ======================================================================
(comment

  ;; basic rhythm
  (euclidian-rhythm 5 7)

  ;; testing examples from the paper.
  ;; these should all return true, but the
  ;; exceptions look reasonable to me and we can rotate to fix
  (= (euclidian-rhythm 1 2)   [1 0])
  (= (euclidian-rhythm 1 3)   [1 0 0])
  (= (euclidian-rhythm 1 4)   [1 0 0 0])
  (= (euclidian-rhythm 2 3 1) [1 0 1])
  (= (euclidian-rhythm 2 5)   [1 0 1 0 0])
  (= (euclidian-rhythm 3 4 2) [1 0 1 1])
  (= (euclidian-rhythm 3 5)   [1 0 1 0 1])
  (= (euclidian-rhythm 3 7)   [1 0 1 0 1 0 0])
  (= (euclidian-rhythm 3 8)   [1 0 0 1 0 0 1 0])
  (= (euclidian-rhythm 4 7)   [1 0 1 0 1 0 1])
  (= (euclidian-rhythm 4 11)  [1 0 0 1 0 0 1 0 0 1 0])
  (= (euclidian-rhythm 4 12)  [1 0 0 1 0 0 1 0 0 1 0 0])
  (= (euclidian-rhythm 5 7)   [1 0 1 1 0 1 1])
  (= (euclidian-rhythm 5 8)   [1 0 1 1 0 1 1 0])
  (= (euclidian-rhythm 5 9)   [1 0 1 0 1 0 1 0 1])
  (= (euclidian-rhythm 5 11)  [1 0 1 0 1 0 1 0 1 0 0])
  (= (euclidian-rhythm 5 12)  [1 0 0 1 0 1 0 0 1 0 1 0])
  (= (euclidian-rhythm 7 8 6) [1 0 1 1 1 1 1 1])
  (= (euclidian-rhythm 7 12)  [1 0 1 1 0 1 0 1 1 0 1 0])
  (= (euclidian-rhythm 7 16)  [1 0 0 1 0 1 0 1 0 0 1 0 1 0 1 0])
  (= (euclidian-rhythm 9 16)  [1 0 1 1 0 1 0 1 0 1 1 0 1 0 1 0])
  (= (euclidian-rhythm 11 24) [1 0 0 1 0 1 0 1 0 1 0 1 0 0 1 0 1 0 1 0 1 0 1 0])
  (= (euclidian-rhythm 13 24) [1 0 1 1 0 1 0 1 0 1 0 1 0 1 1 0 1 0 1 0 1 0 1 0])

  ;; the minor scale is the default rotation
  (= (euclidian-pattern 5 7 0 2 1) [2 1 2 2 1 2 2])
  ;; the major scale results when you rotate by 2
  (= (euclidian-pattern 5 7 2 2 1) [2 2 1 2 2 2 1])

  ;; scales for all the "white notes"
  (= (get-scale :a :aeolian)    '(:a :b :c :d :e :f :g :a))
  (= (get-scale :b :locrian)    '(:b :c :d :e :f :g :a :b))
  (= (get-scale :c :ionian)     '(:c :d :e :f :g :a :b :c))
  (= (get-scale :d :dorian)     '(:d :e :f :g :a :b :c :d))
  (= (get-scale :e :phrygian)   '(:e :f :g :a :b :c :d :e))
  (= (get-scale :f :lydian)     '(:f :g :a :b :c :d :e :f))
  (= (get-scale :g :mixolydian) '(:g :a :b :c :d :e :f :g))

  (= (get-scale :a :minor)      '(:a :b :c :d :e :f :g :a))
  (= (get-scale :c :major)      '(:c :d :e :f :g :a :b :c))

  ;; continuing, what about pentatonic scales?
  ;; xx  1 1x  2 2x  3  4 4x  5 5x  6 6x  7  8
  ;; Mp  1     2     3        5     6
  ;; MpI       2     2        3     2        3
  ;; mp  1       f3     4     5       f7
  ;; mpI          3     2     2        3     2
  ;; C   0  1  2  3  4  5  6  7  8  9  A  B  C
  ;;

  (= (euclidian-pattern 2 5 0 3 2) [3 2 2 3 2]) ; minor
  (= (euclidian-pattern 2 5 1 3 2) [2 2 3 2 3]) ; major

  )
