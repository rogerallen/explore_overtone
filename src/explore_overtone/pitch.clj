(ns ^{:doc "This is the place for functions representing general
    musical knowledge, like scales, chords, intervals, etc."
      :author "Roger Allen (from overtone.music.pitch by Jeff Rose, Sam Aaron & Marius Kempe)"}
  explore-overtone.pitch
  (require [clojure.set :as set]))

;; this code is basically the overtone.music.pitch, but refactored to
;; match my own sensibilities.
;;
;; Definitions for consistency:
;;   deg       = degree or index into a seq of pitches.
;;   field     = seq of 8 octaves of pitches
;;   freq      = frequency in cycles per second aka Hz
;;   interval  = index into chromatic scale               FIXME? semitone?
;;   Δinterval = seq of steps between pitches of a scale  FIXMĔ̆?
;;   key       = midi pitch string or keyword without octave (e.g. :C or "C")
;;   name      = midi pitch string or keyword (e.g. "C4" or :C4)
;;   octint    = [octave interval] chromatic pitch description tuple
;;   pitch     = midi pitch integer values
;;   scale     = keyword describing a type of scale (e.g. :major)
;;
;; Note that using degree values with a radix having the same base as
;; the number of notes in the scale can be convenient (e.g major scale
;; 7r40 gives starting point of 4th octave) when indexing into the
;; field.

(def KEY-PITCH {:C  0  :c  0  :b# 0  :B# 0
                :C# 1  :c# 1  :Db 1  :db 1  :DB 1  :dB 1
                :D  2  :d  2
                :D# 3  :d# 3  :Eb 3  :eb 3  :EB 3  :eB 3
                :E  4  :e  4
                :E# 5  :e# 5  :F  5  :f  5
                :F# 6  :f# 6  :Gb 6  :gb 6  :GB 6  :gB 6
                :G  7  :g  7
                :G# 8  :g# 8  :Ab 8  :ab 8  :AB 8  :aB 8
                :A  9  :a  9
                :A# 10 :a# 10 :Bb 10 :bb 10 :BB 10 :bB 10
                :B  11 :b  11 :Cb 11 :cb 11 :CB 11 :cB 11})

(def PITCH-KEY {0  :C
                1  :C#
                2  :D
                3  :D#
                4  :E
                5  :F
                6  :F#
                7  :G
                8  :G#
                9  :A
                10 :A#
                11 :B})

(def INTERVAL-SEMITONE
  {:perfect-unison 0  :diminished-second  0
   :minor-second   1  :augmented-unison   1
   :major-second   2  :diminished-third   2
   :minor-third    3  :augmented-second   3
   :major-third    4  :diminished-fourth  4
   :perfect-fourth 5  :augmented-third    5
   :tritone        6  :augmented-fourth   6  :diminished-fifth 6
   :perfect-fifth  7  :diminished-sixth   7
   :minor-sixth    8  :augmented-fifth    8
   :major-sixth    9  :diminished-seventh 9
   :minor-seventh  10 :augmented-sixth    10
   :major-seventh  11 :diminished-octave  11
   :perfect-octave 12 :augmented-seventh  12 })

(defn key->pitch
  "given key string or key keyword, return pitch"
  [k]
  (let [k (keyword k)
        p (KEY-PITCH k)]
    p))

(defn pitch->key
  "given pitch, return key string"
  [p]
  (name (PITCH-KEY (mod p 12))))

(defn- rotate
  [xs offset]
  (take (count xs) (drop offset (cycle xs))))
(rotate '(1 2 3) 1)

(def scale->Δintervals
  (let [ionian-sequence     [2 2 1 2 2 2 1]
        hex-sequence        [2 2 1 2 2 3]
        pentatonic-sequence [3 2 2 3 2]]
    {;; 12 tones per scale
     :chromatic          [1 1 1 1 1 1 1 1 1 1 1 1]
     ;; 8 tones
     :octatonic          [2 1 2 1 2 1 2 1]
     :diminished         [1 2 1 2 1 2 1 2]
     :diminished2        [2 1 2 1 2 1 2 1]
     ;; 7 tones
     :diatonic           ionian-sequence
     :ionian             (rotate ionian-sequence 0)
     :major              (rotate ionian-sequence 0)
     :dorian             (rotate ionian-sequence 1)
     :phrygian           (rotate ionian-sequence 2)
     :lydian             (rotate ionian-sequence 3)
     :mixolydian         (rotate ionian-sequence 4)
     :dominant           (rotate ionian-sequence 4)
     :aeolian            (rotate ionian-sequence 5)
     :minor              (rotate ionian-sequence 5)
     :locrian            (rotate ionian-sequence 6)
     :ahirbhairav        [1 3 1 2 2 1 2]
     :bartok             [2 2 1 2 1 2 2]
     :bhairav            [1 3 1 2 1 3 1]
     :enigmatic          [1 3 2 2 2 1 1]
     :harmonic-major     [2 2 1 2 1 3 1]
     :harmonic-minor     [2 1 2 2 1 3 1]
     :hindu              [2 2 1 2 1 2 2]
     :hungarian-minor    [2 1 3 1 1 3 1]
     :leading-whole      [2 2 2 2 2 1 1]
     :locrian-major      [2 2 1 1 2 2 2]
     :lydian-minor       [2 2 2 1 1 2 2]
     :marva              [1 3 2 1 2 2 1]
     :melodic-major      [2 2 1 2 1 2 2]
     :melodic-minor      [2 1 2 2 2 2 1]
     :melodic-minor-asc  [2 1 2 2 2 2 1]
     :melodic-minor-desc [2 1 2 2 1 2 2]
     :neapolitan-major   [1 2 2 2 2 2 1]
     :neapolitan-minor   [1 2 2 2 1 3 1]
     :purvi              [1 3 2 1 1 3 1]
     :romanian-minor     [2 1 3 1 2 1 2]
     :spanish            [1 3 1 2 1 2 2]
     :super-locrian      [1 2 1 2 2 2 2]
     :todi               [1 2 3 1 1 3 1]
     ;; 6 tones
     :hex-major6         (rotate hex-sequence 0)
     :hex-dorian         (rotate hex-sequence 1)
     :hex-phrygian       (rotate hex-sequence 2)
     :hex-major7         (rotate hex-sequence 3)
     :hex-sus            (rotate hex-sequence 4)
     :hex-aeolian        (rotate hex-sequence 5)
     :whole-tone         [2 2 2 2 2 2]
     :whole              [2 2 2 2 2 2]
     :augmented          [3 1 3 1 3 1]
     :augmented2         [1 3 1 3 1 3]
     ;; 5 tones
     :minor-pentatonic   (rotate pentatonic-sequence 0)
     :yu                 (rotate pentatonic-sequence 0)
     :major-pentatonic   (rotate pentatonic-sequence 1)
     :gong               (rotate pentatonic-sequence 1)
     :egyptian           (rotate pentatonic-sequence 2)
     :shang              (rotate pentatonic-sequence 2)
     :jiao               (rotate pentatonic-sequence 3)
     :pentatonic         (rotate pentatonic-sequence 4) ;; historical match
     :zhi                (rotate pentatonic-sequence 4)
     :ritusen            (rotate pentatonic-sequence 4)
     :chinese            [4 2 1 4 1]
     :hirajoshi          [2 1 4 1 4]
     :indian             [4 1 2 3 2]
     :iwato              [1 4 1 4 2]
     :kumoi              [2 1 4 2 3]
     :pelog              [1 2 4 1 4]
     :prometheus         [2 2 2 5 1]
     :scriabin           [1 3 3 2 3]
     ;; messiaen
     :messiaen1          [2 2 2 2 2 2]
     :messiaen2          [1 2 1 2 1 2 1 2]
     :messiaen3          [2 1 1 2 1 1 2 1 1]
     :messiaen4          [1 1 3 1 1 1 3 1]
     :messiaen5          [1 4 1 1 4 1]
     :messiaen6          [2 2 1 1 2 2 1 1]
     :messiaen7          [1 1 1 2 1 1 1 1 2 1]
     }))

(defn scale->field
  "Create the pitch field for a given scale.  Scales are specified
  with a keyword representing the scale and optional key (defaulting to :C):
  (scale->field :dominant)
  (scale->field :g :minor)"
  ([scale-key scale-name]
     (let [base       (key->pitch scale-key)
           scale-name (or scale-name :major)
           Δintervals  (scale->Δintervals scale-name)]
       (reverse (next
                 (reduce (fn [mem interval]
                           (let [new-note (+ (first mem) interval)]
                             (conj mem new-note)))
                         (list base)
                         (take (* 8 (count Δintervals)) (cycle Δintervals)))))))
  ([scale-name]
     (scale->field :C scale-name)))

(def MIDI-PITCH-RE-STR "([a-gA-G][#bB]?)([-0-9]+)" )
(def MIDI-PITCH-RE (re-pattern MIDI-PITCH-RE-STR))
(def ONLY-MIDI-PITCH-RE (re-pattern (str "\\A" MIDI-PITCH-RE-STR "\\Z")))

(defn- name-matcher
  "Determines whether a midi keyword is valid or not. If valid,
  returns a regexp match object"
  [mk]
  (re-find ONLY-MIDI-PITCH-RE (name mk)))

(defn- validate-name!
  "Throws a friendly exception if midi-keyword mk is not
  valid. Returns [key octave] if valid."
  [mk]
  (let [matches (name-matcher mk)]
    (when-not matches
      (throw (IllegalArgumentException.
              (str "Invalid name. " mk
                   " does not appear to be in MIDI format i.e. C#4"))))

    (let [[match key octave] matches]
      (when (< (Integer. octave) -1)
        (throw (IllegalArgumentException.
                (str "Invalid name: " mk
                     ". Octave is out of range. Lowest octave value is -1"))))
      [key octave])))

(defn octint->pitch
  "Convert an [octave, interval] tuple to a midi pitch."
  [[octave interval]]
  (+ (* octave 12) interval 12))

(defn name->octint
  "convert a name to [octave, interval] tuple"
  [nstr]
  (let [[intstr octstr] (validate-name! nstr)
        oct             (Integer. octstr)]
    [oct (key->pitch (keyword intstr))]))

(defn name->pitch
  "Resolves pitch name to MIDI number format. Resolves upper and lower-case
  keywords and strings in MIDI note format. If given an integer or
  nil, returns them unmodified. All other inputs will raise an
  exception.

  Usage examples:

  (pitch \"C4\")  ;=> 60
  (pitch \"C#4\") ;=> 61
  (pitch \"eb2\") ;=> 39
  (pitch :F#7)    ;=> 102
  (pitch :db5)    ;=> 73
  (pitch 60)      ;=> 60
  (pitch nil)     ;=> nil"
  [n]
  (cond
    (nil? n)     nil
    (integer? n) (if (>= n 0)
                   n
                   (throw (IllegalArgumentException.
                           (str "Unable to resolve pitch: "
                                n
                                ". Value is out of range. Lowest value is 0"))))
    (keyword? n) (name->pitch (name n))
    (string? n)  (octint->pitch (name->octint n))
    :else        (throw (IllegalArgumentException.
                         (str "Unable to resolve pitch: " n
                              ". Wasn't a recognised format (either an integer, keyword, string or nil)")))))

(defn deg->pitch
  "given a scale-field and degree, pitch value"
  [scale-field deg]
  (nth scale-field deg))

(defn pitch->freq
  "Convert a midi pitch (i) to a frequency in Hz.  Use midi pitch 69 =
  A5 = 440 Hz"
  [i]
  (* 440.0 (java.lang.Math/pow 2.0 (/ (- i 69.0) 12.0))))

(defn freq->pitch
  "Convert from a frequency to the nearest midi pitch number.  Use midi
  pitch 69 = A5 = 440 Hz"
  [f]
  (java.lang.Math/round (+ 69
                 (* 12
                    (/ (java.lang.Math/log (* f (/ 440.0)))
                       (java.lang.Math/log 2))))))

(defn pitch->octave
  "given pitch, find the midi octave starting with -1"
  [i]
  (- (int (/ i 12)) 1))

(defn pitch->name
  "given pitch, find the name name"
  [i]
  (let [base-key (pitch->key i)
        octave (pitch->octave i)]
    (str (name base-key) octave)))

;; ======================================================================
;; chords (not finished)
(defn deg->chord
  "basic chord function.  Given a scale-field and degree, return the
  pitches of a chord.  num gives monad, dyad, triad, tetrad, etc,
  correlating to chords 5th, 7th, 9th, 11th, etc."
  [num field deg]
  (let [ds (map #(+ deg (* 2 %)) (range num))
        pitches (map #(deg->pitch field %) ds)]
    pitches))

(def chord->intervals
  {:major       (deg->chord 3 (scale->field :major) 0)
   :maj         (deg->chord 3 (scale->field :major) 0)
   :M           (deg->chord 3 (scale->field :major) 0)
   :dom         (deg->chord 3 (scale->field :dominant) 0)
   :minor       (deg->chord 3 (scale->field :minor) 0)
   :m           (deg->chord 3 (scale->field :minor) 0)
   :augmented   (deg->chord 3 (scale->field :augmented) 0)
   :a           (deg->chord 3 (scale->field :augmented) 0)
   :diminished  (deg->chord 3 (scale->field :diminished) 0)
   :dim         (deg->chord 3 (scale->field :diminished) 0)
   :i           (deg->chord 3 (scale->field :diminished) 0)
   :major7      (deg->chord 4 (scale->field :major) 0)
   :maj7        (deg->chord 4 (scale->field :major) 0)
   :M7          (deg->chord 4 (scale->field :major) 0)
   :dom7        (deg->chord 4 (scale->field :dominant) 0)
   :7           (deg->chord 4 (scale->field :dominant) 0)
   :minor7      (deg->chord 4 (scale->field :minor) 0)
   :m7          (deg->chord 4 (scale->field :minor) 0)
   :augmented7  (deg->chord 4 (scale->field :augmented) 0)
   :a7          (deg->chord 4 (scale->field :augmented) 0)
   :diminished7 (deg->chord 4 (scale->field :diminished) 0)
   :dim7        (deg->chord 4 (scale->field :diminished) 0)
   :i7          (deg->chord 4 (scale->field :diminished) 0)
   :9           (deg->chord 5 (scale->field :dominant) 0)
   :m9          (deg->chord 5 (scale->field :minor) 0)
   :maj9        (deg->chord 5 (scale->field :major) 0)
   :M9          (deg->chord 5 (scale->field :major) 0)
   :11          (deg->chord 6 (scale->field :dominant) 0)
   :m11         (deg->chord 6 (scale->field :minor) 0)
   :maj11       (deg->chord 6 (scale->field :major) 0)
   :M11         (deg->chord 6 (scale->field :major) 0)
   :13          (deg->chord 7 (scale->field :dominant) 0)
   :m13         (deg->chord 7 (scale->field :minor) 0)
   :maj13       (deg->chord 7 (scale->field :major) 0)
   :M13         (deg->chord 7 (scale->field :major) 0)
   ;; FIXME Want a more-elegant way to construct these
   :+5        '(0 4 8)
   :m+5       '(0 3 8)
   :sus2      '(0 2 7)
   :sus4      '(0 5 7)
   :6         '(0 4 7 9)
   :m6        '(0 3 7 9)
   :7sus2     '(0 2 7 10)
   :7sus4     '(0 5 7 10)
   :7-5       '(0 4 6 10)
   :m7-5      '(0 3 6 10)
   :7+5       '(0 4 8 10)
   :m7+5      '(0 3 8 10)
   :m7+9      '(0 3 7 10 14)
   :9sus4     '(0 5 7 10 14)
   :6*9       '(0 4 7 9 14)
   :m6*9      '(0 3 9 7 14)
   :7-9       '(0 4 7 10 13)
   :m7-9      '(0 3 7 10 13)
   :7-10      '(0 4 7 10 15)
   :9+5       '(0 10 13)
   :m9+5      '(0 10 14)
   :7+5-9     '(0 4 8 10 13)
   :m7+5-9    '(0 3 8 10 13)
   :11+       '(0 4 7 10 14 18)
   :m11+      '(0 3 7 10 14 18)
   })

(defn intervals->chord
  "return chord pitches given a tonic name and interval sequence."
  [tonic intervals]
  (map #(pitch->key (+ (name->pitch tonic) %)) intervals))

;; FIXME add fn to take intervals & return chord name
;; (fn '(0 4 7)) -> "major"

(defn chord-invert
  "invert a chord.  Well behaved for 1 <= n < (count chord)."
  [chord n]
  (let [ichord (rotate chord n)
        alter0 (take n (repeat 0))
        alter12 (take (- (count chord) n) (repeat -12))]
    (map + ichord (concat alter12 alter0))))

;; ======================================================================
(comment
  ;; testing & general usage
  (key->pitch :e)
  (key->pitch "e")
  (key->pitch "e4")

  (pitch->key 2)
  (pitch->key 14)

  (scale->Δintervals :major)

  (scale->field :d# :major)
  (take (count (scale->Δintervals :major)) (scale->field :C :major))
  (map pitch->key (take (count (scale->Δintervals :major)) (scale->field :F :major)))

  (validate-name! "C#-1")
  (validate-name! "Db3")

  (octint->pitch [4 0])
  (pitch->name 60)
  (name->octint "C4")

  (deg->pitch (scale->field :C :minor) 7r50)
  (map (partial deg->pitch (scale->field :C :minor)) (range 7r50 7r60))
  (map (partial deg->pitch (scale->field :C :pentatonic)) (range 5r100 5r110))

  (pitch->freq 69)
  (freq->pitch 220)
  (freq->pitch (pitch->freq 69))

  (pitch->name (freq->pitch 220))
  (freq->pitch 230)
  (pitch->name (freq->pitch 230))
  (name->pitch (pitch->name (freq->pitch 230)))
  (pitch->freq (name->pitch (pitch->name (freq->pitch 230))))

  (pitch->octave 60)
  (pitch->octave 59)

  (name->pitch "C4")  ;=> 60
  (name->pitch "C#4") ;=> 61
  (name->pitch "eb2") ;=> 39
  (name->pitch :F#7)  ;=> 102
  (name->pitch :db5)  ;=> 73
  (name->pitch 60)    ;=> 60
  (name->pitch nil)   ;=> nil"

  (take 7 (scale->field :C :major))
  (take 7 (scale->field :C :minor))
  (map #(-> (deg->pitch (scale->field :C :major) %)
            pitch->name)
       (range 7r10 7r20))

  (deg->chord 4 (scale->field :C :major) 0)
  (chord-invert (deg->chord 4 (scale->field :C :major) 7r10) 3)
  (chord->intervals :7)
  (chord->intervals :M11)

  (dotimes [i 7]
    (let [the-chord (deg->chord 3 (scale->field :c :minor) i)
          start-deg (first the-chord)
          alt-chord (map #(- % start-deg) the-chord)]
      (println i the-chord alt-chord)))
  ;; 0 (0 4 7) (0 4 7) = major
  ;; 1 (2 5 9) (0 3 7) = minor
  ;; 2 (4 7 11) (0 3 7) = minor
  ;; 3 (5 9 12) (0 4 7) = major
  ;; 4 (7 11 14) (0 4 7) = major
  ;; 5 (9 12 16) (0 3 7) = major
  ;; 6 (11 14 17) (0 3 6) = dim

  )
