(ns explore-overtone.bach-cello-suite-1
  (:require [overtone.live :as o]
            [leipzig.scale :as ls]
            [leipzig.melody :as lm]
            [leipzig.live :as ll]))

;; Check out http://costanzabach.stanford.edu/commentary

;; M-x iimage-mode
(comment just a snippet...
  <bach_cello_suite_1_snip.png>
;; I used this handy guide to enumerate all the notes...

str n  i staff
A   D 13
A   C 12  -- 12.5=C#
A   B 11
A...A 10----- midi-index=57
D   G  9
D   F# 8-----
D   E  7
D...D  6----- midi-index=50
G   C  5      5.5=C#
G   B  4-----
G   A  3
G...G  2----- midi-index=42
C   F  1
C   E  0  --
    D -1

as for scales...
Gmaj has F#
Dmaj has F# and C#

)
;; wish there was M-x translate-image-into-notes...
(def raw-pitch-data
  [[2   6  11] 10   11  6 11  6 ; 1) line 1 bar 1
  ;;G   D   A   A    A  D  A  D = string that plays the notes above
  ;;since G & D resonate while A plays, they create chords
   [2   6  11] 10   11  6 11  6
  ;;G   D   A   A    A  D  A  D
   [2   7  12] 11   12  7 12  7 ; 2) line 1 bar 2
  ;;G   D   A   A    A  D  A  D
   [2   7  12] 11   12  7 12  7
  ;;G   D   A   A    A  D  A  D
   [2   8  12] 11   12  8 12  8 ; 3) line 1 bar 3
  ;;G   D   A   A    A  D  A  D
   [2   8  12] 11   12  8 12  8
  ;;G   D   A   A    A  D  A  D
   [2   9  11] 10   11  9 11  9 ; 4) line 2 bar 1
  ;;G   D   A   A    A  D  A  D
   [2   9  11] 10   11  9 11  8
  ;;G   D   A   A    A  D  A  D
   [2   7  11] 10   11  9  8  9 7 9 8 9 4
  ;;G   D   A   A    A  D  A  D D D D D G
   ;; I think *here* is where it switches to Dmaj
    6 5.5 4 ; 5) line 2 bar 2
  ;;D G   G
   [5.5 9  10]  9   10  9 10  9 ; 6) line 2 bar 3
  ;;G   D   A   D    A  D  A  D
   [5.5 9  10]  9   10  9 10  9
  ;;G   D   A   D    A  D  A  D
   [8  10  13] 12.5 13 10  9 10 8 10 9 10 6 8 7 6 ; 7) line 2 bar 4
  ;;D   A   A?  A    A  A  D  A D  A D  A D D D D
   [0   4   9]  8    9  4  9  4 ; 8) line 3 bar 1
  ;;C!  G   D   D    D  G  D  G
   [0   4   9]  8    9  4  9  4
  ;;C   G   D   D    D  G  D  G
    0  5.5  6   7    6  5  4  3 ; 9) line 3 bar 2
  ;;C   G   D   D    D  G  G  G
   [9   8   7] 13 12.5 11 10  9
  ;;D   D   D  A     A  A  A  D
   [8   7   6] 13   10 13  8 10 ; 10) line 3 bar 3
  ;;D   D   D  A     A  A  D  A
   [6   7   8] 10    9  8  7  6])
  ;;D   D   D  A     D  D  D  D

;; xform via (clojure.walk/prewalk #(if (number? %) (- % 2) %) g-maj-pitches)
(def g-maj-pitches
  [0 4 9 8 9 4 9 4
   0 4 9 8 9 4 9 4
   0 5 10 9 10 5 10 5
   0 5 10 9 10 5 10 5
   0 6 10 9 10 6 10 6
   0 6 10 9 10 6 10 6
   0 7 9 8 9 7 9 7
   0 7 9 8 9 7 9 6
   0 5 9 8 9 7 6 7
   5 7 6 7 2])

(def d-maj-pitches
  [7 6 5
   6 10 11 10 11 10 11 10
   6 10 11 10 11 10 11 10
   9 11 14 13 14 11 10 11 9 11 10 11 7 9 8 7
   1 5 10 9 10 5 10 5
   1 5 10 9 10 5 10 5 1 6 7 8 7 6 5 4
   10 9 8 14 13 12 11 10
   9 8 7 14 11 14 9 11
   7 8 9 11 10 9 8 7])

(def g-phrase (lm/phrase (repeat 1) g-maj-pitches))
(def d-phrase (lm/phrase (repeat 1) d-maj-pitches))
(def the-phrase (->> (->> g-phrase
                          (lm/where :pitch (comp ls/G ls/major)))
                     (lm/then
                      (->> d-phrase
                           (lm/where :pitch (comp ls/D ls/major))))))

(o/defsynth cello
  "cello inspired by Sound On Sound April-July 2003 articles on violin."
  [pitch   {:default 31  :min 0   :max 127 :step 1}
   amp     {:default 1.0 :min 0.0 :max 1.0 :step 0.01}
   gate    {:default 1   :min 0   :max 1   :step 1}
   out-bus {:default 0   :min 0   :max 127 :step 1}]
  (let [freq   (o/midicps pitch)
        ;; 3b) portamento to change frequency slowly
        freqp  (o/slew:kr freq 100.0 100.0)
        ;; 3a) vibrato to make it seem "real"
        freqv  (o/vibrato :freq freqp :rate 6 :depth 0.01 :delay 1.0)
        ;; 1) the main osc for the violin
        saw    (o/saw freqv)
        ;; 2) add an envelope for "bowing"
        saw0   (* saw (o/env-gen (o/adsr 1.5 1.5 0.8 1.5) :gate gate :action o/FREE))
        ;; a low-pass filter prior to our filter bank
        saw1   (o/lpf saw0 4200) ;; freq???
        ;; 4) the "formant" filters
        band1  (* 0.8 (o/bpf saw1  97.999 (/ 3.5))) ;; going to make this a "G"
        band2  (* 0.9 (o/bpf saw1 293.67 (/ 3.5))) ;; and this is an "D"
        band3  (* 1.2 (o/bpf saw1 880.0 (/ 2)))   ;; an "A"
        saw2   (o/mix [band1 band2 band3])
        ;; a high-pass filter on the way out
        saw3   (o/hpf saw2 30) ;; freq???
        ]
    (o/out out-bus (* amp saw3))))
;;(def c (cello :position :head :pitch 42 :out-bus b0))
;;(o/ctl c :gate 0)

(o/defsynth effect [out-bus 0 in-bus 1
                    mix 0.33 room 0.5 damp 0.5]
                    ;;roomsize 20 revtime 0.5 damping 0.15 inputbw 0.19
                    ;;drylevel 0 earlyreflevel -60 taillevel -60]
  (o/out out-bus (o/pan2
                  (o/free-verb (o/in in-bus 1) :mix mix :room room :damp damp)
                  ;;(o/g-verb
                  ;; (o/in in-bus 1)
                  ;; :roomsize roomsize :revtime revtime :damping damping :inputbw inputbw
                  ;; :drylevel drylevel :earlyreflevel earlyreflevel :taillevel taillevel)
                  )))

(def b0 (o/audio-bus 1))

(def e (effect :position :tail :in-bus b0))
;;(o/ctl e :revtime 0.25 :damping 0.95)
;;(def c (cello :position :head :pitch 42 :out-bus b0))
;;(o/ctl c :gate 0)

(defmethod ll/play-note :default [{:keys [pitch time duration]}]
  (let [cid (cello :position :head :pitch (- pitch 24) :out-bus b0)]
    (o/at (+ time duration) (o/ctl cid :gate 0))))

(comment

  (ll/play (->> the-phrase
                (lm/where :time (lm/bpm 200))
                (lm/where :duration (lm/bpm 200))))

  (o/stop)
)
