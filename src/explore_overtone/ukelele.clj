;; A Ukelele by Roger Allen.
(ns explore-overtone.ukelele
  (:use [explore-overtone.stringed]
        [overtone.music pitch]
        [overtone.studio inst]
        [overtone.sc envelope node server ugens]
        [overtone.sc.cgens mix]))

;; a map of chords to frets.
;; -1 indicates you mute that string
;; -2 indicates you leave that string alone
(def ukelele-chord-frets
  {:A    [ 2  1  0  0 ]
   :A7   [ 0  1  0  0 ]
   :Am   [ 2  0  0  0 ]
   :Am7  [ 0  0  0  0 ]

   :Bb   [ 3  2  1  1 ]
   :Bb7  [ 1  2  1  1 ]
   :Bbm  [ 3  1  1  1 ]
   :Bbm7 [ 1  1  1  1 ]

   :B    [ 4  3  2  2 ]
   :B7   [ 2  3  2  2 ]
   :Bm   [ 4  2  2  2 ]
   :Bm7  [ 2  2  2  2 ]

   :C    [ 0  0  0  3 ]
   :C7   [ 0  0  0  1 ]
   :Cm   [ 0  3  3  3 ]
   :Cm7  [ 3  3  3  3 ]

   :Db   [ 1  1  1  4 ]
   :Db7  [ 1  1  1  2 ]
   :Dbm  [ 1  4  4  4 ]
   :Dbm7 [ 4  4  4  4 ]

   :D    [ 2  2  2  0 ]
   :D7   [ 2  2  2  3 ]
   :Dm   [ 2  2  1  0 ]
   :Dm7  [ 2  2  1  3 ]

   :Eb   [ 3  3  3  6 ]
   :Eb7  [ 3  3  3  4 ]
   :Ebm  [ 3  6  6  6 ]
   :Ebm7 [ 3  3  2  4 ]

   :E    [ 4  4  4  7 ]
   :E7   [ 1  2  0  2 ]
   :Em   [ 0  4  3  2 ]
   :Em7  [ 0  2  0  2 ]

   :F    [ 2  0  1  0 ]
   :F7   [ 2  3  1  0 ]
   :Fm   [ 1  0  1  3 ]
   :Fm7  [ 2  2  1  3 ]

   :Gb   [ 3  1  2  1 ]
   :Gb7  [ 3  4  2  4 ]
   :Gbm  [ 2  1  2  0 ]
   :Gbm7 [ 2  4  2  4 ]

   :G    [ 0  2  3  2 ]
   :G7   [ 0  2  1  2 ]
   :Gm   [ 0  2  3  1 ]
   :Gm7  [ 0  2  1  1 ]

   :Ab   [ 5  3  4  3 ]
   :Ab7  [ 1  3  2  3 ]
   :Abm  [ 4  3  4  2 ]
   :Abm7 [ 1  3  2  2 ]

   })

;; ======================================================================
;; an array of 4 ukelele strings: GCEA (order low-to-hi is CEGA)
(def ukelele-string-notes (map note [:g4 :c4 :e4 :a4]))

;; ======================================================================
;; Main helper functions.  Use pick or strum to play the ukelele instrument.
(def pick (partial pick-string ukelele-string-notes))
(def strum (partial strum-strings ukelele-chord-frets ukelele-string-notes))

;; ======================================================================
;; Create the ukelele definst.  Now via the power of macros
(gen-stringed-inst ukelele 4)
