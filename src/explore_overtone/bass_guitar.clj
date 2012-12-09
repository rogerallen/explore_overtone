;; A Guitar by Roger Allen.
(ns explore-overtone.bass-guitar
  (:use [explore-overtone.stringed]
        [overtone.music pitch]      
        [overtone.studio inst]
        [overtone.sc envelope node server ugens]
        [overtone.sc.cgens mix]))

;; a map of chords to frets.  This is not all chords, just some of
;; them as there are many alternatives to choose from.
;; You can pass in your own arrays to strum, too.
;; -1 indicates you mute that string
;; -2 indicates you leave that string alone
(def bass-guitar-chord-frets
  ;; FIXME ... these are just the guitar versions
  {:A    [ -1  0  2  2 ]
   :A7   [ -1  0  2  0 ]
   :A9   [  0  0  2  4 ]
   :Am   [  0  0  2  2 ]
   :Am7  [  0  0  2  0 ]

   :Bb   [ -1  1  3  3 ]
   :Bb7  [ -1 -1  3  3 ]
   :Bb9  [ -1 -1  0  1 ]
   :Bbm  [ -1 -1  3  3 ]
   :Bbm7 [  1  1  3  1 ]

   :B    [ -1 -1  4  4 ]
   :B7   [ -1  2  1  2 ]
   :B9   [  2 -1  1  2 ]
   :Bm   [ -1 -1  4  4 ]
   :Bm7  [ -1  2  0  2 ]

   :C    [ -1  3  2  0 ]
   :C7   [ -1  3  2  3 ]
   :C9   [  3  3  2  3 ]
   :Cm   [  3  3  5  5 ]
   :Cm7  [  3  3  5  3 ]

   :Db   [ -1 -1  3  1 ]
   :Db7  [ -1 -1  3  4 ]
   :Db9  [  4 -1  3  4 ]
   :Dbm  [ -1 -1  2  1 ]
   :Dbm7 [ -1  3  2  1 ]

   :D    [ -1 -1  0  2 ]
   :D7   [ -1 -1  0  2 ]
   :D9   [ -1 -1  4  2 ]
   :Dm   [ -1  0  0  2 ]
   :Dm7  [ -1 -1  0  2 ]

   :Eb   [ -1 -1  5  3 ]
   :Eb7  [ -1 -1  1  3 ]
   :Eb9  [ -1 -1  1  0 ]
   :Ebm  [ -1 -1  4  3 ]
   :Ebm7 [ -1 -1  1  3 ]

   :E    [  0  2  2  1 ]
   :E7   [  0  2  0  1 ]
   :E9   [  0  2  0  1 ]
   :Em   [  0  2  2  0 ]
   :Em7  [  0  2  2  0 ]

   :F    [  1  3  3  2 ]
   :F7   [  1 -1  2  2 ]
   :F9   [  1  0  3  0 ]
   :Fm   [  1  3  3  1 ]
   :Fm7  [  1  3  3  1 ]

   :Gb   [  2  4  4  3 ]
   :Gb7  [ -1 -1  4  3 ]
   :Gb9  [ -1  4 -1  3 ]
   :Gbm  [  2  4  4  2 ]
   :Gbm7 [  2 -1  2  2 ]

   :G    [  3  2  0  0 ]
   :G7   [  3  2  0  0 ]
   :G9   [ -1 -1  0  2 ]
   :Gm   [ -1 -1  5  3 ]
   :Gm7  [ -1  1  3  0 ]

   :Ab   [ -1 -1  6  5 ]
   :Ab7  [ -1 -1  1  1 ]
   :Ab9  [ -1 -1  1  3 ]
   :Abm  [ -1 -1  6  4 ]
   :Abm7 [ -1 -1  4  4 ]

   :Gadd5 [  3  2  0  0 ]
   :Cadd9 [ -1  3  2  0 ]
   :Dsus4 [ -1 -1  0  2 ]

   })

;; ======================================================================
;; an array of 6 bass-guitar strings: EADGBE
(def bass-guitar-string-notes (map note [:e1 :a1 :d2 :g2]))

;; ======================================================================
;; Main helper functions.  Use pick or strum to play the ukelele instrument.
(def pick (partial pick-string bass-guitar-string-notes))
(def strum (partial strum-strings bass-guitar-chord-frets bass-guitar-string-notes))

;; ======================================================================
;; Create the bass-guitar definst.  Now via the power of macros
(gen-stringed-inst bass-guitar 4)
