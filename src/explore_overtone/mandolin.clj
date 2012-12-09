;; A Mandolin by Roger Allen.
(ns explore-overtone.mandolin
  (:use [explore-overtone.stringed]
        [overtone.music pitch]
        [overtone.studio inst]
        [overtone.sc envelope node server ugens]
        [overtone.sc.cgens mix]))

;; a map of chords to frets.
;; -1 indicates you mute that string
;; -2 indicates you leave that string alone
(def mandolin-chord-frets
  {:A    [ 6  1  0  0 ]
   :A7   [ 2  2  4  3 ]
   :Am   [ 5  1  0  0 ]
   ;;:Am7

   :Bb   [ 3  0  1  1 ]
   :Bb7  [ 3  3  5  4 ]
   :Bbm  [ 3  3  4  6 ]
   ;;:Bbm7

   ;; FIXME ... finish rest
   :B    [ 4  3  2  2 ]
   :B7   [ 2  3  2  2 ]
   :Bm   [ 4  2  2  2 ]
   ;;:Bm7

   :C    [ 0  0  0  3 ]
   :C7   [ 0  0  0  1 ]
   :Cm   [ 0  3  3  3 ]
   ;;:Cm7 

   :Db   [ 1  1  1  4 ]
   :Db7  [ 1  1  1  2 ]
   :Dbm  [ 1  4  4  4 ]
   ;;:Dbm7

   :D    [ 2  2  2  0 ]
   :D7   [ 2  2  2  3 ]
   :Dm   [ 2  2  1  0 ]
   ;;:Dm7

   :Eb   [ 3  3  3  6 ]
   :Eb7  [ 3  3  3  4 ]
   :Ebm  [ 3  6  6  6 ]
   ;;:Ebm7

   :E    [ 4  4  4  7 ]
   :E7   [ 1  2  0  2 ]
   :Em   [ 0  4  3  2 ]
   ;;:Em7

   :F    [ 2  0  1  0 ]
   :F7   [ 2  3  1  0 ]
   :Fm   [ 1  0  1  3 ]
   ;;:Fm7

   :Gb   [ 3  1  2  1 ]
   :Gb7  [ 3  4  2  4 ]
   :Gbm  [ 2  1  2  0 ]
   ;;:Gbm7

   :G    [ 0  2  3  2 ]
   :G7   [ 0  2  1  2 ]
   :Gm   [ 0  2  3  1 ]
   ;;:Gm7

   :Ab   [ 5  3  4  3 ]
   :Ab7  [ 1  3  2  3 ]
   :Abm  [ 4  3  4  2 ]
   ;;:Abm7

   })

;; ======================================================================
;; an array of 4 mandolin strings: GDAE 
(def mandolin-string-notes (map note [:g3 :d4 :a4 :e5]))

;; ======================================================================
;; Main helper functions.  Use pick or strum to play the mandolin instrument.
(def pick (partial pick-string mandolin-string-notes))
(def strum (partial strum-strings mandolin-chord-frets mandolin-string-notes))

;; ======================================================================
;; Create the mandolin definst.  Now via the power of macros
(gen-stringed-inst mandolin 4)
