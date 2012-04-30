(ns explore_overtone.guitar)
;; Guitar by Roger Allen.
;; ----------------------------------------------------------------------
(do
  (use 'overtone.core)
  (connect-external-server 57110))

;; ----------------------------------------------------------------------
(do 
  ;; an array of 6 guitar strings: EADGBE
  (def guitar-string-tones
    [(note :e3) (note :a3) (note :d4) (note :g4) (note :b4) (note :e5)])

  ;; a map of chords to frets.  -1 indicates you don't play that string
  (def gchord-offset-map
    {:C   [ -1  3  2  0  1  0 ]
     :F   [ -1 -1  3  2  1  1 ]
     :G   [  3  2  0  0  0  3 ]
     :D   [ -1 -1  0  2  3  2 ]
     :A   [  0  0  2  2  2  0 ]
     :E   [  0  2  2  1  0  0 ]
     :Bb  [ -1 -1  3  3  3  1 ]
     :Eb  [ -1 -1  5  3  4  3 ]
     :Ab  [ -1 -1  6  5  4  4 ]
     :Db  [ -1 -1  3  1  2  1 ]
     :Gb  [ -1 -1  4  2  1  1 ]
     :B   [ -1 -1  4  4  4  2 ]
     
     :Cm  [ -1 -1  5  5  4  3 ]
     :Fm  [ -1 -1  3  1  1  1 ]
     :Gm  [ -1 -1  5  3  3  3 ]
     :Dm  [ -1  0  0  2  3  1 ]
     :Am  [  0  0  2  2  1  0 ]
     :Em  [  0  2  2  0  0  0 ]
     :Bbm [ -1 -1  3  3  2  1 ]
     :Ebm [ -1 -1  4  3  4  2 ]
     :Abm [ -1 -1  6  4  4  4 ]
     :Dbm [ -1 -1  2  1  2  0 ]
     :Gbm [ -1 -1  4  2  2  2 ]
     :Bm  [ -1 -1  4  4  3  2 ]
     
     :C7  [ -1  3  2  3  1 -1 ]
     :F7  [ -1 -1  1  2  1  1 ]
     :G7  [  3  2  0  0  0  1 ]
     :D7  [ -1 -1  0  2  1  2 ]
     :A7  [ -1 -1  2  2  2  3 ]
     :E7  [  0  2  0  1  0  0 ]
     :Bb7 [ -1 -1  3  3  3  4 ]
     :Eb7 [ -1 -1  1  3  2  3 ]
     :Ab7 [ -1 -1  1  1  1  2 ]
     :Db7 [ -1 -1  3  4  2  4 ]
     :Gb7 [ -1 -1  4  3  2  0 ]
     :B7  [ -1  2  1  2  0  2 ]
     })
  ;;(gchord-offset-map :Em)

  ;; given a fret-offset, add to the base note index with special
  ;; handling for -1
  (defn fret-offset
    [base fret]
    (if (>= fret 0)
      (+ base fret)
      fret))

  ;; given a chord symbol, get a list of tones for each string
  (defn get-tones
    [gchord]
    (map fret-offset guitar-string-tones (gchord-offset-map gchord)))
  ;;(get-tones :C)

  ;; the instrument
  (definst pluck-string
    [freq 440
     amp  1.0]
    (* (env-gen (perc 0.01 0.99) 1 1 0 1 FREE)
       (stk-pluck freq 1.0) amp))

  ;; play a tone if the tone is not -1
  (defn play-tone [tone]
    (if (> tone 0)
      (pluck-string (midi->hz tone))))

  ;; strum a chord on the guitar
  (defn strum [gchord]
    (map play-tone (get-tones gchord)))
)

;; works
(strum :E)

;; ----------------------------------------------------------------------
;; this doesn't work...why?
(defn strumming [ m beat ]
  (println "beat: " beat)
  (at (m (+ beat 0)) (strum :E))
  (at (m (+ beat 4)) (strum :G))
  (apply-at (m (+ beat 8)) #'strumming m (+ beat 8) []))

(def metro (metronome 120))
(strumming metro (metro))

;; ======================================================================
;; testcase

;; first setup simple instrument...
(definst goo [freq 440 amp 0.5 decay 0.6 ]
  (* (env-gen (perc 0.01 decay) 1 1 0 1 FREE)
     (sin-osc freq) amp))

(goo 440) ; see that it works

;; now try using at to schedule beats
(defn tst0 [ m beat ]
  (println "beat: " beat)
  (at (m (+ beat 1)) (goo 440))
  (at (m (+ beat 3)) (goo 400)))
;; this works...
(def metro (metronome 180))
(tst0 metro (metro))

;; now try something just a litle more complex...
(map goo [400 440]) ; "works"
;; again try using at to schedule beats
(defn tst1 [ m beat ]
  (println "beat: " beat)
  (at (m (+ beat 1)) (map goo [400 440])) ; "silent"
  (at (m (+ beat 3)) (goo 420)))
;; the first beat is missing -- why?
(tst1 metro (metro))

;; end testcase
;; ======================================================================

(goo 440)
(goo (midi->hz 69))

;; trying to make it more like above...
(defn ggoo [i]
  (goo (midi->hz i))
  (goo (midi->hz (+ i 1)))
  (goo (midi->hz (+ i 2)))
  (goo (midi->hz (+ i 3))))
(defn testmetro2 [ m beat ]
  (println "beat2: " beat)
  (at (m (+ beat 1)) (ggoo 69))
  (at (m (+ beat 3)) (ggoo 66)))
(testmetro2 metro (metro))

;;(m)

;; ======================================================================
;; cruft...
;; 
;;(demo 2 (stk-pluck (midicps (vec (chord :c4 :major)))))

;; (definst goo [freq 440 amp 0.5 decay 0.6 ]
;;   (* (env-gen (perc 0.01 decay) 1 1 0 1 FREE)
;;      (sin-osc freq) amp))
;; ;;(goo)
;; (def metro (metronome 120))
;; (defn testmetro [ m beat ]
;;   (println "beat: " beat)
;;   (at (m (+ beat 0)) (goo 440))
;;   (at (m (+ beat 1)) (goo 400))
;;   (apply-at (m (+ beat 2))
;;             #'testmetro m (+ beat 2) []))
;; (testmetro metro (metro))

;;(definst rpluck [tone 60 amp 0.5]
;;  (* (env-gen (perc 0.01 0.99) 1 1 0 1 FREE)
;;     (stk-pluck (midicps tone) 1.0) amp))
;;(rpluck 60)

;; play
;;(definst g [] (square))
;;(def gi (g))
;;(inst-fx gi fx-distortion)
;;(inst-fx gi fx-reverb)
;;(kill gi)

;; MIDI Control ==================================================
;;(do 
;;  (def kb (midi-in "iPad Wifi"))
;;  (defn midi-listener [event ts]
;;    (println "midi: " event))
;;  (midi-handle-events kb #'midi-listener))
