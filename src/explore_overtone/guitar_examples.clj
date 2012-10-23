(ns explore_overtone.guitar_examples)
(use 'overtone.live)
(use 'explore_overtone.guitar)

;; ======================================================================
;; try out the guitar...
(def g (guitar))
(ctl g :pre-amp 10.0 :amp 1.0 :distort 0.35) 
(strum g :E :down 0.25)
(strum g :E :up 0.75)
(strum g :B :down 0.25)
(strum g :A :up 0.5)
;; bow down to the power chord!
(ctl g :pre-amp 4.0 :distort 0.99) 
(strum g [0 2 2 -1 -1 -1])
(strum g [2 4 4 -1 -1 -1])
;; mute all strings
(strum g [-1 -1 -1 -1 -1 -1])
(ctl g :pre-amp 10.0 :amp 1.0 :distort 0.15)

;; ======================================================================
;; try out a bit of rhythmic accompanyment
;; http://www.youtube.com/watch?v=DV1ANPOYuH8
;; http://www.guitar.gg/strumming.html
;; FIXME -- these strumming patterns need a better treatment
(defn pat0 [metro cur-measure chord pattern]
  (let [cur-beat (* 4 cur-measure)]
    (doall
     (doseq [[b d] pattern]
       (strum g chord d 0.05 (metro (+ b cur-beat)))))))
(defn dduud [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [1.5 :up] [2.5 :up]
          [3.0 :down] ]))
(defn dduudu [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [1.5 :up] [2.5 :up]
          [3.0 :down]
          [3.5 :up] ]))
(defn ddudu [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [2.5 :up] [3.0 :down] [3.5 :up] ]))
(defn ddduduud [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [2.0 :down] [2.25 :up] [2.5 :down] [2.75 :up]
          [3.25 :up] [3.5 :down]]))
;; now we play...
(do ;; strumming practice
  (let [metro (metronome 100)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Gadd5 :Cadd9 :Cadd9
                                        :Dsus4 :Dsus4 :Gadd5 :Cadd9
                                        :Gadd5 :Cadd9])]
       (dduud metro i c))))
  )
(do ;; knocking on heaven's door
  (let [metro (metronome 100)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Dsus4 :Am :Am
                                        :Gadd5 :Dsus4 :Am :Am
                                        :Gadd5 :Dsus4 :Cadd9 :Cadd9])]
       (dduudu metro i c))))
  )
(do ;; moar strumming practice
  (let [metro (metronome 180)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Cadd9 :Gadd5 :Cadd9])]
       (ddudu metro i c))))
  )
(do ;; evan moar strumming practice
  (let [metro (metronome 90)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Cadd9 :Gadd5 :Cadd9])]
       (ddduduud metro i c))))
  )
  
