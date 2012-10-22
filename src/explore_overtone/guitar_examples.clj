(ns explore_overtone.guitar_examples)
(use 'overtone.live)
(use 'explore_overtone.guitar)

;; ======================================================================
;; try out the guitar...
(ctl guitar :pre-amp 4.0) ;; higher values with no distortion, lower with dist
(ctl guitar :amp 1.0)
(ctl guitar :distort 0.0) ;; try without distortion, then add it in later
;; try :distort 0.95, :pre-amp 1.0
;; ??? what destructive interference is happening?
;; ??? why does it get "quiet" with distortion?
(strum-now :E :down 0.5)
(strum-now :E :up 0.75)
(strum-now :B :down 0.25)
(strum-now :A :up 0.5)
;; mute all strings
(strum-now [-1 -1 -1 -1 -1 -1] :down 0.001)

;; a little strumming pattern fun
;; http://www.youtube.com/watch?v=DV1ANPOYuH8
;; http://www.guitar.gg/strumming.html
(defn pat0 [metro cur-measure chord pattern]
  (let [cur-beat (* 4 cur-measure)]
    (doall
     (doseq [[b d] pattern]
       (strum-at (metro (+ b cur-beat)) chord d)))))
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
(do ;; evin moar strumming practice
  (let [metro (metronome 90)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Cadd9 :Gadd5 :Cadd9])]
       (ddduduud metro i c))))
  )



;; ======================================================================
(defsynth string-test-synth
  [note {:default 60}
   gate {:default 1}]
  (let [frq (midicps note)
        dur 10.0
        decay 30
        coef 0.3
        nze  (* 0.8 (pink-noise))
        dly  (/ 1.0 frq)
        plk  (pluck nze gate (/ 1.0 8.0) dly decay coef)
        snd   (* plk (env-gen (asr 0.0001 1 0.1) :gate gate ))]
    (out 0 (pan2 snd 0))))
(def t0 (string-test-synth))
(do
  (let [_ (recording-start "~/test.wav")
        _ (Thread/sleep 500)
        n (now)]
    (at (+ n   10) (ctl t0 :note 60 :gate 1))
    (at (+ n  500) (ctl t0 :note 60 :gate 0))
    (at (+ n 1000) (ctl t0 :note 60 :gate 1))
    (at (+ n 1500) (ctl t0 :note 60 :gate 0))
    (at (+ n 1550) (ctl t0 :note 61 :gate 1))
    (at (+ n 1900) (ctl t0 :note 61 :gate 0)))
  (Thread/sleep 4000)
  (recording-stop))
  
