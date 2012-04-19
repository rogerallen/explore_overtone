(ns explore_overtone.monotron)
;; Monotron Clone Attempt by Roger Allen.
;;
;; My first definst, so much could easily be wrong below...
;;
;; Source
;; http://korg.com/monotrons
;; http://korg.com/services/products/monotron/monotron_Block_diagram.jpg
;; some inspiration
;; http://www.soundonsound.com/sos/aug10/articles/korg-monotron.htm
;; following patterns from
;; https://github.com/overtone/overtone/blob/master/src/overtone/inst/synth.clj

;; ----------------------------------------------------------------------
(use 'overtone.live)

(definst monotron
  "Korg Monotron from website diagram: http://korg.com/services/products/monotron/monotron_Block_diagram.jpg."
  [note     60            ; midi note value
   volume   0.7           ; gain of the output
   mod_pitch_not_cutoff 1 ; use 0 or 1 only to select LFO pitch or cutoff modification 
   pitch    0.0           ; frequency of the VCO
   rate     4.0           ; frequency of the LFO
   int      1.0           ; intensity of the LFO
   cutoff   1000.0        ; cutoff frequency of the VCF
   peak     0.5           ; VCF peak control (resonance)
   gate     1.0]          ; another output gain?
  (let [note_freq       (midicps note)
        pitch_mod_coef  mod_pitch_not_cutoff
        cutoff_mod_coef (- 1 mod_pitch_not_cutoff)
        LFO             (* int (saw rate))
        VCO             (saw (+ note_freq pitch (* pitch_mod_coef LFO)))
        vcf_freq        (+ cutoff (* cutoff_mod_coef LFO) note_freq)
        vcf_bpf_q       0.1 ; reciprocal! of Q (no idea if this is right)
        VCF             (+ (* (- 1 peak) (lpf VCO vcf_freq))
                           (* peak (bpf VCO vcf_freq vcf_bpf_q)))]
    (* gate volume VCF)))

;; create some instances of the synth
(do
  (def N0 (monotron 72 0.8 0 0.0 2.5 350.0 800.0 0.8 1.0))
  (def N1 (monotron 76 0.8 0 0.0 2.5 350.0 800.0 0.8 1.0))
  (def N2 (monotron 79 0.8 0 0.0 2.5 350.0 800.0 0.8 1.0)))

;; edit & C-x C-e on any these to play around
(ctl monotron :note   70)
(ctl N0 :note   30)
(ctl N1 :note   (+ 30 4))
(ctl N2 :note   (+ 30 7))
(ctl monotron :volume 0.8)
(ctl monotron :mod_pitch_not_cutoff 1)
(ctl monotron :pitch  0.0)
(ctl monotron :rate   2)
(ctl monotron :int    0.0)
(ctl monotron :cutoff 880.0)
(ctl monotron :peak   0.0)
(ctl monotron :gate   1.0)

;; for when you're done
(kill monotron)
(kill N0)
(kill N1)
(kill N2)

;; major chord
(defn maj-tri [note]
  (ctl N0 :note   note)
  (ctl N1 :note   (+ note 4))
  (ctl N2 :note   (+ note 7)))

(maj-tri 44)

;; MIDI Control ==================================================
;; hooking up to the iPad
(def kb (midi-in "Control Session"))
;; simple listener just for testing
(defn midi-listener [event ts]
  (println "listener: " event))
(midi-handle-events kb #'midi-listener)

;; 'real' listener
;; hook up Control to the ctl messages
;; use "Mixer - Landscape" layout
(defn midi-listener [event ts]
  ;;(println "listener: " event)
  (cond

   ;; controller 1 is for CUTOFF
   (and (== 176 (:cmd event)) (== 1 (:note event)))
   (let [v ( * 30 (:vel event))]
     (println "cutoff" v)
     (ctl monotron :cutoff v))

   ;; controller 2 is for PEAK
   (and (== 176 (:cmd event)) (== 2 (:note event)))
   (let [v ( / (:vel event) 127.0)]
     (println "peak" v)
     (ctl monotron :peak v))
  
   ;; controller 3 is for INT
   (and (== 176 (:cmd event)) (== 3 (:note event)))
   (let [v ( * 20 (:vel event))]
     (println "int" v)
     (ctl monotron :int v))

   ;; controller 4 is for RATE
   (and (== 176 (:cmd event)) (== 4 (:note event)))
   (let [v ( * 0.25 (:vel event))]
     (println "rate" v)
     (ctl monotron :rate v))

   ;; controller 5 is for NOTE
   (and (== 176 (:cmd event)) (== 5 (:note event)))
   (let [v ( * 1 (:vel event))]
     (println "note" v)
     (ctl monotron :note v))

   ;; controller 6 is for VOLUME
   (and (== 176 (:cmd event)) (== 6 (:note event)))
   (let [v ( / (:vel event) 127.0)]
     (println "volume" v)
     (ctl monotron :volume v))

   )
  )
