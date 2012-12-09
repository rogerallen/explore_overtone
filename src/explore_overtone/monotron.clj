(ns explore-overtone.monotron)
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
;;
;; found filter discussion here
;; http://www.timstinchcombe.co.uk/index.php?pge=mono

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
        ;; from web vcf reciprocal of Q looks 1-ish
        vcf_bpf_rq       1.0
        ;; from web vcf looks like you should always have a large LPF component
        ;; BPF should get added on top.  Seems like I should scale this to clamp
        ;; the output, though.
        vcf_lpf         (lpf VCO vcf_freq)
        vcf_bpf         (bpf VCO vcf_freq vcf_bpf_rq)
        VCF             (/ (+ vcf_lpf (* peak vcf_bpf)) (+ 1 peak))
        ]
    (* gate volume VCF)))

;; create some instances of the synth
(do
  (def N0 (monotron 72 0.8 1 0.0 2.5 350.0 800.0 0.8 1.0))
  (def N1 (monotron 76 0.8 1 0.0 2.5 350.0 800.0 0.8 1.0))
  (def N2 (monotron 79 0.8 1 0.0 2.5 350.0 800.0 0.8 1.0)))

;; edit & C-x C-e on any these to play around
(ctl monotron :note   70)
(ctl N0 :note   60)
(ctl N1 :note   64)
(ctl N2 :note   (+ 30 7))
(ctl monotron :volume 0.5)
(ctl monotron :mod_pitch_not_cutoff 1)
(ctl monotron :pitch  0.0)
(ctl monotron :rate   0.5)
(ctl monotron :int    500.0)
(ctl monotron :cutoff 380.0)
(ctl monotron :peak   0.0)
(ctl monotron :gate   0.0)

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

(maj-tri 59)

;; MIDI Control ==================================================
;; (do 
;;   (def kb (midi-in "Control Session"))
;;   (defn midi-listener [event ts]
;;     (println "midi: " event))
;;   (midi-handle-events kb #'midi-listener))
;; okay now for reals...
(do
  (def kb (midi-in "Control Session")) ; FIXME

  ;; wow functional programming makes state-handling odd.
  (def threePosState (ref 0))
  (defn nextThreePosState []
    (dosync (ref-set threePosState (mod (+ 1 @threePosState) 3))))

  (defn eventMatch
    "event :cmd matches cmd and :data1 matches data1"
    [ event cmd data1 ]
    (and (== cmd (:cmd event)) (== data1 (:data1 event))))

  ;; switching to "Mix 2" layout in TouchOSC
  ;; FIXME make custom control
  (defn midi-listener [event ts]
    (let [kControlEvent  176
          kCmdThreePos   144
          kData1ThreePos  24
          kData2ThreePos 127
          kData1Pitch      7
          kData1Rate       8
          kData1Int        9
          kData1Cutoff     1
          kData1Peak       2
          kData1Volume     0
          kData1Note      47
          ThreePosState    0]
      ;;(println "listener: " event)
      (cond

        (eventMatch event kControlEvent kData1Volume)
        (let [v ( / (:vel event) 127.0)]
          (println "volume" v)
          (ctl monotron :volume v))

        (eventMatch event kControlEvent kData1Pitch)
        (let [v (midi->hz ( / (:vel event) 1.0))]
          (println "pitch" v)
          (ctl monotron :cutoff v))

        (eventMatch event kControlEvent kData1Cutoff)
        (let [v ( * 30 (:vel event))]
          (println "cutoff" v)
          (ctl monotron :cutoff v))
      
        (eventMatch event kControlEvent kData1Peak)
        (let [v ( / (:vel event) 127.0)]
          (println "peak" v)
          (ctl monotron :peak v))
  
        (eventMatch event kControlEvent kData1Int)
        (let [v ( * 20 (:vel event))]
          (println "int" v)
          (ctl monotron :int v))

        (eventMatch event kControlEvent kData1Rate)
        (let [v ( * 0.25 (:vel event))]
          (println "rate" v)
          (ctl monotron :rate v))

        ;; one octave for now
        (eventMatch event kControlEvent kData1Note)
        (let [v (+ 50 (* 12 (/ (:vel event) 127.0)))]
          (println "note" v)
          (ctl monotron :note v))

        ;; switch toggles through standby/pitch_mod/cutoff_mod
        (and (eventMatch event kCmdThreePos kData1ThreePos)
             (= kData2ThreePos (:data2 event)))
        (do
          (nextThreePosState)
          (cond
            
            (= 0 @threePosState)
            (do
              (println "standby")
              (ctl monotron :gate 0.0))

            ;; ...pitch_mod
            (= 1 @threePosState)
            (do
              (println "pitch_mod")
            (ctl monotron :gate 1.0)
            (ctl monotron :mod_pitch_not_cutoff 1))

            ;; ...cutoff_mod
            (= 2 @threePosState)
            (do
              (println "cutoff_mod")
              (ctl monotron :gate 1.0)
              (ctl monotron :mod_pitch_not_cutoff 0))))
        
        )))
  
  (midi-handle-events kb #'midi-listener))
