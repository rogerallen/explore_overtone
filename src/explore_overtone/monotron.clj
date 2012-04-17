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
        vcf_bpf_q       0.25 ; ??? param
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
(ctl N2 :note   80)
(ctl N1 :note   60)
(ctl monotron :volume 0.8)
(ctl monotron :mod_pitch_not_cutoff 1)
(ctl monotron :pitch  0.0)
(ctl monotron :rate   2.5)
(ctl monotron :int    1200.0)
(ctl monotron :cutoff 1400.0)
(ctl monotron :peak   0.1)
(ctl monotron :gate   1.0)

;; for when you're done
(kill monotron)
(kill N0)
(kill N1)

