(use 'overtone.live)
;; just went through all the instruments in the inst folder...

;; Drums ----------------------------------------
(use 'overtone.inst.drum)

(kick :freq 110 :env-ratio 4 :freq-decay 0.02 :amp-decay 0.5)
(kick2 :freq 110 :amp 0.8 :mod-freq 8 :mod-index 5 :sustain 0.5 :noise 0.025)
(kick3 :freq 110 :amp 0.8)
(kick4 :freq 165 :amp 0.8 :attack 0.01 :decay 0.4)
(dub-kick :freq 133)
(dance-kick :freq 85 :attack 0.01 :decay 0.5 :fattack 0.001 :fdecay 0.1 :amp 0.8)
(dry-kick :freq 110 :amp 0.8 :attack 0.01 :decay 0.4)
(quick-kick :freq 20 :attack 0.01 :decay 0.5 :fattack 0.001 :fdecay 0.1 :amp 0.8)

(open-hat)
(closed-hat)
(hat-demo)
(closed-hat2)
(open-hat)
(hat3 :amp 0.8 :t 0.25 :low 8000 :hi 2000) ; open
(hat3 :amp 0.8 :t 0.05 :low 8000 :hi 2000) ; closed
(soft-hat) ;; ??
;;(noise-hat)
;;(bell-hat)

(snare)
(snare2)
(noise-snare)
(tone-snare)

(tom :freq 120)

(clap)
(haziti-clap)

(bing)

;; Piano ----------------------------------------
(use 'overtone.inst.piano)
(piano)
(stop)

;; Sampled Piano --------------------------------
(use 'overtone.inst.sampled-piano)
(sampled-piano)

;; Synth ----------------------------------------
(use 'overtone.inst.synth)
(simple-flute) ; (ctl simple-flute :gate 0) (stop)
(cs80lead) ; (stop)
(supersaw :amp 0.4) ; (stop)
(ticker)
(ping)
(tb303 :note 40 )
(mooger :osc1 2 :osc2 1) ; (stop)
(rise-fall-pad) ; (stop)
(pad :s 0.5) ; (ctl pad :gate 0) (stop)
(overpad) ; (nice)
(buzz) ; ??? envelope went past end of inputs.
(bass)
(daf-bass) ; (stop)
(grunge-bass)
(vintage-bass :note 80) ; ???
(ks1)
(ks1-demo)
(ks-stringer) ; (stop)
(fm-demo) ; (stop)
(whoahaha) ; (stop)
(bubbles) ; (stop)
