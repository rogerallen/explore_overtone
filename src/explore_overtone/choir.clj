(ns explore-overtone.choir
  (:use [overtone.live]
        [overtone.gui.control]
        [overtone.gui.scope]))

;; look at what you create
(scope)

;; attempting to recreate the Korg M1 23 Choir from
;; http://www.synthmania.com/m1.htm
;; listen here.
;;   http://www.synthmania.com/Korg%20M1/Audio/Factory%20preset%20examples/Programs/23%20Choir.mp3

;; thought that this might be useful, but apparently not?
;; http://en.wikipedia.org/wiki/Formant
;; note-freq2 (* (/ 1150.0 700.0) note-freq)

(defsynth voice-1
  [note                {:default 60  :min 0    :max 127   :step 1}
   velocity            {:default 100 :min 0    :max 127   :step 1}
   milli-q             {:default 12  :min 1    :max 200   :step 1}
   lpf-freq            {:default 1500 :min 400 :max 10000 :step 100}
   separation          {:default 9   :min 0.5  :max 20.0  :step 0.5}
   adsr-attack-time    {:default 0.1 :min 0.0  :max 1.0   :step 0.01}
   adsr-decay-time     {:default 0.0 :min 0.0  :max 1.0   :step 0.01}
   adsr-sustain-level  {:default 1.0 :min 0.0  :max 1.0   :step 0.01}
   adsr-release-time   {:default 0.1 :min 0.0  :max 1.0   :step 0.01}
   adsr-peak-level     {:default 1.0 :min 0.0  :max 1.0   :step 0.01}
   adsr-curve          {:default -1  :min -5   :max 5     :step 1}
   gate                {:default 1.0 :min 0.0  :max 1.0   :step 1}]
  (let [amp (/ velocity 127.0)
        n (* 10 (white-noise)) ; better than pink-noise))
        q (/ milli-q 1000.0)
        ;; mp3 seems to have 3 "formants"
        note-freq (midicps note)
        note-freq2 (* 1.25 note-freq)
        note-freq3 (* 1.5 note-freq)
        ;; and then several octaves of each
        note-freq4 (* note-freq 2)
        note-freq5 (* note-freq 4)
        note-freq6 (* note-freq 8)
        note-freq7 (* note-freq2 2)
        note-freq8 (* note-freq2 4)
        note-freq9 (* note-freq2 8)
        note-freq10 (* note-freq3 2)
        note-freq11 (* note-freq3 4)
        note-freq12 (* note-freq3 8)
        ;; apparently, resonant band-pass filters are how we
        ;; need to do this...
        ;; FIXME -- add lowpass filter on note-freq
        voice-out (bpf n note-freq q)
        voice-out (+ voice-out (bpf n note-freq2 q))
        voice-out (+ voice-out (bpf n note-freq3 q))
        voice-out (+ voice-out (bpf n note-freq4 q))
        voice-out (+ voice-out (bpf n note-freq5 q))
        voice-out (+ voice-out (bpf n note-freq6 q))
        voice-out (+ voice-out (bpf n note-freq7 q))
        voice-out (+ voice-out (bpf n note-freq8 q))
        voice-out (+ voice-out (bpf n note-freq9 q))
        voice-out (+ voice-out (bpf n note-freq10 q))
        voice-out (+ voice-out (bpf n note-freq11 q))
        voice-out (+ voice-out (bpf n note-freq12 q))
        ;; FIXME -- how to do this with a shallower rolloff
        ;; now drop off the high frequencies...
        ;;voice-out (lpf voice-out lpf-freq)
        voice-out (b-low-pass voice-out lpf-freq)
        ;; and drop off some of the low freq, too.
        ;;voice-out (hpf voice-out (* 0.8 note-freq))
        voice-out (b-hi-pass voice-out (* 0.8 note-freq))
        ;; pop in some stereo separation...
        voice-out-2ch [voice-out (* -1 voice-out)]
;;                       (delay-c voice-out 0.5 (/ separation 1000.0))]
        ;; and make a note out of it
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate :action FREE)]
    (out 0 (* amp env-out voice-out-2ch))))

;; check it out with live controllers...
(do
  (midi-poly-player voice-1)
  (synth-controller voice-1))
;; when you want to recompile & reconnect, do this first
(midi-player-stop)

;; or play manually
(def v1 (voice-1 :note 58))
(ctl v1 :gate 0)

;; when you want to look at the output in Audacity
(recording-start "voice-1.wav")
(recording-stop)
