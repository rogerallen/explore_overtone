(ns explore-overtone.violin
  (:use [overtone.live]))

;; From the first article in the 4-part series...
;; http://www.soundonsound.com/sos/may03/articles/synthsecrets49.asp
;; See Image 5/Figure 4 as a good orientation point.
;;
;; 1) we used the synth to generate a sawtooth wave, and filtered it
;; using the internal low-pass and high-pass filters.
;;
;; 2) we chose contour settings that create a slight 'bowing' attack
;; to the beginning of the sound, and which sustain it for as long as
;; a key is depressed.
;;
;; 3a) in an attempt to reduce the rather synthetic nature of the
;; sound, we added delayed vibrato,
;;
;; 3b) plus a smidgen of portamento to make the transitions between
;; notes less like an organ and more like an unfretted instrument.
;;
;; 4) we passed the result through a three-band parametric 'formant'
;; filter that provides a rough emulation of the most prominent body
;; resonances of the real instrument.
;;
;; Love this quote:
;; So, does it now sound like a real violin? Don't be silly -- it
;; still sounds like a 1970s synth patch of a violin. [LOL!]
;;
;; this is a straight translation the articles give some great ideas
;; on using other types of controllers for more expressiveness.
(defsynth violin
  "violin inspired by Sound On Sound April-July 2003 articles."
  [pitch   {:default 60  :min 0   :max 127 :step 1}
   amp     {:default 1.0 :min 0.0 :max 1.0 :step 0.01}
   gate    {:default 1   :min 0   :max 1   :step 1}
   out-bus {:default 0   :min 0   :max 127 :step 1}]
  (let [freq   (midicps pitch)
        ;; 3b) portamento to change frequency slowly
        freqp  (slew:kr freq 100.0 100.0)
        ;; 3a) vibrato to make it seem "real"
        freqv  (vibrato :freq freqp :rate 6 :depth 0.02 :delay 1)
        ;; 1) the main osc for the violin
        saw    (saw freqv)
        ;; 2) add an envelope for "bowing"
        saw0   (* saw (env-gen (adsr 1.5 1.5 0.8 1.5) :gate gate))
        ;; a low-pass filter prior to our filter bank
        saw1   (lpf saw0 4000) ;; freq???
        ;; 4) the "formant" filters
        band1  (bpf saw1 300 (/ 3.5))
        band2  (bpf saw1 700 (/ 3.5))
        band3  (bpf saw1 3000 (/ 2))
        saw2   (+ band1 band2 band3)
        ;; a high-pass filter on the way out
        saw3   (hpf saw2 30) ;; freq???
        ]
    (out out-bus (* amp saw3))))

;; just playing around...
(comment
  (def v0 (violin :pitch 60))
  (ctl v0 :pitch 64)
  (ctl v0 :pitch 67)
  (ctl v0 :pitch 72)
  (ctl v0 :pitch 60)
  (ctl v0 :gate 0)
)

;; let's make a violin we control via keyboard & mouse
;;   keyboard instantiates the note & starts/stops it
;;   vibrato comes from mouse-x
;;   amplitude comes from mouse-y
(defsynth mouse-violin
  "violin inspired by Sound On Sound April-July 2003 articles."
  [note    {:default 60  :min 0   :max 127 :step 1}
   amp     {:default 1.0 :min 0.0 :max 1.0 :step 0.01}
   gate    {:default 1   :min 0   :max 1   :step 1}
   out-bus {:default 0   :min 0   :max 127 :step 1}]
  (let [freq   (midicps note)
        ;; 3b) portamento to change frequency slowly
        freqp  (slew:kr freq 100.0 100.0)
        ;; 3a) mouse-controlled vibrato to make it seem "real"
        freqv  (+ freqp (* (mouse-x) 0.05 freqp (sin-osc (+ 5 (* 3 (mouse-x))))))
        ;; 1) the main osc for the violin
        saw    (saw freqv)
        ;; 2) add an envelope for "bowing"
        ;;    adsr is just instant on/off.  mouse-y controls amp, really
        saw0   (* saw (- 1.0 (mouse-y)) (env-gen (adsr 0.01 0.01 1.0 0.01) :gate gate :action FREE))
        ;; a low-pass filter prior to our filter bank
        saw1   (lpf saw0 3000) ;; freq???
        ;; 4) the "formant" filters
        band1  (bpf saw1 300 (/ 3.5))
        band2  (bpf saw1 700 (/ 3.5))
        band3  (bpf saw1 3000 (/ 2))
        saw2   (+ band1 band2 band3)
        ;; a high-pass filter on the way out
        saw3   (hpf saw2 300) ;; freq???
        ]
    (out out-bus (pan2 (* amp saw3)))))

;;(def v (violin))
;;(ctl v :gate 0)
;; NOTE: with this, portamento goes out the window since we are creating
;; a new note with each 'note-on'
(def player (midi-poly-player mouse-violin))

;; when you want to stop
(midi-player-stop)
