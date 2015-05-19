(ns explore-overtone.gridstrument
  (:use overtone.live
        overtone.synth.stringed))

;; ======================================================================
;; A marriage of GridStrument and Overtone and the guitar synth
;; See https://github.com/rogerallen/GridStrument
;; ======================================================================
(def server (osc-server 8675 "gridstrument"))
;; (osc-close server)

;; ======================================================================
;; trying to improve the guitar sound
(defmacro gen-stringed-synth-2
  "Macro to generate a stringed defsynth with distortion, reverb and
   a low-pass filter.  Use the pluck-strings and strum-strings helper
  functions to play the instrument.
   Note: the strings need to be silenced with a gate -> 0 transition
   before a gate -> 1 transition activates it.  Testing
   showed it needed > 25 ms between these transitions to be effective."
  [name num-strings free-on-silence]
  (let [note-ins (if (= num-strings 1)
                   [(symbol "note")]
                   (apply vector
                          (map #(symbol (format "note-%d" %)) (range num-strings))))
        note-default-ins (apply vector
                                (flatten (map vector
                                              note-ins
                                              (repeat num-strings {:default 60 :min 0 :max 127}))))
        gate-ins (if (= num-strings 1)
                   [(symbol "gate")]
                   (apply vector
                          (map #(symbol (format "gate-%d" %)) (range num-strings))))
        gate-default-ins (apply vector (flatten (map vector
                                                     gate-ins
                                                     (repeat num-strings {:default 0}))))
        both-default-ins (into note-default-ins gate-default-ins)
        note-gate-pairs (apply vector (map vector note-ins gate-ins))
        env-gen-fn (if free-on-silence
                     '(fn [x] (overtone.sc.ugens/env-gen
                              (overtone.sc.envelope/asr 0.0001 1 0.1)
                              :gate (second x)
                              :action overtone.sc.ugens/FREE))
                     '(fn [x] (overtone.sc.ugens/env-gen
                              (overtone.sc.envelope/asr 0.0001 1 0.1)
                              :gate (second x))))
        ]
    `(defsynth ~name
       ~(str "a stringed instrument synth with " num-strings
             " strings mixed and sent thru
  distortion and reverb effects followed by a low-pass filter.  Use
  the pluck-strings and strum-strings helper functions to play the
  instrument. Note: the strings need to be silenced with a gate -> 0
  transition before a gate -> 1 transition activates it."
             (if free-on-silence
               " This instrument
  is transient.  When a string becomes silent, it will be freed."
               " This instrument
  is persistent.  It will not be freed when the strings go silent."))

       [~@both-default-ins
        ~'decay     {:default 30    :min 1   :max 100} ;; pluck decay
        ~'coef      {:default 0.3   :min -1  :max 1}   ;; pluck coef
        ~'noise-amp {:default 0.8   :min 0.0 :max 1.0}
        ~'pre-amp   {:default 6.0   :min 0.0 :max 10.0}
        ~'amp       {:default 1.0   :min 0.0 :max 10.0}
        ;; by default, no distortion, no reverb, no low-pass
        ~'distort   {:default 0.0   :min 0.0 :max 0.9999999999}
        ~'rvb-mix   {:default 0.0   :min 0.0 :max 1.0}
        ~'rvb-room  {:default 0.0   :min 0.0 :max 1.0}
        ~'rvb-damp  {:default 0.0   :min 0.0 :max 1.0}
        ~'lp-freq   {:default 20000 :min 100 :max 20000}
        ;;~'lp-rq     {:default 1.0   :min 0.1 :max 10.0}
        ~'pan       {:default 0.0   :min -1  :max 1}
        ~'out-bus   {:default 0     :min 0   :max 100}]
       (let [strings# (map #(let [frq#  (midicps (first %))
                                  nze#  (~'* ~'noise-amp (white-noise))
                                  plk#  (pluck nze#
                                               (second %)
                                               (/ 1.0 8.0)
                                               (~'/ 1.0 frq#)
                                               ~'decay
                                               ~'coef)]
                              (leak-dc (~'* plk# (~env-gen-fn %))
                                       0.995))
                           ~note-gate-pairs)
             src# (~'* ~'pre-amp (mix strings#))
             ;; get rid of the mud at the bottom
             src# (hpf (hpf src# 60) 30)
             ;; distortion from fx-distortion2
             k#   (~'/ (~'* 2 ~'distort) (~'- 1 ~'distort))
             dis# (~'/ (~'* src# (~'+ 1 k#))
                       (~'+ 1 (~'* k# (abs src#))))
             vrb# (free-verb dis# ~'rvb-mix ~'rvb-room ~'rvb-damp)
             fil# (lpf vrb# ~'lp-freq)] ;; lpf, not rlpf
         (out ~'out-bus (pan2 (~'* ~'amp fil#) ~'pan))))))
;;(macroexpand-1 '(gen-stringed-synth ektara 1 true))

;;(gen-stringed-synth pektara 1 false) ;; persistent ektara
(gen-stringed-synth-2 pektara 1 false)

;; six fingers should be good enough.
(def eks [(pektara) (pektara) (pektara) (pektara) (pektara) (pektara)])
(def eks-notes [(atom 0) (atom 0) (atom 0) (atom 0) (atom 0) (atom 0)])

;; adjust the eks sound to whatever suits you
(dorun (map
        #(ctl %
              :noise-amp 0.7 :coef 0.5 ;; play with these
              :decay 50
              :lp-freq 5000 ;; and this
              :pre-amp 6.0 :distort 0.75
              :rvb-mix 0.4 :rvb-room 0.81 :rvb-damp 0.5)
        eks))

(def pitch-bend-range 10) ;; keep in sync with GridStrument
;;(recording-start "g6.wav")
;;(recording-stop)

;; translate GridStrument into awesomeness!
(defn grid-note
  [channel note value]
  ;;(println "note" channel note value)
  (if (= value 0)
    (do
      (reset! (nth eks-notes channel) 0)
      (ctl (nth eks channel) :gate 0))
    (do
      (reset! (nth eks-notes channel) note)
      (ctl (nth eks channel) :note note :gate 1 :amp (/ value 127.0)))))

(defn grid-cc
  [channel note value]
  (let [norm-value (/ value 127.0)
        norm-value (* norm-value 0.95)] ;; don't allow 1.0 distortion
    ;;(println "cc" channel note value norm-value)
    ;; can't quite figure out what I want to do here.
    ;;(ctl (nth eks channel) :distort (+ 0.25 (* 0.5 norm-value)))
    ;;(ctl (nth eks channel) :lp-freq (+ 3200 (* 4000 norm-value)))
    ;;(ctl (nth eks channel) :rvb-mix (+ 0.5 (* 0.6 (- norm-value 0.5))))
    nil
    ))

(defn grid-pitch-bend
  [channel value]
  (let [norm-value (/ (- value 8192.0) 8192.0)
        offset (* norm-value pitch-bend-range)
        note (+ @(nth eks-notes channel) offset)]
    ;;(println "pitch" channel value norm-value)
    (ctl (nth eks channel) :note note)))

(defn grid-pressure
  [channel value]
  (let [norm-value (/ value 127.0)]
    ;;(println "pressure" channel value norm-value)
    (ctl (nth eks channel) :amp (/ value 127.0))))

(defn grid-unknown
  [msg]
  (println "???" msg))

(defn listen [msg]
  (let [path        (:path msg)
        value       (first (:args msg))
        is-note     (re-matches #"/vkb_midi/(.*)/note/(.*)" (:path msg))
        is-cc       (re-matches #"/vkb_midi/(.*)/cc/(.*)" (:path msg))
        is-pitch    (re-matches #"/vkb_midi/(.*)/pitch" (:path msg))
        is-pressure (re-matches #"/vkb_midi/(.*)/channelpressure" (:path msg))]
    (if is-note (grid-note (Integer/parseInt (nth is-note 1))
                           (Integer/parseInt (nth is-note 2)) value)
        (if is-cc (grid-cc (Integer/parseInt (nth is-cc 1))
                           (Integer/parseInt (nth is-cc 2)) value)
            (if is-pitch (grid-pitch-bend (Integer/parseInt (nth is-pitch 1)) value)
                (if is-pressure (grid-pressure (Integer/parseInt (nth is-pressure 1)) value)
                    (grid-unknown msg)))))))
(osc-listen server listen :gridstrument)
;;(osc-rm-listener server :gridstrument)


(comment
  (osc-listen server (fn [msg] (println msg)) :debug)
  (osc-rm-listener server :debug)
  ;; handle specific case--not very useful to me
  (osc-handle server "/vkb_midi/0/note/64" (fn [msg] (println msg)))
  (osc-rm-all-handlers server)
  )
