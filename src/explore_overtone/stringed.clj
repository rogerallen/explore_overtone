;; A Stringed Instrument Generator by Roger Allen.
(ns explore_overtone.stringed
  (:use [overtone.music pitch time]
        [overtone.studio inst]
        [overtone.sc envelope node server ugens]
        [overtone.sc.cgens mix]))

;; ======================================================================
;; The guitar instrument.  Now with distortion, reverb and a low-pass filter.
;; Note: the strings need to be silenced with a gate -> 0 transition
;; before a gate -> 1 transition activates it.  Testing showed it
;; needed > 25 ms between these transitions to be effective.
;; Use the strum and pick helpers to play the instrument.

;;(macroexpand-1 '(gen-stringed-inst xxx 6))
(defmacro gen-stringed-inst [name num-strings]
  (let [note-ins# (apply vector
                         (map #(symbol (format "note-%d" %)) (range num-strings)))
        note-default-ins# (apply vector
                                 (flatten (map vector
                                               note-ins#
                                               (repeat num-strings "{:default 60 :min 0 :max 127}"))))
        ;;    `(defn foo [] ~note-ins#)))
        gate-ins# (apply vector
                         (map #(symbol (format "gate-%d" %)) (range num-strings)))
        gate-default-ins# (apply vector (flatten (map vector
                                                      gate-ins#
                                                      (repeat num-strings "{:default 0}"))))
        both-default-ins# (into note-default-ins# gate-default-ins#)
        note-gate-pairs# (apply vector (map vector note-ins# gate-ins#))
        ]
    `(definst ~name ~(into both-default-ins#
                           `[dur       {:default 10.0}
                            decay     {:default 30} ;; pluck decay
                            coef      {:default 0.3 :min -1 :max 1} ;; pluck coef
                            noise-amp {:default 0.8 :min 0.0 :max 1.0}
                            pre-amp   {:default 6.0}
                            amp       {:default 1.0}
                            ;; by default, no distortion, no reverb, no low-pass
                            distort   {:default 0.0 :min 0.0 :max 0.9999999999}
                            rvb-mix   {:default 0.0 :min 0.0 :max 1.0}
                            rvb-room  {:default 0.0 :min 0.0 :max 1.0}
                            rvb-damp  {:default 0.0 :min 0.0 :max 1.0}
                            lp-freq   {:default 20000}
                            lp-rq     {:default 1.0}])
       (let [strings (map #(let [frq  (midicps (first %))
                                 nze  (* noise-amp (pink-noise))
                                 plk  (pluck nze
                                             (second %)
                                             (/ 1.0 8.0)
                                             (/ 1.0 frq)
                                             decay
                                             coef)]
                             (leak-dc (* plk (env-gen (asr 0.0001 1 0.1)
                                                      :gate (second %)))
                                      0.995))
                     ~note-gate-pairs#)
             src (* pre-amp (mix strings))
             ;; distortion from fx-distortion2 
             k   (/ (* 2 distort) (- 1 distort))
             dis (/ (* src (+ 1 k)) (+ 1 (* k (abs src))))
             vrb (free-verb dis rvb-mix rvb-room rvb-damp)
             fil (rlpf vrb lp-freq lp-rq)]
         (* amp fil)))))
;;(macroexpand-1 '(gen-stringed-inst xxx 2))
