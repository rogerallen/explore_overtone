(ns explore-overtone.gridstrument
  (:use overtone.live
        overtone.synth.stringed))

;; ======================================================================
;; A marriage of GridStrument and Overtone and the guitar synth
;; See https://github.com/rogerallen/GridStrument
;; ======================================================================
(def server (osc-server 8675 "gridstrument"))
;; (osc-close server)

(gen-stringed-synth pektara 1 false) ;; persistent ektara

(def pitch-bend-range 10) ;; keep in sync with GridStrument

;; six fingers should be good enough.
(def eks [(pektara) (pektara) (pektara) (pektara) (pektara) (pektara)])
(def eks-notes [(atom 0) (atom 0) (atom 0) (atom 0) (atom 0) (atom 0)])

;; adjust the eks sound to whatever suits you
(dorun (map
        #(ctl %
              :pre-amp 5.0
              :lp-freq 3000 :lp-rq 0.25
              :rvb-mix 0.3 :rvb-room 0.7 :rvb-damp 0.4)
        eks))

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
      (ctl (nth eks channel) :note note :gate 1))))

(defn grid-cc
  [channel note value]
  (let [norm-value (/ value 127.0)
        norm-value (* norm-value 0.9)] ;; don't allow 1.0 distortion
    ;;(println "cc" channel note value norm-value)
    (ctl (nth eks channel) :distort norm-value)))

(defn grid-pitch-bend
  [channel value]
  (let [norm-value (/ (- value 8192.0) 8192.0)
        offset (* norm-value pitch-bend-range)
        note (+ @(nth eks-notes channel) offset)]
    ;;(println "pitch" channel value norm-value)
    (ctl (nth eks channel) :note note)))

;; not seeing this change much yet
(defn grid-pressure
  [channel value]
  (let [norm-value (/ value 4095.0)]
    (println "pressure" channel value norm-value)))

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
