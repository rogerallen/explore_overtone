(ns explore-overtone.nano
  (:use [overtone.live])
  (:require [clojure.pprint :as cpp]
            [overtone.device.midi.nanoKONTROL2 :as nk]
            [explore-overtone.nk2 :as nk2]
            [overtone.midi :as midi]))

(def in-devices (midi-connected-devices))
(def out-devices (midi-connected-receivers))
(println (count in-devices) "input devices")
(println (count out-devices) "output devices")
(dorun (doseq [d in-devices] (println (:description d))))
(dorun (doseq [d out-devices] (println (:description d))))
(dorun (doseq [d in-devices] (println (midi-full-device-key d))))
;;(cpp/pprint in-devices)

;; watch the controls happen (in terminal repl)
(on-latest-event (conj (midi-mk-full-device-key
                        (midi-find-connected-device "nanoKONTROL2"))
                       :control-change)
                 (fn [m] (let [{:keys [timestamp data1 data2]} m]
                          (println timestamp data1 data2)))
                 ::nk2-cc)

(def mynk (nk/connect))

(nk/led-on  mynk :play)
(nk/led-off mynk :play)
(cpp/pprint mynk)
(cpp/pprint (-> mynk :state :pot0))
(-> mynk :state :play)

(def mynk (nk2/add-led-toggles! mynk))
(-> mynk :state :toggle-rewind)
(-> mynk :state :rewind)
(-> mynk :state keys)
(-> mynk :interfaces :leds :controls keys)

;; ======================================================================
