(ns explore-overtone.nk2
  (:use [overtone.live])
  (:require [clojure.pprint :as cpp]
            [overtone.device.midi.nanoKONTROL2 :as nk]))

;; code to turn LEDs on/off automatically.
;;
;; connect to the nanoKONTROL2
;;   (def mynk (nk/connect))
;; add watches & handlers for toggling leds
;;   (def mynk (nk2/add-led-toggles! mynk))
;; now each :led key has a :toggle-led version like
;;   (-> mynk :state :toggle-rewind) -> true or false

(defn toggle-keyword
  "given an led key, add toggle to it"
  [k]
  (keyword (str "toggle-" (name k))))

(defn add-led-watches!
  "add a watch to an led-key and return an atom to be used for toggle state"
  [kon led-key]
  (let [led-str   (name led-key)
        toggle-kw (toggle-keyword led-key)]
    (add-watch (-> kon :state led-key)
               (keyword (str "watch-" led-str))
               (fn [k r os ns]
                 (when (= ns 1.0)
                   (swap! (-> kon :state toggle-kw) (fn [v] (not v)))
                   (if (deref (-> kon :state toggle-kw))
                     (nk/led-on  kon led-key)
                     (nk/led-off kon led-key)))))))

(defn add-led-toggles!
  "add  for each led"
  [kon]
  (let [led-keys (-> kon :interfaces :leds :controls keys)
        kon      (assoc kon :state (into (:state kon) (map #(vector (toggle-keyword %) (atom false)) led-keys)))]
    (dorun (doseq [k led-keys] (add-led-watches! kon k)))
    kon))
