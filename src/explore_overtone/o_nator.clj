(ns explore-overtone.o-nator
  (:require [overtone.live :as o]
            [overtone.libs.event :as e]
            [overtone.inst.sampled-piano :as piano]))

(defn quantize
  "given a sorted seq in s, find the item in the seq closest to n.  n
  can be any type that note converts (string, keyword or integer) of a
  floating-point value."
  [s n]
  (let [nt         (if (float? n) n (o/note n))
        split-seq  (split-with #(<= % nt) s)
        nt-below   (last (first split-seq))
        nt-above   (first (last split-seq))
        ;; handle ends of the sequence
        nt-below   (if (nil? nt-below) (first s) nt-below)
        nt-above   (if (nil? nt-above) (last s) nt-above)
        Δ-nt-below (- nt nt-below)
        Δ-nt-above (- nt-above nt)]
    (if (>= Δ-nt-above Δ-nt-below) nt-below nt-above)))

(defn note-to-cmaj-index
  "given a note n, find the closest note in the C major scale and
  return the index of that note."
  [n]
  (let [cmaj-field (o/scale-field :c :major)
        cmaj-note  (quantize cmaj-field n)
        cmaj-index (.indexOf cmaj-field cmaj-note)]
    cmaj-index))

(defn o-nator
  "Convert an input note from the c-major scale 'white keys' to
  another scale described by the tonic scale.  Allows you to always
  play the right note.  E.g (o-nator :a :minor :c4) -> :a4"
  [tonic scale note]
  (nth (o/scale-field tonic scale) (note-to-cmaj-index note)))

;; ======================================================================
;; a global atom to hold the current key, etc
(defonce Ω (atom {:tonic   :d
                  :scale   :minor
                  :dest    (o/midi-find-connected-receiver "Reaper")
                  ;;(map o/midi-full-device-key (o/midi-connected-devices))
                  :src     [:midi-device "KORG INC." "KEYBOARD" "nanoKEY2 KEYBOARD" 0]
                  :channel 0}))

(defn pian-o-nator
  "For use with midi-poly-player.  Wrap sampled-piano synth to convert
  'white keys' to the scale defined in the atom Ω.  returns the
  sampled-piano synth."
  [& {:keys [note amp velocity]}]
  (piano/sampled-piano :note  (o-nator (:tonic @Ω) (:scale @Ω) note)
                       :level amp))

(defn midi-o-nator
  "Read midi inputs from device-key, convert 'white keys' to the scale
  defined in the atom Ω. and send them out to the device & channel in
  the atom Ω."
  ([] (midi-o-nator (:src @Ω)))
  ([device-key]
   (let [_ (println "dk" device-key)
         on-event-key  (concat device-key [:note-on])
         off-event-key (concat device-key [:note-off])
         on-key        (concat [::midi-o-nator] on-event-key)
         off-key       (concat [::midi-o-nator] off-event-key)]
     (e/on-event on-event-key
                 (fn [{note :note velocity :velocity}]
                   (o/midi-note-on (:dest @Ω)
                                 (o-nator (:tonic @Ω) (:scale @Ω) note)
                                 velocity
                                 (:channel @Ω)))
                 on-key)
     (e/on-event off-event-key
                 (fn [{note :note}]
                   (o/midi-note-off (:dest @Ω)
                                    (o-nator (:tonic @Ω) (:scale @Ω) note)
                                    (:channel @Ω)))
                 off-key))))

(defn midi-o-nator-off
  "Pause recording note on/off events to the midi-o-nator-events list"
  ([] (midi-o-nator-off (:src @Ω)))
  ([device-key]
   (let [on-event-key  (concat device-key [:note-on])
         off-event-key (concat device-key [:note-off])
         on-key        (concat [::midi-o-nator] on-event-key)
         off-key       (concat [::midi-o-nator] off-event-key)]
     (e/remove-event-handler on-key)
     (e/remove-event-handler off-key))))

(comment
  ;; cmaj = ... 60 62 64 65 67 ...
  ;; dmin = ... 62 64 65 67 69 ...
  (o-nator :d :minor 60) ;; => 62
  (o-nator :d :minor 64) ;; => 65
  (o-nator :d :minor 67) ;; => 69

  (do ;; c-major chord
    (piano/sampled-piano 60)
    (piano/sampled-piano 64)
    (piano/sampled-piano 67))

  (do ;; d-minor chord
    (piano/sampled-piano (o-nator :d :minor 60))
    (piano/sampled-piano (o-nator :d :minor 64))
    (piano/sampled-piano (o-nator :d :minor 67)))

  ;; play with midi
  (o/midi-poly-player pian-o-nator)
  (swap! Ω assoc :scale :dorian)
  (swap! Ω assoc :tonic :f)
  (swap! Ω assoc :scale :mixolydian)
  (o/midi-player-stop)

  ;; play with midi-to-midi
  (midi-o-nator)
  (swap! Ω assoc :scale :pentatonic)
  (swap! Ω assoc :tonic :e)
  (swap! Ω assoc :scale :phrygian)
  (midi-o-nator-off)
)
