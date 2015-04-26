(ns explore-overtone.gridstrument
  (:use [overtone.live]))
;;(in-ns 'explore-overtone.gridstrument)

(def grid (midi-find-connected-device "MIDIHub Port 1"))
(def grid-device-key (midi-full-device-key grid))

;;(event-debug-on)
;;(event-debug-off)

(def notes* (atom [-1 -1 -1 -1
                   -1 -1 -1 -1
                   -1 -1 -1 -1
                   -1 -1 -1 -1]))

(defn grid-note-on [e]
  (let [{:keys [channel note velocity-f timestamp]} e]
    (println timestamp "note-on" channel note velocity-f)
    (swap! notes* (fn [x]
                    (if (= (nth x channel) -1)
                      (assoc x channel note)
                      (do
                        (println "ERROR: channel " channel " already playing a note " note)
                        x))))))

(defn grid-note-off [e]
  (let [{:keys [note channel timestamp]} e]
    (println timestamp "note-off" channel note)
    (swap! notes* (fn [x]
                    (if (= (nth x channel) note)
                      (assoc x channel -1)
                      (do
                        (println "ERROR: channel " channel " not playing note " note)
                        x))))))

(defn grid-pitch-bend [e]
  (let [{:keys [channel data1 data2 timestamp]} e]
    (println timestamp "pitch-bend" channel data1 data2)
    (if (= (nth @notes* channel) -1)
      (if (not (and (= data1 0) (= data2 64)))
        (println "ERROR: channel " channel " not playing note.")))))

(defn grid-control-change [e]
  (let [{:keys [channel data1 data2 timestamp]} e]
    (println timestamp "control-change" channel data1 data2)
    (if (= (nth @notes* channel) -1)
      (if (not (= data2 0))
        (println "ERROR: channel " channel " not playing note.")))))

(do
  (on-event [:midi :note-on] grid-note-on ::grid-note-on-handler)
  (on-event [:midi :note-off] grid-note-off ::grid-note-off-handler)
  (on-event [:midi :pitch-bend] grid-pitch-bend ::grid-pitch-bend-handler)
  (on-event [:midi :control-change] grid-control-change ::grid-control-change-handler))

(do
  (remove-event-handler ::grid-note-on-handler)
  (remove-event-handler ::grid-note-off-handler)
  (remove-event-handler ::grid-pitch-bend-handler)
  (remove-event-handler ::grid-control-change-handler))

;; Testing results 4/26/2015
;;
;; errors appear to come from missing note-on events.
;; if you quickly tap 2 fingers, I see events on the server for the sendNoteOn calls
;; but only 1 note-on message appears on the client.
;; appears related to the timing of the noteOn events on the server.  If they are
;; within 0.001s, it gets dropped. Example:
;;
;; **SERVER**
;; 04-26 10:24:27.163  30112-30112/com.gmail.rallen.gridstrument D/sendNoteOn﹕ ch=0
;; 04-26 10:24:27.164  30112-30112/com.gmail.rallen.gridstrument D/sendNoteOn﹕ ch=1
;; 04-26 10:24:27.240  30112-30112/com.gmail.rallen.gridstrument D/sendNoteOff﹕ ch=1
;; 04-26 10:24:27.267  30112-30112/com.gmail.rallen.gridstrument D/sendNoteOff﹕ ch=0
;;
;; **CLIENT**
;; 359922598751 note-on 0 69 0.496063
;;   <missing note-on 1 67>
;; 359922679854 note-off 1 67
;; ERROR: channel  1  not playing note  67
;; 359922692396 note-off 0 69


;; >>>
;; {:data2 80, :command :note-on, :channel 0, :msg #<FastShortMessage com.sun.media.sound.FastShortMessage@649c095e>, :note 71, :dev-key [:midi-device humatic MIDIHub Port 1 mnet MIDIHub Port 1 0], :status :note-on, :data1 71, :data2-f 0.62992126, :device {:description mnet MIDIHub Port 1, :vendor humatic, :sinks 0, :sources 2147483647, :name MIDIHub Port 1, :transmitter #<MidiInTransmitter com.sun.media.sound.MidiInDevice$MidiInTransmitter@38e7c679>, :overtone.studio.midi/full-device-key [:midi-device humatic MIDIHub Port 1 mnet MIDIHub Port 1 0], :info #<MidiInDeviceInfo MIDIHub Port 1>, :overtone.studio.midi/dev-num 0, :device #<MidiInDevice com.sun.media.sound.MidiInDevice@447fb905>, :version Unknown version}, :timestamp 328326376007, :velocity 80, :velocity-f 0.62992126}
;; <<<
;; !!!
;; {:data2 0, :command :note-off, :channel 0, :msg #<FastShortMessage com.sun.media.sound.FastShortMessage@242e81c4>, :note 71, :dev-key [:midi-device humatic MIDIHub Port 1 mnet MIDIHub Port 1 0], :status :note-on, :data1 71, :data2-f 0.0, :device {:description mnet MIDIHub Port 1, :vendor humatic, :sinks 0, :sources 2147483647, :name MIDIHub Port 1, :transmitter #<MidiInTransmitter com.sun.media.sound.MidiInDevice$MidiInTransmitter@38e7c679>, :overtone.studio.midi/full-device-key [:midi-device humatic MIDIHub Port 1 mnet MIDIHub Port 1 0], :info #<MidiInDeviceInfo MIDIHub Port 1>, :overtone.studio.midi/dev-num 0, :device #<MidiInDevice com.sun.media.sound.MidiInDevice@447fb905>, :version Unknown version}, :timestamp 328327215408, :velocity 0, :velocity-f 0.0}
;; !!!

;; !!!
;; {:data2 65, :command :pitch-bend, :channel 0, :msg #<FastShortMessage com.sun.media.sound.FastShortMessage@31bf070b>, :note 15, :dev-key [:midi-device humatic MIDIHub Port 1 mnet MIDIHub Port 1 0], :status :pitch-bend, :data1 15, :data2-f 0.511811, :device {:description mnet MIDIHub Port 1, :vendor humatic, :sinks 0, :sources 2147483647, :name MIDIHub Port 1, :transmitter #<MidiInTransmitter com.sun.media.sound.MidiInDevice$MidiInTransmitter@38e7c679>, :overtone.studio.midi/full-device-key [:midi-device humatic MIDIHub Port 1 mnet MIDIHub Port 1 0], :info #<MidiInDeviceInfo MIDIHub Port 1>, :overtone.studio.midi/dev-num 0, :device #<MidiInDevice com.sun.media.sound.MidiInDevice@447fb905>, :version Unknown version}, :timestamp 328430683670, :velocity 65, :velocity-f 0.511811}
;; !!!
