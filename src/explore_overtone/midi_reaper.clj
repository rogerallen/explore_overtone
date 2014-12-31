(ns explore-overtone.midi-reaper
  (:use [overtone.live]))

;; Documenting the steps I took to get Reaper v4.75 up & running on
;; Mac OS X with Overtone.
;;
;; First get Audio Midi Setup with a virtual midi interface to use to
;; communicate.
;;
;; Open Audio Midi Setup
;; - click IAC Driver
;; - make sure it has a port
;; - enable the device
;; - rescan midi to show that it is enabled.
;;
;; http://forum.cockos.com/showthread.php?t=110650
;; https://sites.google.com/site/mfalab/mac-stuff/how-to-use-the-iac-driver
;;
;; Now, Open REAPER
;; - Setup Preferences/Audio/MIDI
;;  - enable input & output devices to that port you just setup above
;; - Main window have a MIDI item
;; - MIDI input from that port
;; - add an FX of some sort (e.g. VSTi: ReaSynth)
;; - IMPORTANT!
;;   make the midi track taller to see the "Record Monitor" enable
;;   in the lower right corner.  Enable that!
;; - arm midi recording
;; - add a "new MIDI item"
;;
;; Now, Open Overtone and ...
;;
(print (midi-connected-receivers))
;; You should see something like this:
;; ({:description IAC Driver Reaper, :vendor Apple Inc., :sinks
;; 2147483647, :sources 0, :name
;; Reaper, :overtone.studio.midi/full-device-key [:midi-device Apple
;; Inc. Reaper IAC Driver Reaper 0], :info #<MidiOutDeviceInfo
;; Reaper>, :overtone.studio.midi/dev-num 0, :device #<MidiOutDevice
;; com.sun.media.sound.MidiOutDevice@1c46f6b4>, :version Unknown
;; version, :receiver #<MidiOutReceiver
;; com.sun.media.sound.MidiOutDevice$MidiOutReceiver@50c9e90d>})
(def ^:dynamic *reaper* (midi-find-connected-receiver "Reaper"))

;; send to a track that accepts MIDI Channel 1 (aka 0 for us)
(midi-note-on *reaper* 64 100 0)  ;; start the note
(midi-note-off *reaper* 64 0)     ;; stop the note

;; send to a track that accepts MIDI Channel 2 (aka 2 for us)
(midi-note-on *reaper* 67 100 1)
(midi-note-off *reaper* 67 1)

;; ======================================================================
;; Okay, the below isn't actually "good", but it shows how to do the
;; recursive pattern.

(def ^:dynamic *reaper* (midi-find-connected-receiver "Reaper"))
(def ^:dynamic *metro*  (metronome 76))
(def ^:dynamic *field*  (scale-field :E :minor))
(def ^:dynamic *syn1*   0) ;; midi channel
(def ^:dynamic *syn2*   1) ;; midi channel
(def root (atom :e2))

(defn quantize
  "given a sorted seq in s, find the item in the seq closest to n.  n
  can be any type that note converts (string, keyword or integer) or a
  floating-point value."
  [s n]
  (let [nt         (if (float? n) n (note n))
        split-seq  (split-with #(<= % nt) s)
        nt-below   (last (first split-seq))
        nt-above   (first (last split-seq))
        ;; handle ends of the sequence
        nt-below   (if (nil? nt-below) (first s) nt-below)
        nt-above   (if (nil? nt-above) (last s) nt-above)
        Δ-nt-below (- nt nt-below)
        Δ-nt-above (- nt-above nt)]
    (if (> Δ-nt-above Δ-nt-below)
      nt-below
      nt-above)))

(defn play-note
  "play a midi note at beat for duration with midi pitch midi level on
  midi channel"
  [channel beat dur pitch level]
  (apply-at (*metro* beat)         #'midi-note-on  [*reaper* pitch level channel])
  (apply-at (*metro* (+ beat dur)) #'midi-note-off [*reaper* pitch       channel]))

(def quant1 (partial quantize *field*))
(def play1 (partial play-note *syn1*))
(def play2 (partial play-note *syn2*))
(defn next-measure-beat []
  (* 4 (metro-bar *metro*)))

;; overtone cosr is (cosr index range centre period)
(defn player1
  [beat dur];)
  (let [ap (quant1 (cosr beat (cosr beat 8 0 3) (+ (note @root) 24) 8))
        av (cosr beat (cosr beat 10 0 4) 20 16)
        bp (quant1 (cosr beat 12 (+ (note @root) 12) 4))
        bv (cosr beat (cosr beat 10 0 4) 20 16)
        p  [:a :a :b :a]
        pb (mod (/ beat 4) (count p))
        cp (nth p pb)
        p  (if (= cp :a) ap bp)
        v  (if (= cp :a) av bv)]
    (play1 beat (* 0.75 dur) p v)
    (apply-by (*metro* (+ beat dur)) #'player1 [(+ beat dur) dur])))

(defn player2
  [beat dur];)
  (let [ap (quant1 (cosr beat (cosr beat 8 0 3) (+ (note @root) 12) 12))
        av (cosr beat (cosr beat 20 0 4) 100 16)
        bp (quant1 (cosr beat 12 (+ (note @root) 12) 12))
        bv (cosr beat (cosr beat 10 0 4) 100 16)
        p  [:a :a :b :a]
        pb (mod (/ beat 4) (count p))
        cp (nth p pb)
        p  (if (= cp :a) ap bp)
        v  (if (= cp :a) av bv)]
    (play2 beat (* 1.25 dur) p v)
    (apply-by (*metro* (+ beat dur)) #'player2 [(+ beat dur) dur])))

;; start
(do
  (player1 (next-measure-beat) 0.5)
  (player2 (next-measure-beat) 2))

;;(reset! root (nth *field* 7r30))

;; stop
(do
  (defn player1 [beat dur])
  (defn player2 [beat dur]))
