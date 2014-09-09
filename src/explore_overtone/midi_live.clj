(ns explore-overtone.midi-live
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]
            [overtone.inst.synth :as osynth]
            [overtone.inst.sampled-piano :as piano]
            [explore-overtone.midi-persi :as mp]))

(defn play-piano
  [t note dur]
  (let [s (o/at t (piano/sampled-piano note))]
    (o/at (+ t dur) (o/ctl s :gate 0))))

(defn play-ektara
  [t note dur]
  (let [s (o/at t (oss/ektara note 1))]
    (o/at (+ t dur) (o/ctl s :gate 0))))

(defn play-overpad
  [t note dur]
  (let [s (o/at t (osynth/overpad note))]
    (o/at (+ t dur) (o/ctl s :gate 0))))

(def click (o/sample (o/freesound-path 87731)))
(defn play-click
  [t]
  (click :amp 0.25))

(defn seq-synth
  [synth t0 notes]
  (when-not (empty? notes)
    (synth (+ t0 (:timestamp (first notes)))
                (:note (first notes))
                (:duration (first notes)))
    (when-not (empty? (rest notes))
      (o/apply-by (+ t0 (:timestamp (first (rest notes))))
                  #'seq-synth [synth t0 (rest notes)]))))

(defonce do-loop (atom false))
(defn loop-synth
  ([synth notes Δt]
     (reset! do-loop true)
     (loop-synth synth (o/now) notes Δt))
  ([synth t0 notes Δt]
     (loop-synth synth t0 notes Δt notes))
  ([synth t0 orig-notes Δt notes]
     (when @do-loop
       (let [t1 (+ t0 Δt)]
         (if-not (empty? notes)
           (do
             (synth (+ t0 (:timestamp (first notes)))
                    (:note (first notes))
                    (:duration (first notes)))
             (if-not (empty? (rest notes))
               (o/apply-by (+ t0 (:timestamp (first (rest notes))))
                           #'loop-synth [synth t0 orig-notes Δt (rest notes)])
               (o/apply-by (+ t0 Δt)
                           #'loop-synth [synth (+ t0 Δt) orig-notes Δt orig-notes])))
           (o/apply-by (+ t0 Δt)
                       #'loop-synth [synth (+ t0 Δt) orig-notes Δt orig-notes]))))))

(defn mspb
  "milliseconds per beat"
  [tempo]
  (* 1000 (/ 60.0 tempo)))

(defonce do-click (atom false))
(defn click-track
  ([tempo]
     (reset! do-click true)
     (click-track false (o/now) tempo))
  ([active t tempo]
     (let [Δt (mspb tempo))]
       (if active
         (play-click t))
       (when @do-click
         (o/apply-by (+ t Δt) #'click-track [true (+ t Δt) tempo])))))

(comment

  ;; midi-persi interaction
  (mp/init!)  ;; once a session

  (mp/new!)   ;; each time you want to put work in a new file
  (mp/record) ;; start recording midi events
  (mp/pause)  ;; pause recording midi events
  (mp/summary) ;; what does the file look like?
  (mp/save!)  ;; to save work away
  (mp/open! "140907_145403.clj")

  ;; find a tempo
  (mp/record-keyboard-events) ;; find tiny pop-under window in center of screen
  (mp/keyboard-tempo)
  (click-track 90)
  (reset! do-click false)

  ;; connect a synth to the midi player
  (def mpp (o/midi-poly-player (partial oss/ektara :gate 1)))
  (def mpp (o/midi-poly-player (partial piano/sampled-piano
                                        :gate 1 :play-buf-action o/NO-ACTION)))
  (o/midi-player-stop) ;; stop the midi when you want

  ;; partition into sequences & play each
  (def nl (mp/partition-by-timestamp (mp/get-list)))
  (count nl)
  (do
    (seq-synth play-piano (o/now) (mp/make-notes (last nl)))
    (seq-synth play-piano (o/now) (mp/make-notes (nth nl 4))))

  (loop-synth play-piano
              (mp/make-notes (last (mp/partition-by-timestamp (mp/get-list))))
              (* 4 (mspb 90)))
  @do-loop
  (reset! do-loop false)

  ;; FIXME
  ;; o add a way to loop a sequence
  ;; o add keyboard controls for splitting a sequence
  ;; o relate midi timestamp to system timestamp

  ;; 2 1-second on/off events just for scale.  Timestamps are just "off"
  ;; {:command :note-on, :note 48, :velocity 72,  :timestamp     560637372180}
  ;; {:command :note-off, :note 48, :velocity 64, :timestamp     560638500203}
  ;;                                                                  1000000 ~~~
  ;; {:command :key-pressed, :keycode 90,         :timestamp 1410272947740000}
  ;; {:command :key-released, :keycode 90,        :timestamp 1410272948774000}
  ;;                                                                  1000000 ~~~
  (mp/partition-by-timestamp (mp/get-list))

)
