(ns explore-overtone.servbeats
  (:require [overtone.live :as o]))

;; based on overtone/src/overtone/examples/timing/interal_sequencer.clj
;; code rearranged to fit my brain + add polyrhythm

;; Default constants to use
;; 120Hz = 7200bpm & 6000/30 = 240bpm or 4bps
;; 16 beats = 4 seconds
(def DEFAULT-ROOT-RATE     120)
(def DEFAULT-BEAT-DIVISOR   30)
(def DEFAULT-SEQUENCE-BEATS 16)
(def MAX-BEAT-BUSES          8)

(defn get-rate-divisor
  [num-beats seq-len-in-seconds root-rate-in-hz]
  (let [bps (/ num-beats seq-len-in-seconds)]
    (/ root-rate-in-hz bps)))

;; put global metronome pulse on root-trg-bus
(defonce root-trg-bus (o/control-bus))
(o/defsynth root-trg-synth [rate DEFAULT-ROOT-RATE]
  (o/out:kr root-trg-bus (o/impulse:kr rate)))

;; put global metronome count on root-cnt-bus
(defonce root-cnt-bus (o/control-bus))
(o/defsynth root-cnt-synth []
  (o/out:kr root-cnt-bus (o/pulse-count:kr (o/in:kr root-trg-bus))))

(defn dovec [xs]
  "doall and convert sequence to vector"
  (apply vector (doall xs)))

;; put out polyrhythm beat pulses on beatN-trg-bus's
(defonce beat-trg-buses
  (dovec (for [i (range MAX-BEAT-BUSES)]
           (o/control-bus))))
(o/defsynth beat-trg-synth [beat-bus 0
                            div DEFAULT-BEAT-DIVISOR]
  (o/out:kr beat-bus (o/pulse-divider (o/in:kr root-trg-bus) div)))

;; put out beat counts on beatN-cnt-bus's
(defonce beat-cnt-buses
  (dovec (for [i (range MAX-BEAT-BUSES)]
           (o/control-bus))))
(o/defsynth beat-cnt-synth [cnt-bus 0 trg-bus 0]
  (o/out:kr cnt-bus (o/pulse-count (o/in:kr trg-bus))))

(o/defsynth mono-sample-beat-synth
  "Plays a single channel audio buffer on a particular beat."
  [beat-num     0 ; the beat number of this synth
   beat-buf     0 ; the buffer of all beats 0=off, 1=on
   seq-beats    8 ; the number of beats in a sequence
   beat-cnt-bus 0 ; the beat count bus
   beat-trg-bus 0 ; the beat trigger bus
   sample-buf   0 ; the sample buffer to play
   sample-rate  1 ; rate of sample-buffer playback
   amp          1 ; output volume
   out-bus      0]
  (let [cnt           (o/in:kr beat-cnt-bus)
        beat-trg      (o/in:kr beat-trg-bus)
        this-beat-trg (and (o/buf-rd:kr 1 beat-buf cnt)
                           (= beat-num (mod cnt seq-beats))
                           beat-trg)
        vol           (o/set-reset-ff this-beat-trg)]
    (o/out out-bus
         (* vol amp
            (o/pan2 (o/scaled-play-buf 1 sample-buf sample-rate this-beat-trg))))))

;; collection of synths
(def kick-s (o/sample (o/freesound-path 777)))
(def click-s (o/sample (o/freesound-path 406)))
(def snare (o/sample (o/freesound-path 26903)))
(def kick (o/sample (o/freesound-path 2086)))
(def close-hihat (o/sample (o/freesound-path 802)))
(def open-hihat (o/sample (o/freesound-path 26657)))
(def clap (o/sample (o/freesound-path 48310)))
(def gshake (o/sample (o/freesound-path 113625)))

;; control atoms
(defonce root-trg-atom (atom nil))
(defonce root-cnt-atom (atom nil))
(defonce beat-trgs-atom (atom []))
(defonce beat-cnts-atom (atom []))
;; list of o/buffers.  one for each sequence. buffers are the length
;; of the corresponding sequence and contain either 1 or 0 to control
;; playing the sample on that beat.  Initialized to be all 0
(defonce seq-bufs-atom (atom []))
;; a vector of vectors of synths.  each sequence contatains one synth
;; per beat that plays the sample
(defonce synth-seqs-atom (atom []))

;; get all busses counting and running
(defn restart [seq-seconds
               beat-seq-beats
               beat-seq-synths]
  (let [beat-set (apply vector (sort (set beat-seq-beats)))]
    (assert (>= MAX-BEAT-BUSES (count beat-set)))
    (assert (= (count beat-seq-beats) (count beat-seq-synths)))

    (swap! root-trg-atom (fn [_] (root-trg-synth)))
    (swap! root-cnt-atom (fn [_] (root-cnt-synth)))
    (swap! beat-trgs-atom
           (fn [_] (dovec (for [i (range (count beat-set))]
                           (let [beat-trg-bus (nth beat-trg-buses i)
                                 num-beats    (nth beat-set i)
                                 cur-divisor  (get-rate-divisor num-beats seq-seconds DEFAULT-ROOT-RATE)]
                             (beat-trg-synth beat-trg-bus cur-divisor))))))
    (swap! beat-cnts-atom
           (fn [_] (dovec (for [i (range (count beat-set))]
                           (let [beat-trg-bus (nth beat-trg-buses i)
                                 beat-cnt-bus (nth beat-cnt-buses i)]
                             (beat-cnt-synth beat-cnt-bus beat-trg-bus))))))
    (swap! seq-bufs-atom
           (fn [_] (dovec (for [num-beats beat-seq-beats]
                           (o/buffer num-beats)))))

    (swap! synth-seqs-atom
      (fn [_] (dovec (for [[seq-index num-beats] (map-indexed vector beat-seq-beats)]
                      (let [beat-index (.indexOf beat-set num-beats)
                            seq-buf      (nth @seq-bufs-atom seq-index)
                            beat-cnt-bus (nth beat-cnt-buses beat-index)
                            beat-trg-bus (nth beat-trg-buses beat-index)
                            synth        (nth beat-seq-synths seq-index)]
                        (dovec
                         (for [k (range num-beats)]
                           (mono-sample-beat-synth
                            :beat-num     k
                            :beat-buf     seq-buf
                            :seq-beats    num-beats
                            :beat-cnt-bus beat-cnt-bus
                            :beat-trg-bus beat-trg-bus
                            :sample-buf   synth))))))))))

(defn set-seq-ctl
  [seq-index key value]
  (doall (map #(o/ctl % key value) (@synth-seqs-atom seq-index)))
  nil)

(defn set-seq-buf
  [seq-index new-buf]
  (o/buffer-write! (nth @seq-bufs-atom seq-index) new-buf)
  nil)

(comment

  (o/stop)
  (restart 4 [16     12     12          8]
             [kick-s kick-s close-hihat open-hihat])

  (do
    ;;                                1 1 1 1 1 1 1
    ;;              1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
    (set-seq-buf 0 [1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0])
    (set-seq-buf 1 [1 0 1 0 1 0 1 0 1 0 1 0])
    (set-seq-buf 2 [0 1 0 1 0 1 0 1 0 1 0 1])
    (set-seq-buf 3 [0 1 0 1 0 1 0 1])
    )

  (do
    ;;                                1 1 1 1 1 1 1
    ;;              1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
    (set-seq-buf 0 [1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0])
    (set-seq-buf 1 [1 0 0 0 1 0 0 0 1 0 0 0])
    (set-seq-buf 2 [0 0 0 1 0 0 0 1 0 0 0 1])
    (set-seq-buf 3 [0 1 0 1 0 0 0 1])
    )

  (do
    ;;                                1 1 1 1 1 1 1
    ;;              1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
    (set-seq-buf 0 [0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0])
    (set-seq-buf 1 [1 0 1 0 1 0 1 0 1 0 1 0])
    (set-seq-buf 2 [0 1 0 0 0 1 0 0 0 1 0 0])
    (set-seq-buf 3 [0 0 1 1 0 0 1 1])
    )


  ;; adjust tempo via
  (o/ctl @root-trg-atom :rate 124)

  ;; adjust sound via
  (set-seq-ctl 1 :sample-buf kick-s)
  (set-seq-ctl 3 :amp 0.65)

)
