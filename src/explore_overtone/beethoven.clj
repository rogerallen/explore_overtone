(ns explore-overtone.beethoven
  (:use [overtone.live]))

;; Beginning with some of the ideas in Sam Aaron's example[1],
;; let's see about trying to get rid of the limitation on sequence
;; length.
;;
;; I'd like to create a composition outside the server in "beat space"
;; (play note N at beat B).  This should allow for composing notes via
;; an abstraction that a library like Leipzig [2] provides.  But,
;; there can be java-side timing troubles that will come when there
;; are two time bases (java vs. scsynth).  So, I want to send some
;; notes just ahead of time to the server which plays them with
;; precise timing.
;;
;; I think something like this provides realtime control of
;; tempo/timing with clojure abstraction:
;;
;; ----------------------------------------------------
;;                                   clojure process
;;     [conduct]------>[compose]   "composer/conductor"
;;        ^              |  ^
;; -------|--------------|--|--------------------------
;;        |              |  |        scsynth process
;;        |        ......|..|.......     "player"
;;        |        :play |  |      :
;;        |        :     V  |      :
;; timing |        : [FIFO buffer] :
;;   &    |        :      |        :
;;  ctl   |        :      V        :
;;  sigs  +---------->[note-synth] :
;;        |        :      |        :
;;        |        :      V        :
;;        +--------->[audio-synth] :
;;                 :......|........:
;;                        V
;;                      audio
;;
;; To start, the composer process creates and sends a few notes to the
;; fifo, until it becomes full.  N.B the composer will want to
;; schedule them to be played slightly in the future so the note synth
;; does not get confused.  The scsynth process would watch for valid
;; data and play the notes from the fifo at the proper time.  The
;; performer side uses a FIFO "full" signal to communicate with the
;; the composer.  When the fifo has room, the composer adds more notes
;; until your song is complete.  So, now you have the ability to
;; stream an endless song.
;;
;; The conductor controls coordinating the composition process and the
;; performance.  The conductor may adjust the tempo and other
;; performance information in real time, by using ctl signals.
;;
;; [1] https://github.com/overtone/overtone/blob/master/src/overtone/examples/timing/internal_sequencer.clj
;; [2] https://github.com/ctford/leipzig

;; ======================================================================
;; SCSYNTH SERVER-SIDE PERFORMER
;;
;; these are the buffers containing the notes in the sequence played by
;;
;; max (:control-rate (server-info)) => 689.0625
(def TICKS-PER-SEC 300) ;; 500 had issues. FIXME--what are limits?
;; FIFO size. Larger numbers allow for more separation & slower
;; composition.  Smaller numbers allow for less distance between the
;; conductor & composer.
(def NOTES-PER-FIFO 4)

(defn beats-per-tick
  [tempo]
  (/ TICKS-PER-SEC (/ tempo 60)))

;; ----------------------------------------------------------------------
;; conductor communicates via a fifo of note-on/off/value tuples
;; these are per-voice, not global
;; FIXME - change to a single fifo that accesses "packets" of data
;;       - the size of the packet can be adjusted in the note & audio-synth
;;       - e.g. percussion = single entry for note-on time
;;       - e.g. add amplitude to note control, etc.
(defonce note-on-fifo-buf  (buffer NOTES-PER-FIFO))
(defonce note-off-fifo-buf (buffer NOTES-PER-FIFO))
(defonce note-val-fifo-buf (buffer NOTES-PER-FIFO))
;; cur write index. use modulo for actual index
(defonce fifo-wr-ptr-buf   (buffer 1))
;; #2
(defonce note-on-fifo-buf2  (buffer NOTES-PER-FIFO))
(defonce note-off-fifo-buf2 (buffer NOTES-PER-FIFO))
(defonce note-val-fifo-buf2 (buffer NOTES-PER-FIFO))
(defonce fifo-wr-ptr-buf2   (buffer 1))

;; ----------------------------------------------------------------------
;; Next let's create some global timing buses.
(defonce tick-trg-bus  (control-bus)) ;; global metronome pulse
(defonce tick-cnt-bus  (control-bus)) ;; global metronome count
(defonce beat-trg-bus  (control-bus)) ;; beat pulse
(defonce beat-cnt-bus  (control-bus)) ;; beat count

;; these are per-voice, not global
(defonce fifo-trg-bus  (control-bus)) ;; move to next note in fifo
(defonce fifo-cnt-bus  (control-bus)) ;; fifo read index counter. use
                                      ;; modulo for actual index
(defonce note-gate-bus (control-bus)) ;; tell the audio synth to turn on/off
(defonce note-val-bus  (control-bus)) ;; tell the audio synth what note to play
;; #2
(defonce fifo-trg-bus2  (control-bus))
(defonce fifo-cnt-bus2  (control-bus))
(defonce note-gate-bus2 (control-bus))
(defonce note-val-bus2  (control-bus))

;; ----------------------------------------------------------------------
;; Here we design synths that will drive our pulse buses.
(defsynth tick-trg [rate TICKS-PER-SEC]
  (out:kr tick-trg-bus (impulse:kr rate)))

(defsynth tick-cnt [reset 0]
  (out:kr tick-cnt-bus (pulse-count:kr (in:kr tick-trg-bus) reset)))

(defsynth beat-trg [div 100]
  (out:kr beat-trg-bus (pulse-divider (in:kr tick-trg-bus) div)))

(defsynth beat-cnt [reset 0]
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus) reset)))

(defsynth fifo-cnt [fifo-trg-bus 0 reset 0]
  (out:kr fifo-cnt-bus (pulse-count (in:kr fifo-trg-bus) reset)))

;; This synth watches the fifo and controls the audio synth
(defsynth note-synth [fifo-cnt-bus      0
                      fifo-trg-bus      0
                      note-gate-bus     0
                      note-val-bus      0
                      note-on-fifo-buf  0
                      note-off-fifo-buf 0
                      note-val-fifo-buf 0
                      fifo-wr-ptr-buf   0]
  (let [fifo-wr-ptr   (buf-rd:kr 1 fifo-wr-ptr-buf 0.0 0 1)
        _debug_       (tap "wr-ptr" 10 fifo-wr-ptr)
        fifo-rd-ptr   (in:kr fifo-cnt-bus)
        _debug_       (tap "rd-ptr" 10 fifo-rd-ptr)
        fifo-rd-index (mod (in:kr fifo-cnt-bus) NOTES-PER-FIFO)
        fifo-empty    (= fifo-wr-ptr fifo-rd-ptr)
        _debug_       (tap "empty" 10 fifo-empty)
        fifo-vld      (- 1.0 fifo-empty)
        _debug_       (tap "valid" 10 fifo-vld)
        fifo-full     (= (- fifo-wr-ptr fifo-rd-ptr) NOTES-PER-FIFO)
        _             (tap "full" 10 fifo-full) ;; the only required tap
        note-on       (buf-rd:kr 1 note-on-fifo-buf fifo-rd-index 0 1)
        note-off      (buf-rd:kr 1 note-off-fifo-buf fifo-rd-index 0 1)
        note-val      (buf-rd:kr 1 note-val-fifo-buf fifo-rd-index 0 1)
        beat-trg      (in:kr beat-trg-bus)
        beat-cnt      (in:kr beat-cnt-bus)
        _debug_       (tap "beat" 10 beat-cnt)
        gate-note     (set-reset-ff:kr (and fifo-vld (>= beat-cnt note-on))
                                       (>= beat-cnt note-off))
        _             (tap "gate" 10 gate-note)
        fifo-trg      (and fifo-vld (>= beat-cnt note-off))]
    (out:kr fifo-trg-bus  fifo-trg)   ;; increment read pointer
    (out:kr note-gate-bus gate-note)  ;; turn on/off the note
    (out:kr note-val-bus  note-val))) ;; note value
;;(show-graphviz-synth note-synth)

;; A simple audio synth.
(defsynth audio-synth [note-gate-bus 0
                       note-val-bus  0
                       audio-out-bus 0
                       attack        0.05
                       release       0.2
                       pulse-width1  0.3
                       pulse-width2  0.8
                       pan-pos       0.0
                       note-offset  12
                       lp-freq    1200
                       lp-res        0.25]
  (let [gate-note (in:kr note-gate-bus)
        note-val  (in:kr note-val-bus)
        env       (env-gen (asr attack 1.0 release) :gate gate-note)
        snd       (pulse:ar (midicps note-val) pulse-width1)
        snd       (mix [snd (pulse:ar (midicps (+ note-val note-offset)) pulse-width2)])
        snd       (rlpf snd lp-freq lp-res)
        snd       (* env snd)]
    (out:ar audio-out-bus (pan2 snd pan-pos))))
;;(show-graphviz-synth audio-synth)

;; ======================================================================
;; CLOJURE-SIDE COMPOSER/CONDUCTOR
(def CONDUCTOR-SLEEP-TIME 500)

;; during debug, the conductor could go into a loop waiting forever
;; for the fifo to drain.  This helps you get things back to normal.
(def conductor-alive (atom true))
(defn start-conductor []
  (swap! conductor-alive (fn [_] true)))
(defn stop-conductor []
  (swap! conductor-alive (fn [_] false)))

;; handy for debug.  Almost all taps are strictly for debug.  Only
;; "full" is really necessary.
(defn print-synth-status
  [performer-note-synth]
  (println)
  (println "rd-ptr" @(get-in performer-note-synth [:taps "rd-ptr"])
           "wr-ptr" @(get-in performer-note-synth [:taps "wr-ptr"])
           "empty"  @(get-in performer-note-synth [:taps "empty"])
           "full"   @(get-in performer-note-synth [:taps "full"])
           "valid"  @(get-in performer-note-synth [:taps "valid"])
           "gate"   @(get-in performer-note-synth [:taps "gate"])
           "beat"   @(get-in performer-note-synth [:taps "beat"])))

(defn print-fifo-status
  [note-on-fifo-buf note-off-fifo-buf note-val-fifo-buf fifo-wr-ptr-buf]
  (println "ons   " (map #(nth (buffer-read note-on-fifo-buf) %) (range 4)))
  (println "offs  " (map #(nth (buffer-read note-off-fifo-buf) %) (range 4)))
  (println "notes " (map #(nth (buffer-read note-val-fifo-buf) %) (range 4)))
  (println "wr-ptr" (nth (buffer-read fifo-wr-ptr-buf) 0)))

(defn tap-tap-tap
  "get the conductor & performer back to a good initial condition"
  [beat-counter fifo-counter fifo-wr-ptr-buf]
  (start-conductor)
  (buffer-write! fifo-wr-ptr-buf [0])
  (ctl beat-counter :reset 1) ;; beat-count => 0
  (ctl fifo-counter :reset 1) ;; rd-ptr => 0
  (Thread/sleep 100) ;; need to wait a while in order to work
  (ctl beat-counter :reset 0)
  (ctl fifo-counter :reset 0)
  (Thread/sleep 100))

(defn performer-has-room?
  "is the fifo full or does it have room?"
  [performer-note-synth]
  (let [full @(get-in performer-note-synth [:taps "full"])]
    (not= full 1.0)))

(defn get-note-ons-offs
  "given a list of durations, calculate the times for the note on/off"
  [start-beat cur-lens cur-durs]
  (let [ons  (butlast (reductions + start-beat cur-lens))
        offs (map + ons cur-durs)]
    [ons offs]))

(defn send-note
  "the performer has room, send some notes.  return remaining notes, lengths & durations"
  [cur-notes cur-note-ons cur-note-offs
   note-on-fifo-buf note-off-fifo-buf
   note-val-fifo-buf fifo-wr-ptr-buf]
  (let [wr-ptr (nth (buffer-read fifo-wr-ptr-buf) 0)
        wr-index (mod (int wr-ptr) NOTES-PER-FIFO)]
    ;;(println "  send a note")
    (buffer-write! note-val-fifo-buf wr-index (take 1 cur-notes))
    (buffer-write! note-on-fifo-buf  wr-index (take 1 cur-note-ons))
    (buffer-write! note-off-fifo-buf wr-index (take 1 cur-note-offs))
    (buffer-write! fifo-wr-ptr-buf   [(inc wr-ptr)])
    ;; return remaining notes
    [(drop 1 cur-notes) ;; FIXME rest vs. drop 1?
     (drop 1 cur-note-ons)
     (drop 1 cur-note-offs)]))

(defn try-send-note
  "try sending note to the performer.  return remaining notes, lengths & durations"
  [performer-note-synth cur-notes cur-ons cur-offs
   note-on-fifo-buf note-off-fifo-buf
   note-val-fifo-buf fifo-wr-ptr-buf]
  (if (performer-has-room? performer-note-synth)
    (send-note cur-notes cur-ons cur-offs
               note-on-fifo-buf note-off-fifo-buf
               note-val-fifo-buf fifo-wr-ptr-buf)
    [cur-notes cur-ons cur-offs]))

(defn beethoven
  [beat-counter fifo-counter performer-note-synth
   note-on-fifo-buf note-off-fifo-buf
   note-val-fifo-buf fifo-wr-ptr-buf]
  (let [;; a bit of "ode to joy"
        all-notes (map note [:e4 :e4 :f4 :g4
                             :g4 :f4 :e4 :d4
                             :c4 :c4 :d4 :e4
                             :e4 :d4 :d4])
        ;; ! use integer values that match with beat-counter
        ;; length is distance from note-on to next note-on (?name?)
        all-lens  [4   4   4   4
                   4   4   4   4
                   4   4   4   4
                   4   4   8]
        ;; duration is distance from note-on to note-off (?name?)
        ;; this is a mono synth, so dur < len
        ;; FIXME--the integer resolution is a bit of a limitation
        ;; that would be nice to overcome.
        all-durs  [2   2   3   3
                   2   2   3   3
                   2   2   3   3
                   2   2   7]
        ;; simple tune for debug
        all-notes [60 64 67 69 60 64 67 69]
        all-lens  [2 2 2 2 2 2 2 2]
        all-durs  [1 1 1 1 1 1 1 1]
        ;; let's start on beat 4...
        [all-ons all-offs] (get-note-ons-offs 4 all-lens all-durs)]
    (println "beethoven start")
    (tap-tap-tap beat-counter fifo-counter fifo-wr-ptr-buf)
    (loop [cur-notes all-notes
           cur-ons   all-ons
           cur-offs  all-offs]
      (print-synth-status performer-note-synth)
      (print-fifo-status note-on-fifo-buf note-off-fifo-buf
                         note-val-fifo-buf fifo-wr-ptr-buf)
      (assert (== (count cur-notes) (count cur-ons) (count cur-offs)))
      (if (or (not @conductor-alive) (empty? cur-notes))
        (println "beethoven done") ;; be done, else play your notes
        (let [_ (println (count cur-notes) "notes remain")
              [nxt-notes nxt-ons nxt-offs] (try-send-note performer-note-synth
                                                          cur-notes cur-ons cur-offs
                                                          note-on-fifo-buf note-off-fifo-buf
                                                          note-val-fifo-buf fifo-wr-ptr-buf)]
          (Thread/sleep CONDUCTOR-SLEEP-TIME)
          (recur nxt-notes nxt-ons nxt-offs))))))

(comment
  (do
    (stop)
    (def tick-trigger    (tick-trg))
    (def tick-counter    (tick-cnt [:after tick-trigger]))
    (def beat-trigger    (beat-trg [:after tick-trigger] (beats-per-tick 120)))
    (def beat-counter    (beat-cnt [:after beat-trigger]))
    ;; 1
    (def note-performer  (note-synth fifo-cnt-bus fifo-trg-bus note-gate-bus note-val-bus
                                     note-on-fifo-buf note-off-fifo-buf
                                     note-val-fifo-buf fifo-wr-ptr-buf))
    (def snd-performer   (audio-synth [:after note-performer] note-gate-bus note-val-bus 0 :pan-pos -1.0))
    (def fifo-counter    (fifo-cnt [:after note-performer] fifo-trg-bus))
    (tap-tap-tap beat-counter fifo-counter fifo-wr-ptr-buf)
    ;; 2
    (def note-performer2 (note-synth fifo-cnt-bus2 fifo-trg-bus2 note-gate-bus2 note-val-bus2
                                     note-on-fifo-buf2 note-off-fifo-buf2
                                     note-val-fifo-buf2 fifo-wr-ptr-buf2))
    (def snd-performer2  (audio-synth [:after note-performer2] note-gate-bus2 note-val-bus2 0 :pan-pos 1.0))
    (def fifo-counter2   (fifo-cnt [:after note-performer2] fifo-trg-bus2))
    (tap-tap-tap beat-counter fifo-counter2 fifo-wr-ptr-buf2)
    )

  ;; put composer in another thread to allow for realtime control
  (do
    (def bp (future (beethoven beat-counter fifo-counter note-performer
                               note-on-fifo-buf note-off-fifo-buf
                               note-val-fifo-buf fifo-wr-ptr-buf)))
    (def bp2 (future (beethoven beat-counter fifo-counter2 note-performer2
                                note-on-fifo-buf2 note-off-fifo-buf2
                                note-val-fifo-buf2 fifo-wr-ptr-buf2)))
    )

  (stop-conductor)
  ;; you're the conductor...
  ;; realtime control of tempo
  (ctl beat-trigger :div (beats-per-tick 240))
  (ctl beat-trigger :div (beats-per-tick 480))
  ;; realtime control of audio synth
  (ctl snd-performer :attack 0.25)
  (ctl snd-performer :release 0.3)
  (ctl snd-performer :pulse-width1 0.8)
  (ctl snd-performer :pulse-width2 0.8)
  (ctl snd-performer :pan-pos -1.0)
  (ctl snd-performer :note-offset 7)
  (ctl snd-performer :lp-freq 1000)
  (ctl snd-performer :lp-freq 2000)
  (ctl snd-performer :lp-res  0.6)

  (ctl snd-performer2 :pan-pos 0.5)
  (print-synth-status note-performer)
)
