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
;; ----------------------------------------------
;;   [compose/create notes]    clojure process
;;           |                   "composer"
;;           V
;;     [conduct notes]           "conductor"
;;        ^       |
;; -------|-------|-----------------------------
;;        |       V           scsynth process
;; timing |  [FIFO buffer]       "player"
;;   &    |       |
;;  ctl   |       V
;;  sigs  +->[note-synth]
;;        |       |
;;        |       V
;;        +->[audio-synth]
;;                |
;;                V
;;              audio
;;
;; To start, you'd create and send a few notes to the fifo, until it
;; was full.  You would want to schedule them to be played slightly in
;; the future so the note synth does not get confused.  The scsynth
;; process would watch for valid data and play the notes from the
;; fifo.  The performer side uses a FIFO "full" signal to communicate
;; with the the conductor.  When the fifo has room, add more notes
;; until your song is complete.  So, now you have the ability to
;; stream an endless song.
;;
;; You are not limited by sending data through the fifo only.  You may
;; adjust the tempo and other performance information in real time, by
;; using ctl signals.
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
(def NOTES-PER-FIFO 4)  ;; FIFO size

(defn beats-per-tick
  [tempo]
  (/ TICKS-PER-SEC (/ tempo 60)))

;; conductor communicates via a fifo of note-on/off/value tuples
(defonce note-on-fifo-buf  (buffer NOTES-PER-FIFO))
(defonce note-off-fifo-buf (buffer NOTES-PER-FIFO))
(defonce note-val-fifo-buf (buffer NOTES-PER-FIFO))
(defonce fifo-wr-ptr-buf   (buffer 1)) ;; cur write index. use modulo
                                       ;; for actual index

;; Next let's create some global timing buses.
(defonce tick-trg-bus  (control-bus)) ;; global metronome pulse
(defonce tick-cnt-bus  (control-bus)) ;; global metronome count
(defonce beat-trg-bus  (control-bus)) ;; beat pulse
(defonce beat-cnt-bus  (control-bus)) ;; beat count

;; these are per-voice, not global
(defonce fifo-trg-bus  (control-bus)) ;; move to next in fifo
(defonce fifo-cnt-bus  (control-bus)) ;; fifo read index counter. use
                                      ;; modulo for actual index
(defonce note-gate-bus (control-bus)) ;; tell the audio synth to turn on/off
(defonce note-val-bus  (control-bus)) ;; tell the audio synth what note to play

;; Here we design synths that will drive our pulse buses.
(defsynth tick-trg [rate TICKS-PER-SEC]
  (out:kr tick-trg-bus (impulse:kr rate)))

(defsynth tick-cnt [reset 0]
  (out:kr tick-cnt-bus (pulse-count:kr (in:kr tick-trg-bus) reset)))

(defsynth beat-trg [div 100]
  (out:kr beat-trg-bus (pulse-divider (in:kr tick-trg-bus) div)))

(defsynth beat-cnt [reset 0]
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus) reset)))

(defsynth fifo-cnt [reset 0]
  (out:kr fifo-cnt-bus (pulse-count (in:kr fifo-trg-bus) reset)))

;; This synth watches the fifo and controls the audio synth
(defsynth note-synth []
  (let [fifo-wr-ptr   (buf-rd:kr 1 fifo-wr-ptr-buf 0.0 0 1)
        fifo-rd-ptr   (in:kr fifo-cnt-bus)
        _             (tap "wr-ptr" 10 fifo-wr-ptr)
        _             (tap "rd-ptr" 10 fifo-rd-ptr)
        fifo-rd-index (mod (in:kr fifo-cnt-bus) NOTES-PER-FIFO)
        fifo-empty    (= fifo-wr-ptr fifo-rd-ptr)
        _              (tap "empty" 10 fifo-empty)
        fifo-vld      (- 1.0 fifo-empty)
        _              (tap "valid" 10 fifo-vld)
        fifo-full     (= (- fifo-wr-ptr fifo-rd-ptr) NOTES-PER-FIFO)
        _              (tap "full" 10 fifo-full) ;; this is the only
        note-on        (buf-rd:kr 1 note-on-fifo-buf fifo-rd-index 0 1)
        note-off       (buf-rd:kr 1 note-off-fifo-buf fifo-rd-index 0 1)
        note-val       (buf-rd:kr 1 note-val-fifo-buf fifo-rd-index 0 1)
        beat-trg       (in:kr beat-trg-bus)
        beat-cnt       (in:kr beat-cnt-bus)
        _              (tap "beat" 10 beat-cnt)
        gate-note      (set-reset-ff:kr (and fifo-vld (>= beat-cnt note-on))
                                        (>= beat-cnt note-off))
        _              (tap "gate" 10 gate-note)
        fifo-trg      (and fifo-vld (>= beat-cnt note-off))]
    (out:kr fifo-trg-bus  fifo-trg)  ;; increment read pointer
    (out:kr note-gate-bus gate-note) ;; turn on the note
    (out:kr note-val-bus  note-val)))
;;(show-graphviz-synth note-synth)

;; A simple audio synth.
(defsynth audio-synth [lp-freq 1200 lp-res 0.25]
  (let [gate-note     (in:kr note-gate-bus)
        note-val      (in:kr note-val-bus)
        env           (env-gen (asr 0.1 1.0 0.1) :gate gate-note)
        snd           (pulse:ar (midicps note-val) 0.6)
        snd           (mix [snd (pulse:ar (midicps (- note-val 24)) 0.4)])
        snd           (rlpf snd lp-freq lp-res)
        snd           (* env snd)]
    (out:ar 0 (pan2 snd))))
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
(defn print-status
  [performer-note-synth]
  (println)
  (println "rd-ptr" @(get-in performer-note-synth [:taps "rd-ptr"])
           "wr-ptr" @(get-in performer-note-synth [:taps "wr-ptr"])
           ;;"wr-ptr" (nth (buffer-read fifo-wr-ptr-buf) 0)
           "empty"  @(get-in performer-note-synth [:taps "empty"])
           "full"   @(get-in performer-note-synth [:taps "full"])
           "valid"  @(get-in performer-note-synth [:taps "valid"])
           "gate"   @(get-in performer-note-synth [:taps "gate"])
           "beat"   @(get-in performer-note-synth [:taps "beat"]))
  ;;(println "notes" (map #(nth (buffer-read note-val-fifo-buf) %) (range 4)))
  ;;(println "ons  " (map #(nth (buffer-read note-on-fifo-buf) %) (range 4)))
  ;;(println "offs " (map #(nth (buffer-read note-off-fifo-buf) %) (range 4)))
  )

(defn tap-tap-tap
  "get the conductor & performer back to a good initial condition"
  [beat-counter fifo-counter]
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
  [cur-notes cur-note-ons cur-note-offs]
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
  [performer-note-synth cur-notes cur-ons cur-offs]
  (if (performer-has-room? performer-note-synth)
    (send-note cur-notes cur-ons cur-offs)
    [cur-notes cur-ons cur-offs]))

(defn beethoven
  [beat-counter fifo-counter performer-note-synth]
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
        ;;all-notes [60 64 67 69 60 64 67 69]
        ;;all-lens  [2 2 2 2 2 2 2 2]
        ;;all-durs  [1 1 1 1 1 1 1 1]
        ;; let's start on beat 4...
        [all-ons all-offs] (get-note-ons-offs 4 all-lens all-durs)]
    (println "beethoven start")
    (tap-tap-tap beat-counter fifo-counter)
    (loop [cur-notes all-notes
           cur-ons   all-ons
           cur-offs  all-offs]
      ;;(print-status performer-note-synth)
      ;;(println "loop" cur-notes cur-lens cur-durs)
      (assert (== (count cur-notes) (count cur-ons) (count cur-offs)))
      (if (or (not @conductor-alive) (empty? cur-notes))
        (println "beethoven done") ;; be done, else play your notes
        (let [_ (println (count cur-notes) "notes remain")
              [nxt-notes nxt-ons nxt-offs] (try-send-note
                                            performer-note-synth
                                            cur-notes cur-ons cur-offs)]
          (Thread/sleep CONDUCTOR-SLEEP-TIME)
          (recur nxt-notes nxt-ons nxt-offs))))))

(comment
  (do
    (stop)
    (def tick-trigger   (tick-trg))
    (def tick-counter   (tick-cnt [:after tick-trigger]))
    (def beat-trigger   (beat-trg [:after tick-trigger] (beats-per-tick 120)))
    (def beat-counter   (beat-cnt [:after beat-trigger]))
    (def note-performer (note-synth))
    (def snd-performer  (audio-synth [:after note-performer]))
    (def fifo-counter   (fifo-cnt [:after note-performer]))
    (tap-tap-tap beat-counter fifo-counter))

  ;; put conductor in another thread to allow for realtime control
  (def bp (future (beethoven beat-counter fifo-counter note-performer)))
  ;; realtime control of tempo
  (ctl beat-trigger :div (beats-per-tick 120))
  (ctl beat-trigger :div (beats-per-tick 240))
  (ctl beat-trigger :div (beats-per-tick 480))
  ;; realtime control of audio synth
  (ctl snd-performer :lp-freq 800)
  (ctl snd-performer :lp-freq 2800)

  (print-status performer-note-synth)
)
