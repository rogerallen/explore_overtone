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
;; max (:control-rate (server-info)) => 689.0625
(def TICKS-PER-SEC 2210) ;; 300 ? 500 had issues. FIXME--what are limits?
;; 300 ticks/second / 32 = 9.375 beats/second * 60 = 562 bpm max
;; using "quaver" as a sub-beat measure
(def QUAVERS-PER-BEAT 16) ;; each beat has 1/32nd beat resolution
(defn ticks-per-beat
  [tempo]
  ;; tick/sec / (beats/min / sec/min) => tick/sec / beats/sec => ticks/beat
  (/ TICKS-PER-SEC (/ tempo 60)))
(defn ticks-per-quaver
  [tempo]
  ;; ticks/beat / quavers/beat
  (/ (ticks-per-beat tempo) QUAVERS-PER-BEAT))

;; FIFO size. Larger numbers allow for more separation & slower
;; composition.  Smaller numbers allow for less distance between the
;; conductor & composer.
(def NOTES-PER-FIFO 4)
(def FLOATS-PER-NOTE 3) ;; FIXME go to single fifo/player

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
;; wanting 1/16th beat accuracy is just too much at 200bpm for control-bus
;; switching to audio-bus for control.
(defonce tick-trg-bus   (audio-bus)) ;; global metronome pulse
(defonce tick-cnt-bus   (audio-bus)) ;; global metronome count
(defonce quaver-trg-bus (audio-bus)) ;; sub-beat = quaver pulse
(defonce beat-cnt-bus   (audio-bus)) ;; beat count (floating-point)

;; these are per-voice, not global
(defonce fifo-trg-bus   (audio-bus)) ;; move to next note in fifo
(defonce fifo-cnt-bus   (audio-bus)) ;; fifo read index counter. use
                                       ;; modulo for actual index
(defonce note-gate-bus  (control-bus)) ;; tell the audio synth to turn on/off
(defonce note-val-bus   (control-bus)) ;; tell the audio synth what note to play
;; #2
(defonce fifo-trg-bus2  (audio-bus))
(defonce fifo-cnt-bus2  (audio-bus))
(defonce note-gate-bus2 (control-bus))
(defonce note-val-bus2  (control-bus))

;; ----------------------------------------------------------------------
;; Here we design synths that will drive our pulse buses.
(defsynth tick-trg [rate TICKS-PER-SEC]
  (out:ar tick-trg-bus (impulse:ar rate)))

(defsynth ar-pulse-divider [trg-bus 0 div 100]
  "use global tick-trg-bus to create a slower tick"
  (out:ar trg-bus (pulse-divider:ar (in:ar tick-trg-bus) div)))

(defsynth ar-counter-divider
  "count triggers on trg-bus, divide by div, send float value onto cnt-bus"
  [trg-bus 0 cnt-bus 0 div QUAVERS-PER-BEAT reset 0]
  (out:ar cnt-bus (/ (pulse-count:ar (in:ar trg-bus) reset) div)))

(defsynth ar-counter [trg-bus 0 cnt-bus 0 reset 0]
  (out:ar cnt-bus (pulse-count:ar (in:ar trg-bus) reset)))

;; This synth watches the fifo and controls the audio synth
(defsynth note-synth [fifo-cnt-bus      0 ; in
                      ;beat-cnt-bus     0 ; in (global)
                      fifo-trg-bus      0 ; out
                      note-gate-bus     0 ; out
                      note-val-bus      0 ; out
                      note-on-fifo-buf  0
                      note-off-fifo-buf 0
                      note-val-fifo-buf 0
                      fifo-wr-ptr-buf   0]
  (let [kr-fifo-wr-ptr   (buf-rd:kr 1 fifo-wr-ptr-buf 0.0 0 1)
        _debug_          (tap "wr-ptr" 10 kr-fifo-wr-ptr)
        fifo-rd-ptr      (in:ar fifo-cnt-bus)
        kr-fifo-rd-ptr   (a2k fifo-rd-ptr)
        _debug_          (tap "rd-ptr" 10 kr-fifo-rd-ptr)
        kr-fifo-rd-index (mod kr-fifo-rd-ptr NOTES-PER-FIFO)
        kr-fifo-empty    (= kr-fifo-wr-ptr kr-fifo-rd-ptr)
        _debug_          (tap "empty" 10 kr-fifo-empty)
        kr-fifo-vld      (- 1.0 kr-fifo-empty)
        _debug_          (tap "valid" 10 kr-fifo-vld)
        kr-fifo-full     (= (- kr-fifo-wr-ptr kr-fifo-rd-ptr) NOTES-PER-FIFO)
        _                (tap "full" 10 kr-fifo-full) ;; the only required tap
        kr-note-on       (buf-rd:kr 1 note-on-fifo-buf  kr-fifo-rd-index 0 1)
        kr-note-off      (buf-rd:kr 1 note-off-fifo-buf kr-fifo-rd-index 0 1)
        kr-note-val      (buf-rd:kr 1 note-val-fifo-buf kr-fifo-rd-index 0 1)
        beat-cnt         (in:ar beat-cnt-bus) ;; NOTE assumed global
        kr-beat-cnt      (a2k beat-cnt)
        _debug_          (tap "beat" 10 kr-beat-cnt)
        kr-gate-note     (set-reset-ff:kr (and kr-fifo-vld (>= kr-beat-cnt kr-note-on))
                                          (>= kr-beat-cnt kr-note-off))
        _                (tap "gate" 10 kr-gate-note)
        fifo-trg         (and kr-fifo-vld (>= beat-cnt kr-note-off))]
    (out:ar fifo-trg-bus fifo-trg)   ;; increment read pointer
    (out:kr note-gate-bus kr-gate-note)  ;; turn on/off the note
    (out:kr note-val-bus  kr-note-val))) ;; note value
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
        ;; NOTE: can now use float values for beats
        ;; length is distance from note-on to next note-on
        all-lens  [1   1   1   1
                   1   1   1   1
                   1   1   1   1
                   1   1   2]
        ;; duration is distance from note-on to note-off
        ;; this is a mono synth, so dur must be < len
        all-durs  [0.5 0.5 0.8 0.8
                   0.5 0.5 0.8 0.8
                   0.5 0.5 0.8 0.8
                   0.9 0.5 1.75]
        ;; simple tune for debug
        ;;all-notes [60 64 67 69 60 64 67 69]
        ;;all-lens  [2 2 2 2 2 2 2 2]
        ;;all-durs  [1 1 1 1 1 1 1 1]
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

  ;; test 1 - left ear...
  (do
    (stop)
    (def tick-trigger   (tick-trg))
    (def tick-counter   (ar-counter [:after tick-trigger] tick-trg-bus tick-cnt-bus))
    (def quaver-trigger (ar-pulse-divider [:after tick-trigger] quaver-trg-bus (ticks-per-quaver 100)))
    (def beat-counter   (ar-counter-divider [:after quaver-trigger] quaver-trg-bus beat-cnt-bus))
    ;; 1
    (def note-performer (note-synth [:after beat-counter]
                                    fifo-cnt-bus fifo-trg-bus note-gate-bus note-val-bus
                                    note-on-fifo-buf note-off-fifo-buf
                                    note-val-fifo-buf fifo-wr-ptr-buf))
    (def snd-performer  (audio-synth [:after note-performer] note-gate-bus note-val-bus 0 :pan-pos -1.0))
    (def fifo-counter   (ar-counter [:after note-performer] fifo-trg-bus fifo-cnt-bus))

    (def bp (future (beethoven beat-counter fifo-counter note-performer
                               note-on-fifo-buf note-off-fifo-buf
                               note-val-fifo-buf fifo-wr-ptr-buf)))
    )
  (stop-conductor)
  (stop)

  ;; test 2 - right ear...
  (do
    (stop)
    (def tick-trigger    (tick-trg))
    (def tick-counter    (kr-counter [:after tick-trigger] tick-trg-bus tick-cnt-bus))
    (def beat-counter    (kr-divider [:after tick-counter] tick-cnt-bus (ticks-per-beat 300) beat-cnt-bus))
    (def note-performer2 (note-synth fifo-cnt-bus2 fifo-trg-bus2 note-gate-bus2 note-val-bus2
                                     note-on-fifo-buf2 note-off-fifo-buf2
                                     note-val-fifo-buf2 fifo-wr-ptr-buf2))
    (def snd-performer2  (audio-synth [:after note-performer2] note-gate-bus2 note-val-bus2 0 :pan-pos 1.0))
    (def fifo-counter2   (kr-counter [:after note-performer2] fifo-trg-bus2 fifo-cnt-bus2))
    (def bp2 (future (beethoven tick-counter fifo-counter2 note-performer2
                                note-on-fifo-buf2 note-off-fifo-buf2
                                note-val-fifo-buf2 fifo-wr-ptr-buf2)))
    )
  (stop-conductor)
  (stop)

  ;; Test 3 - now try both
  (do
    (stop)
    (def tick-trigger    (tick-trg))
    (def tick-counter    (kr-counter [:after tick-trigger] tick-trg-bus tick-cnt-bus))
    (def beat-counter    (kr-divider [:after tick-counter] tick-cnt-bus (ticks-per-beat 300) beat-cnt-bus))
    ;; 1
    (def note-performer  (note-synth fifo-cnt-bus fifo-trg-bus note-gate-bus note-val-bus
                                     note-on-fifo-buf note-off-fifo-buf
                                     note-val-fifo-buf fifo-wr-ptr-buf))
    (def snd-performer   (audio-synth [:after note-performer] note-gate-bus note-val-bus 0 :pan-pos -1.0))
    (def fifo-counter    (kr-counter [:after note-performer] fifo-trg-bus fifo-cnt-bus))
    ;;(tap-tap-tap beat-counter fifo-counter fifo-wr-ptr-buf)
    ;; 2
    (def note-performer2 (note-synth fifo-cnt-bus2 fifo-trg-bus2 note-gate-bus2 note-val-bus2
                                     note-on-fifo-buf2 note-off-fifo-buf2
                                     note-val-fifo-buf2 fifo-wr-ptr-buf2))
    (def snd-performer2  (audio-synth [:after note-performer2] note-gate-bus2 note-val-bus2 0 :pan-pos 1.0))
    (def fifo-counter2   (kr-counter [:after note-performer2] fifo-trg-bus2 fifo-cnt-bus2))
    ;;(tap-tap-tap beat-counter fifo-counter2 fifo-wr-ptr-buf2)
    )

  ;; adjust for a few differences
  (ctl snd-performer :note-offset 7)
  (ctl snd-performer2 :note-offset 19)

  ;; put composer in another thread to allow for realtime control
  (do
    (def bp (future (beethoven tick-counter fifo-counter note-performer
                               note-on-fifo-buf note-off-fifo-buf
                               note-val-fifo-buf fifo-wr-ptr-buf)))
    (def bp2 (future (beethoven tick-counter fifo-counter2 note-performer2
                                note-on-fifo-buf2 note-off-fifo-buf2
                                note-val-fifo-buf2 fifo-wr-ptr-buf2)))
    )

  (stop-conductor)
  (stop)
  ;; you're the conductor...


  ;; BOOM. Here is where using a divider fails.  Need a hybrid of some sort...
  ;; realtime control of tempo
  (ctl beat-counter :div (ticks-per-beat 240))
  (ctl beat-counter :div (ticks-per-beat 480))


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
