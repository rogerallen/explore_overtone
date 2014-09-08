(ns explore-overtone.midi-persi
  (:use [overtone.music.time :only [now]]
        [overtone.libs.event :only [on-event remove-event-handler]])
  (:require [persi.persi :as persi]
            [quil.core :as q]))

;; ======================================================================
;; 10 seconds should be pretty decent
(def partition-threshold 10000000)

;; ======================================================================
;; echo persi api
(def get-list persi/get-list)
(def get-map persi/get-map)
(def get-file-name persi/get-file-name)
(def get-dir-name persi/get-dir-name)
(def dirty? persi/dirty?)
(def init! persi/init!)
(def new! persi/new!)
(def save! persi/save!)
(def open! persi/open!)
(def append! persi/append!)
(def set! persi/set!)
(def summary persi/summary)

;; ======================================================================
(defn record
  "Record note on/off events to the midipe-events list"
  ([] (record [:midi] ::midipe))
  ([device-key player-key]
     (let [on-event-key  (concat device-key [:note-on])
           off-event-key (concat device-key [:note-off])
           on-key        (concat [::midipe] on-event-key)
           off-key       (concat [::midipe] off-event-key)]
       (on-event on-event-key
                 (fn [{note :note
                      velocity :velocity
                      timestamp :timestamp}]
                   (append! {:command :note-on
                                   :note note
                                   :velocity velocity
                                   :timestamp timestamp}))
                 on-key)
       (on-event off-event-key
                 (fn [{note :note
                      velocity :velocity
                      timestamp :timestamp}]
                   (append! {:command :note-off
                                   :note note
                                   :velocity velocity
                                   :timestamp timestamp}))
                 off-key))))

(defn pause
  "Pause recording note on/off events to the midipe-events list"
  []
  (remove-event-handler [::midipe :midi :note-on])
  (remove-event-handler [::midipe :midi :note-off]))

;; ======================================================================
;; manipulating events
(defn- events-timestamp-Δ
  "read list of midi events and add timestamp-Δ (cur-prev timestamp) to
  each event"
  [the-list]
  (cons
   (assoc (first the-list) :timestamp-̣Δ 0)
   (map #(assoc %2 :timestamp-̣Δ (- (:timestamp %2) (:timestamp %1)))
        the-list
        (rest the-list))))

;; variation of partition-by
(defn- partition-at-true
  "Applies f to each value in coll, splitting it each time f returns
   true.  Returns a lazy seq of partitions."
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(= false (f %)) (next s)))]
       (cons run (partition-at-true f (seq (drop (count run) s))))))))

;; FIXME?  Do I need to worry about note-on ... note-off pairs that go
;;   over threshold?  idea--count note-on as +, note-off as -.  Don't
;;   end unless sum is 0.

;; FIXME make a version that takes a partition-threshold
(defn partition-by-timestamp
  "create a lazy seq of event seqs partitioned when timestamp goes
  over a threshold."
  [the-list]
  (partition-at-true
   #(> (:timestamp-̣Δ %) partition-threshold)
   (events-timestamp-Δ the-list)))

(defn make-notes
  "convert a sequence of events into a sequence of notes with
  duration.  Also changes timestamps from microseconds to milliseconds."
  [event-list0]
  (let [ft (:timestamp (first (drop-while #(not= (:command %) :note-on) event-list0)))]
    (loop [event-list event-list0 first-timestamp ft notes []]
      (let [event-list   (drop-while #(not= (:command %) :note-on) event-list)
            cur-note-on  (first event-list)
            event-list   (rest event-list)
            cur-note-off (first
                          (drop-while
                           #(or (= (:command %) :note-on)
                                (not= (:note %) (:note cur-note-on))) event-list))]
        (if (not (nil? cur-note-on))
          (let [;;_ (println cur-note-on "\n" cur-note-off "\n")
                cur-note {:command   :note
                          :note      (:note cur-note-on)
                          :velocity  (:velocity cur-note-on)
                          :timestamp (/ (- (:timestamp cur-note-on) first-timestamp)
                                        1000.0)
                          :duration  (/ (- (:timestamp cur-note-off)
                                           (:timestamp cur-note-on))
                                        1000.0)}
                notes (conj notes cur-note)]
            (recur event-list first-timestamp notes))
          notes)))))

(defonce record-keyboard-keycount (atom 0))

(defn- record-keyboard-setup
  []
  (reset! record-keyboard-keycount 0)
  (q/background 255))

(defn- record-keyboard-draw
  []
  (if (> @record-keyboard-keycount 0)
    (q/background 128)
    (q/background 255)))

(defn- record-keyboard-key-pressed
  []
  (swap! record-keyboard-keycount inc)
  (append! {:command   :key-pressed
            :keycode   (q/key-code)
            :timestamp (* 1000 (now))}))

(defn- record-keyboard-key-released
  []
  (swap! record-keyboard-keycount dec)
  (append! {:command   :key-released
            :keycode   (q/key-code)
            :timestamp (* 1000 (now))}))

(defn record-keyboard-events
  "bring up quil window that allows for recording keyboard events"
  []
  (q/defsketch keyboard-sketch
    :title        "midi-persi-keys"
    :setup        record-keyboard-setup
    :draw         record-keyboard-draw
    :key-pressed  record-keyboard-key-pressed
    :key-released record-keyboard-key-released
    :size         [200 200]))

(defn keyboard-tempo
  "look at the last partition's keyboard events and return the bpm"
  []
  (let [timestamp-̣Δs (map :timestamp-̣Δ
                          (filter #(= (:command %) :key-pressed)
                                  (rest (last (partition-by-timestamp (get-list))))))
        sum (apply + timestamp-̣Δs)
        ave-spb (/ (/ sum (count timestamp-̣Δs)) 1000000.0)
        ave-tempo (* 60.0 (/ ave-spb))]
    ave-tempo))
