(ns explore-overtone.midi-persi
  (:use [overtone.music.time :only [now]]
        [overtone.libs.event :only [on-event remove-handler]])
  (:require [persi.core :as persi]))

;; ======================================================================
;; 2 seconds should be pretty decent
(def partition-threshold 2000000)

;; ======================================================================
;; echo persi api
(def dirty? persi/dirty?)
(def init! persi/init!)
(def new! persi/new!)
(def save! persi/save!)
(def append! persi/append!)
(def insert! persi/insert!)
(def get-list persi/get-list)
(def get-map persi/get-map)
(def get-file-name persi/get-file-name)
(def get-dir-name persi/get-dir-name)
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
                   (persi/append! {:command :note-on
                                   :note note
                                   :velocity velocity
                                   :timestamp timestamp}))
                 on-key)
       (on-event off-event-key
                 (fn [{note :note
                      velocity :velocity
                      timestamp :timestamp}]
                   (persi/append! {:command :note-off
                                   :note note
                                   :velocity velocity
                                   :timestamp timestamp}))
                 off-key))))

(defn pause
  "Pause recording note on/off events to the midipe-events list"
  []
  (remove-handler [::midipe :midi :note-on])
  (remove-handler [::midipe :midi :note-off]))

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
(defn partition-by-timestamp
  "create a lazy seq of event seqs partitioned when timestamp goes
  over a threshold."
  [the-list]
  (partition-at-true
   #(> (:timestamp-̣Δ %) partition-threshold)
   (events-timestamp-Δ the-list)))
