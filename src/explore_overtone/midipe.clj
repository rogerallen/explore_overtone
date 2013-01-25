(ns explore-overtone.midipe
  (:use [overtone.music.time :only [now]]
        [overtone.libs.event :only [on-event remove-handler]]))

;; basic midi input persistence

(defn- to-file
  "Save a clojure form to a file"
  [#^java.io.File file form]
  (with-open [w (java.io.FileWriter. file)]
    (print-dup form w)))

(defn- from-file
  "Load a clojure form from file."
  [#^java.io.File file]
  (with-open [r (java.io.PushbackReader. (java.io.FileReader. file))]
    (read r)))

(defn get-filename
  ([x] (get-filename)) ;; ignore the arg
  ([] (str "midipe_"
       (.format (java.text.SimpleDateFormat. "yyMMdd_HHmmss") (now))
       ".cljp")))

(def midipe-dirty (atom false))
(defn dirty! []
  (swap! midipe-dirty (fn [x] true)))
(defn clean! []
  (swap! midipe-dirty (fn [x] false)))

(def midipe-filename (atom (get-filename)))

(def midipe-events (atom []))

(defn new-midipe []
  ;;(save-midipe)
  (swap! midipe-filename get-filename)
  (swap! midipe-events (fn [x] []))
  (clean!))

(defn add-event! [e]
  (swap! midipe-events conj e)
  (dirty!))

(defn save []
  (to-file (java.io.File. @midipe-filename) @midipe-events)
  (clean!))

(defn open [filename]
  (swap! midipe-filename (fn [x] filename))
  (swap! midipe-events (fn [x] (from-file (java.io.File. filename))))
  (clean!))

(defn record-events
  ([] (record-events [:midi] ::midipe))
  ([device-key player-key]
     (let [on-event-key  (concat device-key [:note-on])
           off-event-key (concat device-key [:note-off])
           on-key        (concat [::midipe] on-event-key)
           off-key       (concat [::midipe] off-event-key)]
       (on-event on-event-key
                 (fn [{note :note
                      velocity :velocity
                      timestamp :timestamp}]
                   (add-event! {:command :note-on
                                :note note
                                :velocity velocity
                                :timestamp timestamp}))
                 on-key)
       (on-event off-event-key
                 (fn [{note :note
                      velocity :velocity
                      timestamp :timestamp}]
                   (add-event! {:command :note-off
                                :note note
                                :velocity velocity
                                :timestamp timestamp}))
                 off-key))))

(defn stop-events
  []
  (remove-handler [::midipe :midi :note-on])
  (remove-handler [::midipe :midi :note-off]))

(comment
  (new-midipe)
  ;; play some notes...
  (count @midipe-events) ;; count them
  (save) ;; save theml
  (open "midipe_130124_224254.cljp") ;; restore them
  ;; convert timestamps to seconds since beginning of sequence
  (def first-ts (:timestamp (first @midipe-events)))
  (map #(/ (- (:timestamp %) first-ts) 1000000.0) @midipe-events)
  ;; look at notes
  (map #(:velocity %) (filter #(= (:command %) :note-on) @midipe-events))

  )
