;; ======================================================================
;; basic midi note-on/off input persistence
;;
;; Usage:
;;   (require '[explore-overtone.midipe :as mp])
;;   (mp/new)    ;; start recording to a new file
;;   (mp/pause)  ;; pause recording
;;   (mp/record) ;; restart recording
;;   (mp/save)   ;; save data to file
;;   (mp/open "my-saved-events.clj") ;; read events from saved file
;; ======================================================================
(ns explore-overtone.midipe
  (:use [overtone.music.time :only [now]]
        [overtone.libs.event :only [on-event remove-handler]]))

;; ======================================================================
;; state atoms
(def midipe-dirty (atom false))
(def midipe-filename (atom nil))
(def midipe-events (atom []))

;; ======================================================================
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

(defn- get-timestamp-filename
  ([] (str "midipe_"
       (.format (java.text.SimpleDateFormat. "yyMMdd_HHmmss") (now))
       ".clj")))

(defn- dirty! []
  (swap! midipe-dirty (fn [x] true)))

(defn dirty? []
  @midipe-dirty)

(defn- clean! []
  (swap! midipe-dirty (fn [x] false)))

(defn- add-event! [e]
  (swap! midipe-events conj e)
  (dirty!))

;; ======================================================================
;; public api fillows
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

(defn pause
  "Pause recording note on/off events to the midipe-events list"
  []
  (remove-handler [::midipe :midi :note-on])
  (remove-handler [::midipe :midi :note-off]))

(defn new
  "Start a new midi persistence file, losing all prior events.
Start monitoring events. Returns new filename."
  []
  (let [name (get-timestamp-filename)]
    (swap! midipe-filename (fn [x] name))
    (swap! midipe-events (fn [x] []))
    (clean!)
    (record)
    name))

(defn save
  "Save the current events to a file (if necessary).  Returns true if it saved the file."
  []
  (if (and (not (nil? @midipe-filename))
           (dirty?))
    (do
      (to-file (java.io.File. @midipe-filename) @midipe-events)
      (clean!)
      true)
    false))

(defn open
  "Open an existing file and read in the events"
  [filename]
  (swap! midipe-filename (fn [x] filename))
  (swap! midipe-events (fn [x] (from-file (java.io.File. filename))))
  (clean!)
  nil)

(defn events
  "Return the event list"
  []
  @midipe-events)

(comment
  (def first-ts (:timestamp (first @midipe-events)))
  (map #(/ (- (:timestamp %) first-ts) 1000000.0) @midipe-events)
  ;; look at notes
  (map #(:velocity %) (filter #(= (:command %) :note-on) @midipe-events))

  (dirty?)
  @midipe-dirty
  )
