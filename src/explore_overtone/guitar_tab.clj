(ns explore-overtone.guitar-tab
  (:require [clojure.string :as cstr]))

(defn- has-guitar-string-section?
  "look for | at the beginning of the line with/without space before.  This denotes
  a horizontal 'bar' of guitar tab notation."
  [line]
  (not (nil? (re-seq #"^(\s+)?\|" line))))

(defn- add-to-last-bar
  "given a vector of vectors strings, add input line to the last vector in the vector.
  The subvector contains a horizontal 'bar' of guitar tab notation."
  [bars line]
  (let [s (vec (butlast bars))
        l (last bars)]
    (conj s (conj l line))))

(defn- add-new-bar
  "given a vector of vectors strings, add input line to a new vector in the vectors.
  The subvector contains a horizontal 'bar' of guitar tab notation."
  [bars line]
  (conj bars (vector line)))

(defn- lines-to-bars
  "Transform the lines in a text file into a vector of vectors where
  each sub-vector contains the lines related to one horizontal 'bar' of guitar tab
  notation."
  [lines]
  (loop [state :start
         bars  []
         lines lines]
    (if (empty? lines)
      ;; return the bars when we're done
      bars
      ;; otherwise, use the state machine to construct the bars
      (let [[next-state next-bars next-lines]
            (case state
              :start     [:gathering (add-new-bar bars (first lines)) (rest lines)]
              :gathered  (if (str/blank? (first lines))
                           [:start bars lines]
                           [:gathered  (add-to-last-bar bars (first lines)) (rest lines)])
              :gathering (if (has-guitar-string-section? (first lines))
                           [:gathered  (add-to-last-bar bars (first lines)) (rest lines)]
                           [:gathering (add-to-last-bar bars (first lines)) (rest lines)]))]
        (recur next-state next-bars next-lines)))))

(defn- bar-to-columns
  "given a horizontal bar of guitar tab, turn it 90 degrees to make it more easily
   parsable."
  [bar]
  (let [max-count (apply max (map count bar))
        padded-bar (map #(let [len (- max-count (count %))]
                           (if (> len 0)
                           (str % (apply str (repeat len " ")))
                           %))
                        bar)
        ;_  (doall (map println padded-bar))
        columns (apply map str padded-bar)
        ]
    columns))

(defn- bars-to-columns
  "given a vector of vectors of horizontal bars of guitar tab, turn it 90
   degrees to make it more easily parsable."
  [bars]
  (let [columns (flatten (map bar-to-columns bars))]
    (map #(apply str (reverse %)) columns)))

(defn tab-seq
  "read in the file, turn it 90 degrees and stream it out so the tab sequences
   are "
  [filename]
  (-> (slurp filename)
      cstr/split-lines
      lines-to-bars
      bars-to-columns))

;; ======================================================================
(comment usage:

  ;; note println seems to strip out leading whitespace ?
(defn fret-num
  [c]
  (case c
      \0 0
      \1 1
      \2 2
      \3 3
      \4 4
      \5 5
      \6 6
      \7 7
      \8 8
      \9 9
     \- -1
     -2))

  (println "W----------------------")
  (doseq [line (tab-seq "/Users/rallen/Documents/Devel/Overtone/rallen/explore_overtone/snip_tab.txt")]
    (let [strings (take 6 line)
          guitar-vec (vec (map fret-num strings))]
      (if (and (not= guitar-vec '(-1 -1 -1 -1 -1 -1))
              (not= guitar-vec '(-2 -2 -2 -2 -2 -2)))
        (println guitar-vec ))))

)
