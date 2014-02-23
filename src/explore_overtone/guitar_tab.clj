(ns explore-overtone.guitar-tab
  (:require [clojure.string :as cstr]))

;; Guitar Tab Converter

(defn- guitar-bar?
  "look for the character '|' at the beginning of the line
  with/without space before.  This denotes a horizontal 'bar' of
  guitar tab notation."
  [line]
  (not (nil? (re-seq #"^(\s+)?\|" line))))

(defn- add-to-last-bar
  "given a vector of vectors strings, add input line to the last
  vector in the vector.  The subvector contains a horizontal 'bar' of
  guitar tab notation."
  [bars line]
  (let [s (vec (butlast bars))
        l (last bars)]
    (conj s (conj l line))))

(defn- add-new-bar
  "given a vector of vectors strings, add input line to a new vector
   in the vectors.  The subvector contains a horizontal 'bar' of
   guitar tab notation."
  [bars line]
  (conj bars (vector line)))

(defn- lines-to-bars
  "Transform the lines in a text file via a state machine.  Lines are
   gathered into a vector of vectors where each sub-vector contains
   the lines related to one horizontal 'bar' of guitar tab notation.
   The last string of the tab notation will be the bottm line in the
   'bar'."
  [lines]
  (loop [state :start bars [] lines lines]
    (if (empty? lines)
      ;; return the bars when we're done
      bars
      ;; otherwise, use the state machine to construct the bars
      (let [[next-state next-bars next-lines]
            (case state
              ;; start is just before you are 'gathering' a bar
              :start     [:gathering
                          (add-new-bar bars (first lines))
                          (rest lines)]
              ;; you've started to gather a bar, just get all of it
              :gathered  (if (cstr/blank? (first lines))
                           [:start bars lines]
                           [:gathered
                            (add-to-last-bar bars (first lines))
                            (rest lines)])
              ;; you're gathering lines until you see a guitar bar
              :gathering (if (guitar-bar? (first lines))
                           [:gathered
                            (add-to-last-bar bars (first lines))
                            (rest lines)]
                           [:gathering
                            (add-to-last-bar bars (first lines))
                            (rest lines)]))]
        (recur next-state next-bars next-lines)))))

(defn- bar-to-columns
  "given a horizontal bar of guitar tab where the last string of the
   guitar noation is the bottom line of the bar, turn it 90 degrees to
   make it more easily parsable by the computer."
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
  "given a vector of vectors of horizontal bars of guitar tab, turn it
   90 degrees to make it more easily parsable."
  [bars]
  (let [columns (flatten (map bar-to-columns bars))]
    (map #(apply str (reverse %)) columns)))

(defn tab-seq
  "read in the file, turn it 90 degrees and stream it out so the tab
   sequences are easily parsable.  The top string of a guitar should
   be the first character of the line."
  [filename]
  (-> (slurp filename)
      cstr/split-lines
      lines-to-bars
      bars-to-columns))

(defn prettify-line
  "pretty up the guitar tab by filtering only the first num-chars and
   turning common characters 90 degrees"
  [num-chars line]
  (-> (apply str (take num-chars line))
      (cstr/replace #"[\|\-]"
                    {"|" "-", "-" "|"})))

;; ======================================================================
(comment
  ;; usage

  ;; very basic printout -- all the text
  (doseq [line (tab-seq "tab.txt")]
    (println line))
  ;; ||||||      GG TAW     ####
  ;; ------  4 I tt rro     -sT-
  ;; ------HG/Qn rr arr     -oh-
  ;; ------ t4=t    nad     -ni-
  ;; ------ r 1r II sns     -gs-
  ;; ------   1o I  cg      -. -
  ;; ------ I 0   ( rea     - f-
  ;; ------E     (E idn     -Yi-
  ;; ------      E  b d     -ol-
  ;; -2----E      A eb      -ue-
  ;; ------      A  dyM     -  -
  ;; -2----E      D   u     -mi-

  ;; pretty up the tab and filter out non-tab lines
  (defn println-non-blank
    [x]
    (when (not (cstr/blank? x))
      (println x)))
  (doseq [line (tab-seq "tab.txt")]
    (->> line
         (prettify-line 7)
         println-non-blank))
  ;; ------
  ;; ||||||
  ;; ||||||H
  ;; ||||||
  ;; ||||||
  ;; ||||||
  ;; ||||||
  ;; ||||||E
  ;; ||||||
  ;; |2||||E
  ;; ||||||
  ;; |2||||E

  ;; convert tabs to vectors of fret numbers
  (defn select-strum-lines
    "assumes tab has strum length above 6th string"
    [x]
    (if (= \  (last x)) " " x))
  (defn fret-num
    "convert characters to numbers"
    [c]
    (case c
      \0 0, \1 1, \2 2, \3 3, \4 4,
      \5 5, \6 6, \7 7, \8 8, \9 9,
      \| -1,
      -2))
  (defn strum-vec
    [x]
    (if (cstr/blank? x)
      []
      (vec (map fret-num (butlast x)))))
  (defn println-strums
    [x]
    (when (> (count x) 0)
      (println x)))
  (doseq [line (tab-seq "tab.txt")]
    (->> line
         (prettify-line 7)
         select-strum-lines
         strum-vec
         println-strums))
  ;; [-1 -1 -1 -1 -1 -1]
  ;; [-1 -1 -1 -1 -1 -1]
  ;; [-1 2 -1 -1 -1 -1]
  ;; [-1 2 -1 -1 -1 -1]
  ;; [-1 2 -1 -1 -1 -1]
  ;; [-1 -1 1 -1 -1 -1]
  ;; [-1 -1 1 -1 -1 -1]
  ;; [-1 2 -1 -1 -1 -1]
  ;; [3 -1 -1 -1 -1 -1]
  ;; [0 -1 -1 -1 -1 -1]
  ;; [-1 2 2 1 0 0]
  ;; [-1 2 2 1 0 0]

)
