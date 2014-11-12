(ns explore-overtone.guitar-tab-example
  (:require [clojure.string :as cstr]
            [explore-overtone.guitar-tab :as gt]))

;; convert tabs to vectors of fret numbers
(defn select-strum-lines
  "assumes tab has strum length above 6th string"
  [x]
  (if (= \  (last x)) " " x))
(defn to-fret-num
  "convert characters to numbers"
  [c]
  (case c
    \0 0, \1 1, \2 2, \3 3, \4 4,
    \5 5, \6 6, \7 7, \8 8, \9 9,
    \| -1,
    -2))
(defn to-note-length
  [c]
  (case c
    \W :whole \H :half \E :eighth \Q :quarter \S :sixteenth \T :thirtysecond
    :???))
(defn strum-vec
  [x]
  (if (cstr/blank? x)
    []
    [ (vec (map to-fret-num (butlast x)))
      (to-note-length (last x)) ]))
(defn println-strums
  [x]
  (when (> (count x) 0)
    (println x)))
(doseq [line (gt/tab-seq "folsom_prison_tab.txt")]
  (->> line
       (gt/prettify-line 7)
       select-strum-lines
       strum-vec
       println-strums))
