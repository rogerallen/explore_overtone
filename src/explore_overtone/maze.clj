;; Use the Clojure Programming Chapter 3 maze example to create a melody.
;; The idea behind this is that a trip through a maze is both random &
;; has some structure to it with a start & end point you choose.  This
;; program uses that combination to create a melody.
;;
;; -- Roger Allen, May 2012

;; mda piano requires external server
(use 'overtone.live)
(use 'overtone.inst.piano)

;; ======================================================================
;; original code for the maze from Clojure Programming by Emerick,
;; Carper & Grand.  Updated per:
;; https://github.com/clojurebook/ClojureProgramming/issues/7

(require '[clojure.zip :as z])

(defn maze [walls]
  "Returns a random maze carved out of walls; walls is a set of
   2-item sets #{a b} where a and b are locations.
   The returned maze is a set of the remaining walls."
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]}))
                {} (map seq walls))
        start-loc (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) start-loc)]
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
        (let [walk (iterate (comp rand-nth paths) loc)
              steps (zipmap (take-while unvisited walk) (next walk))]
          (recur (reduce disj walls (map set steps))
            (reduce disj unvisited (keys steps))))
        walls))))

(defn grid [w h]
  "Returns a fully connected grid for use is creating a maze"
  (set (concat
         (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})
         (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))

(defn ariadne-zip [labyrinth loc]
  "A zip to traverse the maze and solve it"
  (let [paths (reduce (fn [index [a b]] 
                        (merge-with into index {a [b] b [a]}))
                {} (map seq labyrinth))
        children (fn [[from to]] 
                   (seq (for [loc (paths to) 
                              :when (not= loc from)] 
                          [to loc])))]
    (z/zipper (constantly true)
              children
              nil
              [nil loc])))

(defn draw [w h maze path]
  "Draw the maze and the solution path"
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane 
     (doto (proxy [javax.swing.JPanel] []
             (paintComponent [^java.awt.Graphics g]
               (let [g (doto ^java.awt.Graphics2D (.create g)
                         (.scale 10 10)
                         (.translate 1.5 1.5)
                         (.setStroke (java.awt.BasicStroke. 0.4)))]
                 (.drawRect g -1 -1 w h)
                 (doseq [[[xa ya] [xb yb]] (map sort maze)]
                   (let [[xc yc] (if (= xa xb) 
                                   [(dec xa) ya]
                                   [xa (dec ya)])]
                     (.drawLine g xa ya xc yc)))
                 (.translate g -0.5 -0.5)
                 (.setColor g java.awt.Color/RED)
                 (doseq [[[xa ya] [xb yb]] path]
                   (.drawLine g xa ya xb yb)))))
       (.setPreferredSize (java.awt.Dimension. 
                      	     (* 10 (inc w)) (* 10 (inc h))))))
   .pack
   (.setVisible true)))

;; with create-maze-path result...
;; (map first x) gives [x y] sequence through the maze (except final point)
;; (map #(first (first %)) x) gives the x values of the sequence
(defn create-maze-path [w h start-end-column]
  "return a path from the upper-left corner to the lower-right corner of a maze"
  (let [grid (grid w h)
        walls (maze grid)
        labyrinth (reduce disj grid walls)
        places (distinct (apply concat labyrinth))
        theseus [start-end-column 0] ; travel from top to bottom...
        minotaur [start-end-column (- h 1)]
        full-path #(conj (z/path %) (z/node %))
        path (->> theseus
                  (ariadne-zip labyrinth) 
                  (iterate z/next)
                  (filter #(= minotaur (second (z/node %))))
                  first full-path rest)]
    (draw w h walls path) ;; just so you can see if it might be interesting...
    path))

;; ======================================================================
;; now use the Clojure Programming code...
;; "2" variants of the routines use y value for changing scale

(defn run-length-encode [x]
  "given a sequence x = (0 1 1 2 ...), encode each item as a tuple [value run-length]"
  (reduce (fn [lst v]
            (let [ lstlst (peek lst) ] ; last item in the list is most recent
              (if (= (first lstlst) v)
                ;; if same, increment count
                (conj (pop lst) [v (inc (second lstlst))])
                ;; if different append a count of 1
                (conj lst [v 1])))) 
          '[ [0 0] ] x )) ;; starting with initial vector (seq will be in reverse)

(defn run-length-encode2 [x]
  "like rle above, but each item is a vector [x y] encode each item as a tuple [value run-length start-y]"
  (reduce (fn [lst v]
            (let [ lstlst (peek lst) ] ; last item in the list is most recent
              (if (= (first lstlst) (first v))
                ;; if same, increment count
                (conj (pop lst) [(first lstlst)
                                 (inc (second lstlst))
                                 (nth lstlst 2)])
                (conj lst [(first v)
                           1
                           (second v)])))) 
          '[ [0 0 0] ] x )) ;; starting with initial vector (seq will be in reverse)

(defn remove-threshold [x t]
  "remove any node from a run-length-encoded sequence that is <= threshold"
  (reduce (fn [lst v]
            (if (> (second v) t)
              (conj lst [(first v) (- (second v) t)])
              lst))
          '[ ] x))

(defn remove-threshold2 [x t]
  "remove any node from a run-length-encoded sequence that is <= threshold"
  (reduce (fn [lst v]
            (if (> (second v) t)
              (conj lst [(first v)
                         (- (second v) t)
                         (nth v 2)])
              lst))
          '[ ] x))

(defn digits2inotes [rle-seq]
  "given a list of digits, make it into a list of index notes"
  (map #(hash-map :pitch-index (first %1)
                  :velocity-index 0 ;; FIXME???
                  :duration-index (mod (second %1) 10))
       rle-seq))

(defn digits2inotes2 [rle-seq]
  "given a list of digits, make it into a list of index notes + scale"
  (map #(hash-map :pitch-index (first %1)
                  :velocity-index 0 ;; FIXME???
                  :duration-index (mod (second %1) 10)
                  :scale-index (nth %1 2))
       rle-seq))

(defn index2pitch [tonic type index]
  "given a digit in range 0..9 find index in scale defined by
     tonic & type.  E.g. (index2pitch :c4 :major 1) -> 62"
  (nth (vec (scale tonic type (range 1 10))) (mod index 10)))

(defn index2velocity [index]
  "given a digit 'n' in range 0..9, find a velocity to play"
  (+ 80 (* 3 index)))

(defn index2duration [index]
  "given a digit 'n' in range 0..9, find a length in beats"
  ;; (near) 1/f histogram of length
  ;;  0     1     2    3    4    5    6    7    8    9
  ([ 0.50 0.50 0.75 0.75 1.00 1.00 1.25 1.50 2.00 4.00] index))

(defn inote2snote [tonic type cur-inote]
  "given an index-note, create a sequence-note with a place for a beat."
  (hash-map
   :pitch (index2pitch tonic type (:pitch-index cur-inote))
   :velocity (index2velocity (:velocity-index cur-inote))
   :duration (index2duration (:duration-index cur-inote))
   :beat 0))

(defn duration2beat [cur-snote nxt-snote]
  "given 2 sequence notes, update the nxt beat"
  (hash-map
   :pitch (:pitch nxt-snote)
   :velocity (:velocity nxt-snote)
   :duration (:duration nxt-snote)
   :beat (+ (:duration cur-snote) (:beat cur-snote))))

(defn calc-seq [tonic type rle-notes]
  "calc some seq-notes in a certain key. doall to remove laziness. returns a list of
   (pitch velocity duration curbeat) values"
  (doall (reductions duration2beat
                     (map #(inote2snote tonic type %) rle-notes))))

(defn calc-seq2 [tonic-type-vec rle-notes max-len]
  "calc some seq-notes in a certain key. doall to remove laziness. returns a list of
   (pitch velocity duration curbeat) values"
  (doall (reductions duration2beat
                     (map #(let [steps-per-scale (/ max-len (count tonic-type-vec))
                                 index (int (/ (:scale-index %) steps-per-scale))
                                 tonic-type (nth tonic-type-vec index)
                                 tonic (first tonic-type)
                                 type (second tonic-type)]
                             (inote2snote tonic type %)) rle-notes))))

(defn play-seq [m beat snote-seq]
  "play a list of (pitch velocity duration curbeat) where snote-seq is offset by beat"
  (doseq [cur-snote snote-seq]
    (let [cur-pitch (:pitch cur-snote)
          cur-vel (:velocity cur-snote)
          cur-dur (:duration cur-snote)
          cur-beat (+ beat (:beat cur-snote))]
      ;;(println "play:" (+ beat cur-beat) cur-pitch cur-vel)
      (at (m cur-beat) (piano cur-pitch 1 cur-vel)))))

;; ======================================================================
;; generate & play a melody based on one scale
(defn gen-play-melody [tonic type max-len]
  "bring everything together and play a melody"
  ;; get a path through the maze
  (def x (create-maze-path 10 max-len 5))
  ;; encode the x values from the maze path into a [note duration] sequence
  (def x-rle (run-length-encode
              (map #(first (first %))
                   ;; all to get a sequence that ends at the minotaur for sure
                   (seq (conj (vec x) [(second (last x)) (first (last x))])))))
  ;; remove the notes that only play for 1 beat. Otherwise, this turns into
  ;; just scale exercises (which might be interesting, too)
  (def x-rle2 (remove-threshold x-rle 1))
  (println "(def x-rle2 "x-rle2")" )
  ;; make the notes into a canonical form, calculate & play the sequence
  (play-seq (metronome 72) 0 (calc-seq tonic type (digits2inotes x-rle2))))

;; (gen-play-melody :a3 :pentatonic 64)  << run this to display & play the maze
;; (gen-play-melody :c3 :minor 64)  << run this to display & play the maze
;; (gen-play-melody :d3 :major 64)  << run this to display & play the maze

;; ======================================================================
;; generate & play a melody based on a vector of scales
;; add chord progression via Y coordinate.
(defn gen-play-melody2 [tonic-type-vec max-len]
  "bring everything together and play a melody with scale progressions"
  ;; get a path through the maze
  (def x (create-maze-path 10 max-len 5))
  ;; encode the x values from the maze path into a [note duration] sequence
  (def x-rle (run-length-encode2
              (map #(first %)
                   (seq (conj (vec x) [(second (last x)) (first (last x))])))))
  ;; remove the notes that only play for 1 beat. Otherwise, this turns into
  ;; just scale exercises (which might be interesting, too)
  (def x-rle2 (remove-threshold2 x-rle 1))
  (println "(def x-rle2 "x-rle2")" )
  ;; make the notes into a canonical form, calculate & play the sequence
  (play-seq (metronome 72) 0 (calc-seq2 tonic-type-vec (digits2inotes2 x-rle2) max-len)))

(comment
  ;; adjust & play...
  (gen-play-melody2 [[:c3 :pentatonic]
                     [:g4 :pentatonic]
                     [:a3 :pentatonic]
                     [:f4 :pentatonic]
                     [:c3 :pentatonic]
                     ]
                    80)
  )
