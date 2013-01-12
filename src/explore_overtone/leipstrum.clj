(ns explore-overtone.leipstrum
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon)
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]))

;; create the instrument for play-note to strum
(def lead-guitar (oss/guitar))

(defmethod play-note :leader
  [{chord :pitch}]
  (oss/guitar-strum lead-guitar chord))

(def melody ;; loosely row-row-row-your-boat
  (->> (phrase [3/3 3/3 2/3 1/3 3/3]
               [ :C  :C  :F  :F  :F])
    (then
       (phrase [2/3 1/3 2/3 1/3 6/3]
               [ :C  :C  :G  :F  :G]))
    (then
       (phrase (repeat 12 1/3) 
               (mapcat (partial repeat 3)
                       [:C :F :C :G])))
    (then
       (phrase [2/3 1/3 2/3 1/3 6/3] 
               [ :G  :G  :G  :F  :C]))
    (where :part (is :leader))))

(defn strum-melody [speed]
  (->> melody
    (times 2)
    (where :time speed)
    play))

;; let's rock ========================================
;; http://tabs.ultimate-guitar.com/a/ac_dc/highway_to_hell_ver4_tab.htm
;; tuning may be off...tab says it is tuned 1/2 step down?

(def h2h-chords
  [[-1 -1 -1 -1 -1 -1] ; all off
   [-1  0  2  2  2 -1] ; 1
   [ 2 -1  0  2  3 -1] ; 2
   [ 3 -1  0  0  3 -1] ; 3
   [ 0  2  2 -1 -1 -1] ; 4
   [ 0 -2 -2 -1 -1 -1] ; 5
   ])

;; a little differently this time
(defmethod play-note :h2h-lead
  [{chord :pitch}]
  (oss/guitar-strum lead-guitar (h2h-chords chord)))
;;(oss/guitar-strum lead-guitar (h2h-chords 4))

;; e-||--------------|-------------|---------------------------------------||
;; B-||o-2--2--2-----|-3--3--3-----|-3--3--3----3--3--3----------2--2-----o||
;; G-||--2--2--2-----|-2--2--0-----|-2--2--0----2--2--0-----2----2--2------||
;; D-||o-2--2--2-----|-0--0--0-----|-0--0--0----0--0--0-----0----2--2-----o||
;; A-||--0--0--0-----|-------------|-----------------------------0--0------||
;; E-||--------------|(2)-2--3-----|-2--2--3----2--2--3-----2--------------||
;;    |< - - - common - - - - - - - - - - - - - - - - - ->|
(def h2h-common
  (->>
   (phrase [0.5 0.5 1.0 2.0]
           [ 1   1   1   0 ])
   (then
     (phrase [0.5 0.5 0.5 2.0]
             [ 2   2   3   0 ]))
   (then
    (->>
     (phrase [0.5 0.5 0.5 0.5]
             [ 2   2   3   0 ])
     (times 2)))))
(def h2h-intro
  (->>
   h2h-common
   (then
    (phrase [0.75 0.25 0.5 1.0 1.5]
            [ 2    0    1   1   0 ]))
   (where :part (is :h2h-lead))))

;; e-||--------------|-------------|------------------------------------||
;; B-||o-2--2--2-----|-3--3--3-----|-3--3--3----3--3--3----------------o||
;; G-||--2--2--2-----|-2--2--0-----|-2--2--0----2--2--0-----2-----------||
;; D-||o-2--2--2-----|-0--0--0-----|-0--0--0----0--0--0-----0----2-----o||
;; A-||--0--0--0-----|-------------|-----------------------------2------||
;; E-||--------------|(2)-2--3-----|-2--2--3----2--2--3-----2----0------||
;;    |< - - - common - - - - - - - - - - - - - - - - - ->|
(def h2h-pre-chorus
  (->>
   h2h-common
   (then
    (phrase [0.75 0.25 0.5 2.5]
            [ 2    0    1   4 ]))
   (where :part (is :h2h-lead))))

;; e-|-------------------------------------------------|
;; B-|-------------------------------------------------|
;; G-|-------------------------------------------------|
;; D-|-2--2--2--2--2--2--2--2--2--2--2----2-2-2--------|
;; A-|-2--2--2--2--2--2--2--2--2--2--2----2-2-2--------|
;; E-|-0--0--0--0--0--0--0--0--0--0--0----0-0-0--0-----|
;;                                        ?????
(def h2h-pre-chorus2
  (->>
   (->>
    (phrase [0.5] [4])
    (times 11))
   (then
    (phrase [0.5 0.5 0.5 2.5]
            [ 0   4   4   5 ]))
   (where :part (is :h2h-lead))))
   
(defn strum-h2h [speed]
  (->>
   (->> h2h-intro
        (times 2)) ;; should be 5 for the real song
   (then
    h2h-pre-chorus)
   (then
    h2h-pre-chorus2)
   ;; gives some time for clojure to compile?
   ;; otherwise first pair of notes are not well timed.
   (after 2) 
   (where :time speed)
   play))

(comment
  (strum-melody (bpm 100))
  ;; plug in the amp...
  (o/ctl lead-guitar
         :pre-amp 5.0 :distort 0.96
         :lp-freq 5000 :lp-rq 0.25
         :rvb-mix 0.5 :rvb-room 0.7 :rvb-damp 0.4)
  (strum-h2h (bpm 103))
)
