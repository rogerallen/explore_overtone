(ns explore-overtone.folsom-prison
  (:use leipzig.melody
        leipzig.live
        leipzig.scale
        leipzig.canon)
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]))

;; create the instrument for play-note to strum
(def lead-guitar (oss/guitar :pre-amp 50))
(def solo-guitar (oss/guitar :pre-amp 5
                             :distort 0.5
                             :lp-freq 5000 :lp-rq 0.25
                             :rvb-mix 0.5 :rvb-room 0.7 :rvb-damp 0.4))
;; -1 silences, -2 lets it ring
(def folsom-chords
  {:rest  [[-1 -1 -1 -1 -1 -1] :down 0.7]
   :s1f0  [[ 0 -1 -2 -2 -2 -2] :down 0.7]
   :s1f2  [[ 2 -1 -2 -2 -2 -2] :down 0.7]
   :s1f3  [[ 3 -1 -2 -2 -2 -2] :down 0.7]
   :s2f0  [[-1  0 -2 -2 -2 -2] :down 0.7]
   :s2f2  [[-1  2 -2 -2 -2 -2] :down 0.7]
   :s3f1  [[-1 -1  1 -2 -2 -2] :down 0.7]
   :Edn   [[-2  2  2  1  0  0] :down 0.45]
   :Eup   [[-2  2  2  1  0  0] :up   0.45]
   :Adn   [[-2 -2  2  2  2  0] :down 0.45]
   :Aup   [[-2 -2  2  2  2  0] :up   0.45]
   :B7dn  [[-2 -2  1  2  0  2] :down 0.45]
   :B7up  [[-2 -2  1  2  0  2] :up   0.45]
   ;; solo
   :S4F9  [[-1 -1 -1  9 -1 -1] :up   1.0]
   :S5F8  [[-1 -1 -1 -1  8 -1] :up   1.0]
   :S5F9  [[-1 -1 -1 -1  9 -1] :up   1.0]
   :S6F7  [[-1 -1 -1 -1 -1  7] :up   1.0]
   :S6F10 [[-1 -1 -1 -1 -1 10] :up   1.0]
   :S5F0  [[-1  0 -1 -1 -1 10] :up   1.0]
   :S34A  [[-1 -1  2  2 -1 -1] :down 1.0]
   :S56A  [[-1 -1 -1 -1  2  5] :down 1.0]
   })

(defmethod play-note :leader
  [{pitch :pitch duration :duration}]
  (let [[chord strum-dir strum-amp] (folsom-chords pitch)
        strum-dur 0.03]
    ;;(println "pn" chord strum-dir strum-amp)
    (o/ctl lead-guitar :amp strum-amp)
    (oss/guitar-strum lead-guitar chord strum-dir strum-dur)))

(defmethod play-note :solo
  [{pitch :pitch duration :duration}]
  (let [[chord strum-dir strum-amp] (folsom-chords pitch)
        strum-dur 0.03]
    ;;(println "pn" chord strum-dir strum-amp)
    (o/ctl solo-guitar :amp strum-amp)
    (oss/guitar-strum solo-guitar chord strum-dir strum-dur)))

(defn phrase-e
  [n]
  (->> (phrase [1/2   1/4  1/4  1/2   1/4  1/4 ]
               [:s1f0 :Edn :Eup :s2f2 :Edn :Eup])
       (times n)))

(defn phrase-a
  [n]
  (->> (phrase [1/2   1/4  1/4  1/2   1/4  1/4 ]
               [:s2f0 :Adn :Aup :s1f0 :Adn :Aup])
       (times n)))

(defn phrase-b7
  [n]
  (->> (phrase [1/2   1/4   1/4   1/2   1/4  1/4 ]
               [:s2f2 :B7dn :B7up :s1f2 :B7dn :B7up])
       (times n)))

(def melody
  (->> (phrase [2.5   1/2   1/2   1/2   1     1     1     1     ]
               [:rest :s2f2 :s2f2 :s2f2 :s3f1 :s3f1 :s2f2 :s1f3 ])
       (then (phrase-e 2))
       ;; start of repeat
       (then (phrase-e 3))
       (then (phrase-a 2))
       (then (phrase-e 2))
       (then (phrase-b7 2))
       ;; 1.
       (then (phrase-e 2))
       ;; first repeat
       (then (phrase-e 3))
       (then (phrase-a 2))
       (then (phrase-e 2))
       (then (phrase-b7 2))
       ;; 2. here is where the 2nd guitar should show up
       (then (phrase-e 2))
       (then (phrase-e 3))
       (then (phrase-a 2))
       (then (phrase-e 2))
       (then (phrase-b7 2))
       (then (phrase-e 2))
       ;; back to main theme
       (then (phrase-e 3))
       (then (phrase-a 2))
       (then (phrase-e 2))
       (then (phrase-b7 2))
       (then (phrase-e 2))
       ;; done
       (where :part (is :leader))))

(def solo
  (->> (phrase [(* 2 (+ 2 2 3 2 2 2 2 3 2 2 2))]
               [:rest])
       (then (phrase [3.5   1/2   1/2   1/2   1/2    1/4   3/4    1     1/2]
                     [:rest :s5f8 :s5f9 :s6f7 :s6f10 :s6f7 :s6f10 :s6f7 :s5f8]))
       ;; done
       (where :part (is :solo))))

(defn strum-melody [speed]
  (->> melody
       (with solo)
       (where :time speed)
       play))

;; ======================================================================
(def melodytest
  (->> (phrase-e 10)
       (
       (where :part (is :leader))))

(def solotest
  (->> (phrase [3.5   1/2   1/2   1/2   1/2    1/4   3/4    1     1/2]
               [:rest :S5F8 :S5F9 :S6F7 :S6F10 :S6F7 :S6F10 :S6F7 :S5F8])
       (then
        (phrase [1/2   1/2   1/2    1/4   1/4    1/2    1      1/2  ]
                [:S5F9 :S6F7 :S6F10 :S6F7 :S6F10 :S6F10 :S6F10 :S5F8]))
       (then
        (phrase [1/2   1/2   1/2    1/4   3/4    1     1/2  ]
                [:S5F9 :S6F7 :S6F10 :S6F7 :S6F10 :S6F7 :S5F8]))
       (then
        (phrase [1/2   1/2   1/2    1/4   1/4    1/2    1/2   1/2   1/2  ]
                [:S5F9 :S6F7 :S6F10 :S6F7 :S6F10 :S6F10 :S6F7 :S5F9 :S4F9]))
       (where :part (is :solo))))

(defn strumtest [speed]
  (->> melodytest
       (with solotest)
       (where :time speed)
       play))

;; play a few chords...
(comment
  (strumtest (bpm 110))
  (strum-melody (bpm 110))
  (stop)

  (o/ctl lead-guitar
         :pre-amp 20.0 :distort 0.0
         :lp-freq 7000 :lp-rq 0.8
         :rvb-mix 0.2 :rvb-room 0.3 :rvb-damp 0.2)
  (oss/guitar-strum lead-guitar :A)

  (o/ctl solo-guitar
         :pre-amp 60.0 :distort 0.3
         :lp-freq 8000 :lp-rq 0.2
         :rvb-mix 0.5 :rvb-room 0.7 :rvb-damp 0.4)
  (oss/guitar-strum solo-guitar :A)
)
