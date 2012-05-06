(ns explore_overtone.piano)
;; noodling on the built in piano instrument
;; ----------------------------------------------------------------------
(do
  (use 'overtone.core)
  (connect-external-server 57110))

;; simple stuff
;;(use 'overtone.inst.piano)
;;(piano (note :E3))
;;(piano (map note [:E3 :G3]))

;; ----------------------------------------------------------------------
;; some infinite blues...with a bit of randomness thrown in.
(do
   (use 'overtone.inst.piano)
   (defn piano-note [n]
     (piano (note n)))
   (defn strum [ c ]
     (doall (map piano-note (apply chord c))))
   (defn strumming [ m beat ]
     (let [gchords [ (choose '([:e3 :major] [:e3 :major7] [:e3 :minor]))
                     (choose '([:e3 :major] [:e3 :major7] [:e3 :minor]))
                     (choose '([:g3 :major] [:g3 :major7] [:g3 :minor]))
                     (choose '([:e3 :major] [:e3 :major7] [:e3 :minor]))
                     (choose '([:a3 :major] [:a3 :major7] [:a3 :minor]))
                     (choose '([:e3 :major] [:e3 :major7] [:e3 :minor]))
                     ]
           next-measure-beat (+ beat (* 2 (.size gchords)))
           ]
       (println "beat: " beat " " gchords)
       (doseq [[index cur-chord]
               (map vector (iterate inc 0) gchords)]
         (at (m (+ beat (* 2 index))) (strum cur-chord)))
       (apply-at (m next-measure-beat) #'strumming m next-measure-beat [])))
   )

;;(def metro (metronome 120))
;;(strumming metro (metro))
;;(stop) ; when you are sick of it...
