;; see discussion
;; https://groups.google.com/forum/?fromgroups#!topic/overtone/_fHoIslw3zI
;; wanted to save this for posterity.
;; ======================================================================
;; testcase with fixes at tst2 & 3.

;; first setup simple instrument...
(definst goo [freq 440 amp 0.5 decay 0.6 ]
  (* (env-gen (perc 0.01 decay) 1 1 0 1 FREE)
     (sin-osc freq) amp))
;; interesting...trying to pass an array to freq--it expects a pair?
(definst hoo [freqs [440 :kr] amp 0.5 decay 0.6 ]
  (* (env-gen (perc 0.01 decay) 1 1 0 1 FREE)
     (sin-osc freqs) amp))

(goo 440) ; see that it works

;; now try using at to schedule beats
(defn tst0 [ m beat ]
  (println "beat: " beat)
  (at (m (+ beat 1)) (goo 440))
  (at (m (+ beat 3)) (goo 400)))
;; this works...
(def metro (metronome 180))
(tst0 metro (metro))

;; now try something just a litle more complex...
(map goo [400 440]) ; "works"
;; again try using at to schedule beats
(defn tst1 [ m beat ]
  (println "beat: " beat)
  (at (m (+ beat 1)) (map goo [400 440])) ; ? "silent"
  (at (m (+ beat 3)) (goo 420)))
;; the first beat is missing -- why?
(tst1 metro (metro))

;; philip explains:
;;
;; map returns a lazy seq. At the repl, the seq is automatically
;; realized for you, but I'm guessing in the call to at, it is not.
;;
;; When traversing a seq for side-effects, try using doseq instead.

;; fixed with doseq. This works...
(doseq [i [400 440]] (goo i))
(defn tst2 [ m beat ]
  (println "beat: " beat)
  (at (m (+ beat 1)) (doseq [i [400 440]] (goo i)))
  (at (m (+ beat 3)) (goo 420)))
(tst2 metro (metro))

;; and doall, too...
(doall (map goo [400 440]))
(defn tst3 [ m beat ]
  (println "beat: " beat)
  (at (m (+ beat 1)) (doall (map goo [400 440])))
  (at (m (+ beat 3)) (goo 420)))
(tst3 metro (metro))


;; end testcase
;; ======================================================================
