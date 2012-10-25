;; best explanation:
;; http://danielnouri.org/docs/SuperColliderHelp/Tutorials/Getting-Started/Busses.html
;;
(defsynth test1 [freq    {:default 440}
                 out-bus {:default 0 :rate :ar}]
  (out out-bus (saw freq)))

(defsynth test2 [freq    {:default 500}
                 out-bus {:default 0 :rate :ar}]
  (out out-bus (saw freq)))

(defsynth mixem [in-bus {:default 16 :rate :ar}
                 amp    {:default 1.0}]
  (out 0 (pan2 (in in-bus 1) 0 amp)))

(def test-bus (audio-bus))
(def t1 (test1 :position :head 400 test-bus))
(def t2 (test2 :position :after :target t1 500 test-bus))
(def m0 (mixem :position :tail test-bus 0.5))
(ctl t2 :freq 800)
(ctl m0 :amp 0.1)
(stop)

(definst mixim [in-bus {:default 16 :rate :ar}
                 amp   {:default 1.0}]
  (* (in in-bus 1) amp))

(def test-bus (audio-bus))
(def t1 (test1 400 test-bus))

(def t2 (test2 500 test-bus))
(def m1 (mixim test-bus 0.2))  ;; does not work.
(ctl t2 :freq 800)
(ctl m1 :amp 0.8)
(stop)
