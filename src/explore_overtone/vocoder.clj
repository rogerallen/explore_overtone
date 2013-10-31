(ns explore-overtone.vocoder
  (:use [overtone.live]
        [explore-overtone.sawbble]
        [overtone.synth.stringed]))

;; http://www.musicfromouterspace.com/index.php?CATPARTNO=&PROJARG=VOCODER2013%2FVOCODER2013.php&MAINTAB=SYNTHDIY&SONGID=NONE&VPW=1260&VPH=653
;; 3330  2546  2001  1495  1013  720   542   395   285   208   154   101
;; (map #(Math/pow 10 (/ % 20)) [-2.13 -2.8 -3.15 -3.4 -2.8 -2.8 -3.12 -2.5 -3 -2.4 -2.4 -2.55])
;; 0.782 0.724 0.695 0.676 0.724 0.724 0.698 0.749 0.707 0.758 0.758 0.745
;; 0.88  4.48  5.16  3.2   3.9   3.7   3.75  3.7   3.55  4     2.96  0.55

(defsynth vocoder [ins-bus     0
                   ctl-lvl     1.0
                   ctl-mix-lvl 0.1
                   ins-lvl     1.0
                   ins-mix-lvl 0.1
                   ]
  (let [ctl-sig (* ctl-lvl (sound-in))
        ctl-mix (* ctl-mix-lvl ctl-sig)
        ;;ins-sig (* ins-lvl [(lf-saw 500) (lf-saw 500)])
        ;;ins-sig (* ins-lvl [(pink-noise) (pink-noise)])
        ins-sig (* ins-lvl (in ins-bus 2))
        ins-mix (* ins-mix-lvl ins-sig)
        h-sig   (* 0.782
                   (peak-follower:ar (rhpf ctl-sig 3330 (/ 0.88)) 0.99)
                   (rhpf ins-sig 3330 (/ 0.88)))
        l-sig   (* 0.745
                   (peak-follower:ar (rlpf ctl-sig  101 (/ 0.55)) 0.99)
                   (rlpf ins-sig  101 (/ 0.55)))
        b-sigs  (map #(* %2
                         (peak-follower:ar (bpf ctl-sig %1 (/ %3)) 0.99)
                         (bpf ins-sig %1 (/ %3)))
                     [2546  2001  1495  1013  720   542   395   285   208   154  ]
                     [0.724 0.695 0.676 0.724 0.724 0.698 0.749 0.707 0.758 0.758]
                     [4.48  5.16  3.2   3.9   3.7   3.75  3.7   3.55  4     2.96 ])
        sigs    (apply vector (concat [ctl-mix ins-mix h-sig l-sig] b-sigs))]
    (out 0 (mix sigs))))

(def b0 (audio-bus 2))

;; pick one
(def syn (sawbble-synth :out-bus b0))
(def gtr (guitar :out-bus b0))

(def v (vocoder b0 25.0 0.1 8.0 0.01))
(ctl v :ctl-lvl 30.0)
(ctl v :ctl-mix-lvl 0.001)
(ctl v :ins-lvl 15.0)
(ctl v :ins-mix-lvl 0.001)

;; play input
(guitar-strum gtr :E)
(guitar-strum gtr :A)
(guitar-strum gtr :Cm)

(recording-start "~/Desktop/vocoder_fun2.wav")
(recording-stop)

(stop)

;; this mixes in stereo
(defsynth x []
  (out 0 (* 25.0 (lpf (sound-in) 300)
            [(* 0.1 (lf-tri 500))
             (* 0.1 (lf-tri 500))])))

(x)
(stop)
