;; an attempt to translate
;; https://twitter.com/redFrik/status/333317729073381377
;; play{a=LFSaw;mean({|i|Ringz.ar(Blip.ar(a.ar(i+1/[3,4])>(a.ar(i+1/8)+1)*25+50,i+[2,3])*a.ar(i+1/50,i/25),i+1*99,0.1)}!50)/5}
;; to Overtone
;;
;; play {
;;   a=LFSaw;
;;   mean(
;;     { |i|
;;       Ringz.ar(
;;         Blip.ar(
;;           a.ar(i+1/[3,4]) > (a.ar(i+1/8) + 1 ) * 25 + 50,
;;           i+[2,3]
;;         )
;;         *
;;         a.ar(
;;           i+1/50,
;;           i/25
;;         ),
;;         i+1*99,
;;         0.1
;;       )
;;     } ! 50
;;   ) / 5
;; }
;;
;; play {
;;   mean(
;;     { |i|
;;       Ringz.ar(
;;         Blip.ar(
;;           LFSaw.ar(i+1/[3,4]) > (LFSaw.ar(i+1/8) + 1) * 25 + 50,
;;           i+[2,3]
;;         ) * LFSaw.ar(i+1/50, i/25),
;;         i+1*99,
;;         0.1
;;       )
;;     } ! 50
;;   ) / 5
;; }
;;
;; Remember precedence in Supercollider is strict left-to-right
;; i+1/50 is really (i+1)/50
;;
(ns explore-overtone.red-frick
  (:use [overtone.live]))

;; NOTE: default overtone settings gives this message when compiling
;; the defsynth:
;;   exception in GraphDef_Recv: exceeded number of interconnect buffers.
;;
;; Workaround (from overtone/sc/machinery/server/args.clj) is to add
;;   :sc-args { :max-w-buffers 512 }
;; in .overtone/config.clj
;;
;; but that still doesn't get it working.  If the for loop exceeds 39 you get
;;   Exception Invalid ugen tree passed to topological-sort-ugens, maybe you have cycles in the synthdef...
;;   overtone.sc.synth/topological-sort-ugens (synth.clj:415)
;;
(defsynth red-frik-130511
  []
  (out 0
       (/ (mix ;; docs suggest this is the same as 'mean'
           (for [i (range 39)] ;; 40 starts fail, see above
             (ringz:ar (* (blip:ar (+ (* (> (lf-saw:ar (/ (+ i 1) [3 4]))
                                            (+ (lf-saw:ar (/ (+ i 1) 8)) 1))
                                         25)
                                      50)
                                   (+ i [2 3]))
                          (lf-saw:ar (/ (+ i 1) 50)
                                     (/ i 25)))
                       (* (+ i 1) 99)
                       0.1)))
          40))) ;; reduce volume to avoid clipping

(red-frik-130511)
