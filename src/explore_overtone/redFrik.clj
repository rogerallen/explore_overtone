(ns explore-overtone.red-frik
  (:use [overtone.live]))

;; Translations of @redFrik's sctweets to Overtone

;; May 11, 2013
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
;; Opened up issue https://github.com/overtone/overtone/issues/231 and
;; Rich Hickey dropped by to fix this.  Neat!
(defsynth red-frik-333317729073381377
  []
  (out 0
       (/ (mix ;; docs suggest this is the same as 'mean'
           (for [i (range 50)]
             (ringz:ar (* (blip:ar (+ (* (> (lf-saw:ar (/ (+ i 1) [3 4]))
                                            (+ (lf-saw:ar (/ (+ i 1) 8)) 1))
                                         25)
                                      50)
                                   (+ i [2 3]))
                          (lf-saw:ar (/ (+ i 1) 50)
                                     (/ i 25)))
                       (* (+ i 1) 99)
                       0.1)))
          5)))

(red-frik-333317729073381377)
(stop)

;; May 1, 2013
;; https://twitter.com/redFrik/status/329680702205468674
;; {|k|play{a=SinOsc;Mix({|i|LeakDC.ar(Pan2.ar(a.ar(1/9+i,0,j=a.ar(i+1/99)),a.ar(i+1+k*(j.ceil*39+39),a.ar(k+2),j)))}!9)/3}}!2
;;
;; { |k|
;;   play{
;;     a=SinOsc;
;;     Mix(
;;       { |i|
;;         LeakDC.ar(
;;           Pan2.ar(
;;             a.ar(
;;               1/9+i,
;;               0,
;;               j=a.ar(i+1/99)
;;             ),
;;             a.ar(
;;               i+1+k*(j.ceil*39+39),
;;               a.ar(k+2),
;;               j
;;             )
;;           )
;;         )
;;       } ! 9
;;     ) / 3
;;   }
;; } ! 2
;;
;; { |k|
;;   play{
;;     Mix(
;;       { |i|
;;         LeakDC.ar(
;;           Pan2.ar(
;;             SinOsc.ar(
;;               1/9+i,
;;               0,
;;               j=SinOsc.ar(i+1/99)
;;             ),
;;             SinOsc.ar(
;;               i+1+k*(j.ceil*39+39),
;;               SinOsc.ar(k+2),
;;               j
;;             )
;;           )
;;         )
;;       } ! 9
;;     ) / 3
;;   }
;; } ! 2
;;

(defsynth red-frik-329680702205468674
  []
  (for [k (range 2)]
    (out k
         (/ (mix
             (for [i (range 9)]
               (let [j (sin-osc:ar (/ (+ i 1) 99))]
                 (leak-dc:ar
                  (pan2:ar (* (sin-osc:ar (+ (/ 1 9) i) 0)
                              j)
                           (* (sin-osc:ar (* (+ i 1 k) (+ (* (ceil j) 39) 39))
                                              (sin-osc:ar (+ k 2)))
                              j))))))
            3))))

(red-frik-329680702205468674)
(stop)

;; https://twitter.com/redFrik/status/329311535723839489
;; play{a=LFTri;RLPF.ar(LeakDC.ar(a.ar(LeakDC.ar((1..9).sum{|x|Pan2.ar(a.ar(1/x,x/3)>0.3333,a.ar(666/x))})*999)).distort,3e3)}
;;
;; play {
;;   RLPF.ar(
;;     LeakDC.ar(
;;       LFTri.ar(
;;         LeakDC.ar(
;;           (1..9).sum{
;;             |x|
;;             Pan2.ar(
;;               LFTri.ar(
;;                 1/x,
;;                 x/3) > 0.3333,
;;               LFTri.ar(666/x)
;;             )
;;           }
;;         ) * 999
;;       )
;;     ).distort,
;;     3e3
;;   )
;; }
(defsynth red-frik-329311535723839489
  []
  (out 0
       (rlpf:ar
        (distort
         (leak-dc:ar
          (lf-tri:ar
           (* (leak-dc:ar
               (sum
                (for [x (range 1 9)]
                  (pan2:ar
                   (> (lf-tri:ar (/ 1 x) (/ x 3)) 0.3333)
                   (lf-tri:ar (/ 666 x))))))
              999))))
        3e3)))

;; 218 chars.  :^)
;; (defsynth red-frik-329311535723839489 [] (out 0 (rlpf:ar (distort (leak-dc:ar (lf-tri:ar (* (leak-dc:ar (sum (for [x (range 1 9)] (pan2:ar (> (lf-tri:ar (/ 1 x) (/ x 3)) 0.3333) (lf-tri:ar (/ 666 x)))))) 999)))) 3e3)))

(red-frik-329311535723839489)
(stop)

;; https://twitter.com/redFrik/status/356151997256306691
;; play{a=LFTri;l=LocalBuf(b=600,9).clear;BufWr.ar(a.ar(c=(3..11)*3.5),l,a.ar(9/c,c/99)*b);Splay.ar(PlayBuf.ar(9,l,loop:1)/2)}
;;
;; play{
;;   a=LFTri;
;;   l=LocalBuf(b=600,9).clear;
;;   BufWr.ar(
;;     a.ar(
;;       c=(3..11)*3.5
;;     ),
;;     l,
;;     a.ar(9/c,c/99)*b
;;   );
;;   Splay.ar(
;;     PlayBuf.ar(9,l,loop:1)/2
;;   )
;; }
(defsynth red-frik-356151997256306691
  []
  (out 0
       (let [b 600
             l (local-buf b 9)
             _ (clear-buf l)
             _ (buf-wr:ar
                (lf-tri:ar
                 (for [c (* (range 3 11) 3.5)] c))
                l
                (* (lf-tri:ar
                    (for [c (* (range 3 11) 3.5)] (/ 9 c))
                    (for [c (* (range 3 11) 3.5)] (/ c 99)))
                   b))]
         (* 3 (splay:ar (play-buf 9 l :loop 1))))))
;; I needed to amplify it a bit...

(red-frik-356151997256306691)
(stop)
