;; Last night, I tweeted about some success I had with reverb & was
;; asked to post my findings.  In the end, this is pretty obvious in
;; hindsight, but trying to get rid of the ringing inherent in reverb
;; confounded me for a while, so I hope this saves someone else some
;; time. I'm also interested in hearing from more experienced folks
;; about their use of effects to create presence in their creations.

(use 'overtone.live)
;; using the piano so that we have lots of frequency content to
;; interact with the fx
(use 'overtone.inst.sampled-piano)

;; ======================================================================
;; create a simple 'song' with some dissonance to bring out the ringing
(def m (metronome 90))
(defn song []
  (let [notes [:c3 :d3 :e3 :f3 :g3 :c3 :g3]]
    (dotimes [i (count notes)]
      (let [nx (at (m (+ (m) i)) (sampled-piano :note (note (notes i)) :level 0.8))]
        (at (m (+ (m) i 0.5)) (ctl nx :gate 0))))))

;; ======================================================================
;; play it dry...boring.
(do (clear-fx sampled-piano)
    (song))

;; ======================================================================
;; let's try adding the simplest reverb--echo. This is just for
;; example purposes, other reverbs (g-verb, freeverb) have the same
;; issue & the same technique applies.
(defsynth fx-echo-that-rings
  [bus 0 max-delay 10.0 delay-time 0.15 decay-time 5.0]
  (let [source (in bus)
        echo (comb-c source max-delay delay-time decay-time)]
    (replace-out bus (pan2 (+ echo source) 0))))

;; play it again...a "ringing" builds up.  In a longer piece this is
;; really annoying & distracting.  This ringing is an inherent part of
;; any delay effect.
(do (clear-fx sampled-piano)
    (inst-fx! sampled-piano fx-echo-that-rings)
    (song))

;; ======================================================================
;; try to get rid of ringing with a lowpass filter
(defsynth fx-muddy-echo
  [bus 0 max-delay 10.0 delay-time 0.15 decay-time 5.0 lpf-freq 900]
  (let [source (in bus)
        echo (comb-c source max-delay delay-time decay-time)]
    (replace-out bus (pan2 (lpf (+ echo source) lpf-freq) 0))))

;; play it again...the result is a muddy piano sound.  yuck.
(do (clear-fx sampled-piano)
    (inst-fx! sampled-piano fx-muddy-echo)
    (song))

;; ======================================================================
;; now only lowpass filter the echo, not the piano.  Don't know why,
;; but I had to see this in the CSound book before it dawned on me to
;; try this.
(defsynth fx-better-echo
  [bus 0 max-delay 10.0 delay-time 0.15 decay-time 5.0 lpf-freq 900]
  (let [source (in bus)
        echo (comb-c source max-delay delay-time decay-time)
        lpf-echo (lpf echo lpf-freq)]
    (replace-out bus (pan2 (+ lpf-echo source) 0))))

;; play it again. piano comes through & ringing is diminished and the
;; ear doesn't seem to mind the lowpass filter on the echoes.
(do (clear-fx sampled-piano)
    (inst-fx! sampled-piano fx-better-echo)
    (song))

;; So, the result isn't exactly a "great" reverb, but it is on the way
;; to a much better situation than just naively adding echo without
;; filtering.  A reverb effect is made of both early & late
;; reflections, so more echoes need to be added.  All of these echoes
;; can have different lowpass filters & cutoff frequencies.

;; ======================================================================
;; Got a great reverb?  I'd love to hear about it...here is one I'm
;; liking at the moment.  
(defsynth fx-my-reverb
  [bus 0
   roomsize 30.0
   revtime 4.50
   damping 0.40
   inputbw 0.39
   spread 14.93
   drylevel 0.25
   earlyreflevel 0.20
   taillevel 0.10
   lpf-freq 1000.0
   maxroomsize 300.0
   gate 1.0]
  (let [source (in bus)
        my-reverb (* gate
                     (g-verb (* gate source)
                             roomsize revtime damping inputbw spread
                             drylevel earlyreflevel taillevel maxroomsize))
        lpf-reverb (lpf my-reverb lpf-freq)]
    (replace-out bus (+ lpf-reverb source))))
;; (you can replace lpf-reverb with my-reverb in the sum above to hear the ringing)

(do (clear-fx sampled-piano)
    (inst-fx! sampled-piano fx-my-reverb)
    (song))



