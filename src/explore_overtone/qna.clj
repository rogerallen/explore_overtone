;; a series of questions I have + any answers I've found so far.
(ns explore_overtone.qna)

;; ......................................................................
;; * how do I start overtone?
(use 'overtone.live)

;; ......................................................................
;; * how do I start the external server
;; ......................................................................
;; * how do I find documentation on 'demo' or anything else
(odoc demo)

;; ......................................................................
;; * how do we stop repl in a good way?
(System/exit 0)

;; this might be handy
(defn quit [] 
  (System/exit 0)) 

;; ......................................................................
;; * how do I load/use other files containing code? 
;; ......................................................................
;; * how to play a simple sine wave?
(demo 3 (sin-osc 300)) ; 3 seconds of a 300 Hz sine wave

;; ......................................................................
;; * how to connect to OSC ipad or android controllers

;; get http://hexler.net/software/touchosc for ipad/android + bridge
;; bridge app doesn't show any gui, but there is something in the menu bar.
;;
;; control also seems to work
;; http://itunes.apple.com/us/app/control-osc-+-midi/id413224747?mt=8
;; http://charlie-roberts.com/Control/

(midi-in) ; brings up a dialog, but it can 'pop under' so look for it

;; just listen to the events coming in & 
(def kb (midi-in "TouchOSC Bridge"))
(def kb (midi-in "Control Session"))
(defn midi-listener [event ts]
  (println "listener: " event))
(midi-handle-events kb #'midi-listener)

;; ......................................................................
;; * how to play notes rather than frequencies

;; converts midi index to cycles
(midicps i) 
;; funny, they have this, too.
(midi->hz i)

;; ......................................................................
;; * how to work with scales
;; ......................................................................
;; * how to select chords
;; ......................................................................
;; * how to play a sequence
;; ......................................................................
;; * how to play several parallel sequences/instruments
;; ......................................................................
;; * where are there organized versions of example instruments
;; ......................................................................
;; * how do I find quality sampled sounds

;; there is an automated way to get freesound stuff
(def kick-d (sample (freesound-path 41155)))

;; ......................................................................
;; * how do I find quality sampled instruments/packs
;; ......................................................................
;; * how do I play quality sampled instruments
;; ......................................................................
;; * how do I save the audio?
I saw this on the main website...
;; ......................................................................
;; * how do I add "presence"/"echo"/"reverb" effects to a song.
;; ......................................................................
;; * what is the history/difference between defsynth & definst?
;; ......................................................................
;; * how would I do this?  http://korg.com/monotrons# see schematic at
;;   http://korg.com/services/products/monotron/monotron_Block_diagram.jpg
;; ......................................................................
;; * What is wrong with this?

;; this works
(demo 3 (saw 300))

;; ??? this causes exception
(definst foo
  "foo"
  [freq 440]
  (saw freq))
(demo 3 (foo 300))
;; Evaluation aborted.
;; Error in checker for ugen out. out:ar requires that all incoming signals be audio rate
;; ???

;; but this seems to work
(foo 300)
(stop)
;; or 
(kill foo)

;; ......................................................................
;; * why can't I do a function around inst?
(defn lerp
  "linear interpoloate between a & b.  alpha=0 for a, alpha=1 for b."  
  [alpha a b]
  (+ (* (- 1 alpha) a)
     (* alpha b)))
(lerp 0.5 (sin-osc 440) (sin-osc 330))
