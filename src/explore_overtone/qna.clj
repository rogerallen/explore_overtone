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
;; * how to connect to OSC ipad or android controllers

(midi-in) ; brings up a dialog, but it can 'pop under' so look for it

;; just listen to the events coming in & 
(def kb (midi-in "TouchOSC Bridge"))
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
;; ......................................................................
;; * how do I add "presence"/"echo"/"reverb" effects to a song.
;; ......................................................................
;; * what is the history/difference between defsynth & definst?




