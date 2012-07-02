;; a series of questions I have + any answers I've found so far.
(ns explore_overtone.qna)

;; ......................................................................
;; * how do I start overtone?
(use 'overtone.live)

;; ......................................................................
;; * how do I start the external server

;; download & install SuperCollider from http://supercollider.sourceforge.net/downloads/
;;  1) main app -- install normally
;;  2) extra plugins -- apparently on the mac it goes to...
;;     ~/Library/Application Support/SuperCollider/Extensions/
(use 'overtone.core)

(boot-external-server)
;; Works now bug fixed. https://github.com/overtone/overtone/issues/89

;; if you want already running server
;; bring up app first, open "localhost" window.  click boot button...
;; Welcome to SuperCollider 3.5.1, type cmd-d for help
;; booting 57110
(connect-external-server)

;; Note this for later...  If the server is on a different machine use
;; (connect-external-server "192.168.1.23" 57110) substituting the
;; appropriate hostname and port number.

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

;; here is a simple sine wave note
(definst goo [freq 440 amp 0.5 decay 0.6 ]
  (* (env-gen (perc 0.01 decay) 1 1 0 1 FREE)
     (sin-osc freq) amp))
(goo :freq 300 :decay 0.9)

;; ......................................................................
;; * how to connect to ipad or android controllers

;; * TouchOSC
;; 
;;   http://hexler.net/software/touchosc
;;
;;   for ipad/android + bridge.  bridge app doesn't show any gui, but
;;   there is something in the menu bar.  iOS is $5 and has GUI
;;   creation.  Android is free, but doesn't have GUI stuff yet.
;;
;;   Works for me where Control (below) did not.
;;

;; *NEW* midi info--use the event stream.
;; see https://groups.google.com/forum/?fromgroups#!topic/overtone/d0x91fJg06I
;; start/stop debug with
(event-debug-on) 
;; (event-debug-off) 
;; example handler...
(on-event [:midi :note-on] 
          (fn [e] 
            (let [note (:note e) 
                  vel  (:velocity e)] 
              (foo note vel))) 
          ::keyboard-handler) 

;; This is the *OLD WAY* 
(midi-in) ; brings up a dialog, but it can 'pop under' so look for it

;; just listen to the events coming in & 
(def kb (midi-in "TouchOSC Bridge"))
(def kb (midi-in "Control Session"))
(defn midi-listener [event ts]
  (println "listener: " event))
(midi-handle-events kb #'midi-listener)

;; * There is also Control (OSC + Midi)
;; 
;;   http://itunes.apple.com/us/app/control-osc-+-midi/id413224747?mt=8
;;   http://charlie-roberts.com/Control/
;;
;;   Unfortunately, this App is disappointing.  I cannot get reliable
;;   communication.  It "mostly" works, but it definitely has issues
;;   with closely spaced events.  A bit sad because I liked the
;;   javascript editor.

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

;; a simple metronome sequence repeating forever
(definst goo [freq 440 amp 0.5 decay 0.6 ]
  (* (env-gen (perc 0.01 decay) 1 1 0 1 FREE)
     (sin-osc freq) amp))
;;(goo)
(def metro (metronome 120))
(defn testmetro [ m beat ]
  (println "beat: " beat)
  (at (m (+ beat 0)) (goo 440)) ; beat 0 play note
  (at (m (+ beat 1)) (goo 400)) ; beat 1 play note
  (apply-at (m (+ beat 2))      ; beat 2 schedule a repeat
            #'testmetro m (+ beat 2) [])) ; call this routine over-and-over
(testmetro metro (metro))

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

(recording-start "~/Desktop/foo.wav")
;;make some noise...
(demo (pan2 (sin-osc)))
(recording-stop)

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
