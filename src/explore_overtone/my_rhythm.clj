(ns explore_overtone.my_rhythm
  (:use [overtone.music time]
        [overtone.music.rhythm]))

;; A Variable Rate (vr) metronome is a metronome that takes a tempo
;; function and uses integration to find the current beat.
;;
;; A variable-rate metronome is like solving a rate * time = distance
;; problem, where the "distance' is measured in 'beats'.  Since rate
;; is a function of time, this metronome needs calculus to solve for
;; the current beat (given rate & time) or to solve for time (given
;; rate & beat) or current distance
;;
;; the basic formula is:
;;   distance (beats) = rate (beats/second) * time (seconds)
;; and since rate is a function of the current beat
;;   distance (beats) = integrate bps-rate-fn() dt
;; similarly,
;;   time (seconds) = (1/rate) (seconds/beat) * distance (beats)
;;   time (seconds) = integrate spb-rate-fn() dBeats
;;
;; As long as your tempo functions do not have discontinuities (aka
;; they change smoothly) then this should work out.  Be careful,
;; though, since discontinuities can hang the solver.
;;
;; Another thing to keep in mind is that the integration function will
;; take longer & longer as the current beat gets further from zero.
;; It may also become more imprecise, so try to work near beat 0.  Use
;; the fn (start-beat m 0) to reset when you can.  This is the biggest
;; issue with the current implementation.
;;
;; Currently uses a numerical integration algorithm that I found on
;; John Lawrence Aspden's blog.  See:
;; http://www.learningclojure.com/2011/05/numerical-integration-better_26.html
;; There are likely better, faster & more-robust algorithms out there
;; that we could use, but this one was consise & solid enough to work
;; through some initial examples.
;;
;; Another tack to take might be to use direct integration to figure
;; out the current beat.  For simple functions like the
;; piece-wise-linear function, and sine + offset below, the integral
;; function is pretty straightforward and perhaps the better way to go
;; about this would be to pass both the function and the integration
;; function.  This could also be a way to allow for discontinuities
;; that are likely a major issue with the current implementation.
;;

(defn bpm2mspb
  "beats/min -> milliseconds/beat"
  [bpm]
  (* 1000 (/ 60.0 (java.lang.Math/abs bpm))))

;; (now) ticks are in ms, not s.
(defn bpm2bps
  "beats/min -> beats/millisecond"
  [bpm]
  (/ (java.lang.Math/abs bpm) 60.0))

(defn bps2bpm
  "beats/sec -> beats/min"
  [bps]
  (* bps 60.0))

(defn pwl-fn
  "piece-wise-linear function.  Given a list of [x0 y0 x1 y1 ... xN
  yN] control points, along with a current x value, solve for the y
  value & return it.  If x < x0, return y0.  If x > xN, return yN."
  [control-points x]
  (let [[xs0 xs1] (split-with #(<= % x) (take-nth 2 control-points))
        [ys0 ys1] (split-at (count xs0) (take-nth 2 (drop 1 control-points)))]
    (if (empty? xs0)
      (first ys1) ;; x < x0, return y0
      (if (empty? xs1)
        (last ys0) ;; x > xN, return yN
        (let [x0 (if (empty? xs0) (first xs1) (last xs0))
              x1 (if (empty? xs1) (last xs0) (first xs1))
              y0 (if (empty? ys0) (first ys1) (last ys0))
              y1 (if (empty? ys1) (last ys0) (first ys1))
              m (/ (- y1 y0) (- x1 x0))]
          (+ y0 (* m (- x x0))))))))

;; functions from John Lawrence Aspden blog discussion
(defn booles-rule [f a b]
  (let [midpoint1 (/ (+ a a a b) 4)
        midpoint2 (/ (+ a a b b) 4)
        midpoint3 (/ (+ a b b b) 4)]
    (* 1/90
       (- b a)
       (+ (* 7 (f a))
          (* 32 (f midpoint1))
          (* 12 (f midpoint2))
          (* 32 (f midpoint3))
          (* 7 (f b))))))

(defn adaptive-rule-recurse [rule f a b desired-error]
  (let [guess (rule f a b)
        midpoint (/ (+ a b) 2)
        better-guess (+ (rule f a midpoint) (rule f midpoint b))
        error-estimate (- guess better-guess)
        abs-error-estimate (if (> error-estimate 0) error-estimate (- error-estimate))]
    (if (< abs-error-estimate desired-error) better-guess
        (let [half-desired-error (/ desired-error 2)]
          (+ (adaptive-rule-recurse rule f a midpoint half-desired-error)
             (adaptive-rule-recurse rule f midpoint b half-desired-error))))))

(defn integrate [f a b]
  ;; accurate to 5/100 of a beat or second...good enough?
  ;; something to keep investigating...
  (adaptive-rule-recurse booles-rule f a b 0.05))

;; Add 2 functions to the Metronome protocol
(defprotocol VRMetronome
  (vr-metro-now-beat [metro]
    "convert (now) to a precise floating-point beat value.  Not a whole number.")
  (vr-metro-time [metro b]
    "convert beat b to a precise time value in milliseconds (ticks)."))

(deftype VariableRateMetronome [start bps-fn mspb-fn]

  VRMetronome
  ;; distance (beats) = rate (beats/second) * time (seconds)
  ;; distance (beats) = integrate rate-fn(bps) dt
  (vr-metro-now-beat [metro]
    "convert (now) to a precise beat value"
    (integrate @bps-fn 0 (* 0.001 (- (now) @start)))) ;; time in seconds, not ms
  ;; time (seconds) = (1/rate) (seconds/beat) * distance (beats)
  ;; time (seconds) = integrate rate-fn(spb) dBeats
  (vr-metro-time [metro b]
    "convert b to a precise time value"
    (+ @start (integrate @mspb-fn 0 b)))

  IMetronome
  (metro-start [metro] @start)
  (metro-start [metro start-beat]
    (let [new-start (- (now) (- (vr-metro-time metro start-beat) @start))]
      (reset! start new-start)
      new-start))
  (metro-tick  [metro] (bpm2mspb (bps2bpm (@bps-fn (vr-metro-now-beat metro)))))
  (metro-beat  [metro] (inc (long (vr-metro-now-beat metro))))
  (metro-beat  [metro b] (vr-metro-time metro b))
  (metro-bpm   [metro] (bps2bpm (@bps-fn (vr-metro-now-beat metro))))
  (metro-bpm   [metro new-bpm-fn]
    (let [cur-beat (metro-beat metro)
          new-bps-fn (fn [x] (bpm2bps (new-bpm-fn x)))
          new-mspb-fn (fn [x] (bpm2mspb (new-bpm-fn x)))
          new-start (- (metro-beat metro cur-beat) (integrate new-mspb-fn 0 cur-beat))]
      (reset! start new-start)
      (reset! bps-fn new-bps-fn)
      (reset! mspb-fn new-mspb-fn)
      nil)) ;; return is different from metronome.  okay?

  clojure.lang.ILookup
  (valAt [this key] (.valAt this key nil))
  (valAt [this key not-found]
    (cond (= key :start) @start
          (= key :bpm) (metro-bpm this)
          :else not-found))

  clojure.lang.IFn
  (invoke [this] (metro-beat this))
  (invoke [this arg]
    (cond
     (number? arg) (metro-beat this arg)
     (= :bpm arg) (metro-beat this)
     :else (throw (Exception. (str "Unsupported metronome arg: " arg)))))
  ;; I don't know what this is. ??? (invoke [this _ new-bpm] (.bpm this new-bpm)))
  )

(defn vr-metronome
  "A variable rate metronome is a beat management function that varies
  over time according to a tempo function. Give it a function of the
  current beat that returns the tempo (beats per minute) at that beat.
  Call the returned function with no arguments to get the next beat
  number, or pass it a beat number to get the timestamp to play a note
  at that beat.

  FIXME - Add more docs here.  See source for the moment."
  [bpm-fn]
  (let [start (atom (now))
        ;; beats-per-second function. Remember (now) is in ms, not s
        bps-fn (atom (fn [x] (bpm2bps (bpm-fn x))))
        ;; milliseconds-per-beat function
        mspb-fn (atom (fn [x] (bpm2mspb (bpm-fn x))))
        ]
    (VariableRateMetronome. start bps-fn mspb-fn)))

;; ======================================================================
;; Testing...
;; ======================================================================
#_(
   (use 'overtone.live)
   (use 'overtone.inst.sampled-piano)
   (use 'explore_overtone.my_rhythm)
   ;; for dev (use :reload-all 'explore_overtone.my_rhythm)

   ;; ----------------------------------------------------------------------
   ;; mary
   (defn song [m]
     (let [notes [:b3 :a3 :g3 :a3
                  :b3 :b3 :b3 :b3
                  :a3 :a3 :a3 :a3
                  :b3 :d4 :d4 :d4
                  :b3 :a3 :g3 :a3
                  :b3 :b3 :b3 :b3
                  :a3 :a3 :b3 :a3
                  :g3 :g3 :g3 :g3]
           ;; IMPORTANT: reset metronome start-beat back to 0
           ;; You don't want to have the integrator working too hard
           ;; as it integrates from 0...current-beat
           _ (metro-start m 0)] 
       (println "beat & beat-time in seconds")
       (dotimes [i (count notes)]
         (println (format "%d %.1f" i (/ (- (m i) (now)) 1000.0)))
         (let [nx (at (m i) (sampled-piano :note (note (notes i)) :level 0.8))]
           (at (m (+ i 0.75)) (ctl nx :gate 0))))))

   ;; metronomes to try with (song vrm)
   ;; An important feature is C0 continuity == no "jumps" in tempo
   (def vrm (vr-metronome
             (fn [x] (pwl-fn [0.0 60.0 16.0 240.0 32.0 60.0] x))))
   (song vrm)
   ;; others, just to play around...
   (def vrm (vr-metronome
             #(pwl-fn [0.0 240.0 16.0 60.0 32.0 240.0] %)))
   (def vrm (vr-metronome
             #(pwl-fn [0.0 60.0 6.0 240.0 10.0 240.0 12.0 60.0] (mod % 12))))
   ;; this has a nice "swing"...cosine wave with a period of 4 beats.
   (def vrm (vr-metronome
             #(+ 180 (* 90 (java.lang.Math/cos (/ (* % 2 3.14159265358979) 4.0))))))

   ;; this one is really bad since it has a huge discontinuity at 10.0-10.1.
   ;; Just for illustration--don't do this unless you want to hang.
   ;; Happily C-c C-c seems to work.  :^)
   ;; (defn mytempo-bad [x] (pwl-fn [0.0 60.0 10.0 120.0] (mod x 10)))

   ;; ----------------------------------------------------------------------
   ;; notes & testing below...uninteresting for others, likely.
   ;; ----------------------------------------------------------------------
   ;; trying a varying sine wave + offset works "okay" but the naive
   ;; integrator can get messed up.  We need to work around a
   ;; serious issue -- as time goes on and ((now) - start-beat) gets
   ;; large, the integrator gets more & more inaccurate & the beat can
   ;; go backwards in time.
   (defn mytempo [x] (+ 120 (* 60 (java.lang.Math/sin (/ x 3.1415)))))
   (def nm (metronome 100.0))
   (def vrm (vr-metronome mytempo))
   (dotimes [i 20]
     (println (nm) (vr-metro-now-beat vrm)))
   (dotimes [i 20]
     (println (nm) (vrm)))
   (dotimes [i 20]
     (println i (- (vrm i) (metro-start vrm))))
   
   ;; ----------------------------------------------------------------------
   ;; very basic testing of normal vs. variable (when variable doesn't vary)
   (bpm2mspb 60)
   (defn test-nm-vrm [nm vrm]
     (println "--------------------")
     (println "metro-start" (metro-start nm) (metro-start vrm) (= (metro-start nm) (metro-start vrm)))
     (println "metro-tick" (metro-tick nm) (metro-tick vrm) (= (metro-tick nm) (metro-tick vrm)))
     (println "metro-beat" (metro-beat nm) (metro-beat vrm) (= (metro-beat nm) (metro-beat vrm)))
     (println "metro-beat-10" (metro-beat nm 10) (metro-beat vrm 10) (= (metro-beat nm 10) (metro-beat vrm 10)))
     (println "metro-bpm" (metro-bpm nm) (metro-bpm vrm) (= (metro-bpm nm) (metro-bpm vrm)))
     (println ":start" (:start nm) (:start vrm) (= (:start nm) (:start vrm)))
     (println ":bpm" (:bpm nm) (:bpm vrm) (= (:bpm nm) (:bpm vrm)))
     (println "(call)" (nm) (vrm) (= (nm) (vrm)))
     (println "(call 100)" (nm 100) (vrm 100) (= (nm 100) (vrm 100)))
     )
   (defn mytempo [x] 100)
   (def nm (metronome 100.0)) (def vrm (vr-metronome mytempo))
   (test-nm-vrm nm vrm)
   (Thread/sleep 2000)
   (metro-bpm nm 60.0) (metro-bpm vrm (fn [x] 60))
   (test-nm-vrm nm vrm)
   (Thread/sleep 2000)
   (metro-start nm (nm)) (metro-start vrm (vrm))
   (test-nm-vrm nm vrm)

   
)
