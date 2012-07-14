(ns explore_overtone.my_rhythm
  (:use [overtone.music time]
        [overtone.music.rhythm]))

;; toying with the idea of a metronome that takes a tempo function and uses integration to find
;; the current beat.  Not finished & this may fail.

(defn bpm2mspb [bpm]
  "beats/min -> milliseconds/beat"
  (* 1000 (/ 60.0 (java.lang.Math/abs bpm))))

;; (now) ticks are in ms, not s.
(defn bpm2bps [bpm]
  "beats/min -> beats/millisecond"
  (/ (java.lang.Math/abs bpm) 60.0))

(defn bps2bpm [bps]
  "beats/sec -> beats/min"
  (* bps 60.0))

(defn pwl-fn [control-points x]
  (let [[xs0 xs1] (split-with #(<= % x) (take-nth 2 control-points))
        [ys0 ys1] (split-at (count xs0) (take-nth 2 (drop 1 control-points)))]
    (if (empty? xs0)
      (first ys1)
      (if (empty? xs1)
        (last ys0)
        (let [x0 (if (empty? xs0) (first xs1) (last xs0))
              x1 (if (empty? xs1) (last xs0) (first xs1))
              y0 (if (empty? ys0) (first ys1) (last ys0))
              y1 (if (empty? ys1) (last ys0) (first ys1))
              m (/ (- y1 y0) (- x1 x0))]
          ;;(println x x0 x1 y0 y1 m)
          (+ y0 (* m (- x x0))))))))

;; using the integrator from
;; http://www.learningclojure.com/2011/05/numerical-integration-better_26.html

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
  ;;(println "integrate" a b)
  (adaptive-rule-recurse booles-rule f a b 0.05)) ;; 1/2 ms should be enough accuracy

(defprotocol VRMetronome
  (var-metro-now-beat [metro]
    "convert (now) to a precise beat value")
  (var-metro-time [metro b]
    "convert b to a precise time value"))

(deftype VariableRateMetronome [start bps-fn mspb-fn]

  VRMetronome
  ;; a metronome is like a rate * time = distance problem
  ;; a variable-rate metronome needs calculus.
  ;; distance (beats) = rate (beats/second) * time (seconds)
  ;; distance (beats) = integrate rate-fn(bps) dt
  (var-metro-now-beat [metro]
    "convert (now) to a precise beat value"
    (integrate @bps-fn 0 (* 0.001 (- (now) @start)))) ;; beats start at 0, measure time in s
  ;; time (seconds) = (1/rate) (seconds/beat) * distance (beats)
  ;; time (seconds) = integrate rate-fn(spb) dBeats
  (var-metro-time [metro b]
    "convert b to a precise time value"
    (+ @start (integrate @mspb-fn 0 b)))

  IMetronome
  (metro-start [metro] @start)
  (metro-start [metro start-beat] ;; done (I think)
    (let [new-start (- (now) (- (var-metro-time metro start-beat) @start))]
      (reset! start new-start)
      new-start))
  (metro-tick  [metro] (bpm2mspb (bps2bpm (@bps-fn (var-metro-now-beat metro)))))
  (metro-beat  [metro] (inc (long (var-metro-now-beat metro)))) ;; done
  (metro-beat  [metro b] (var-metro-time metro b)) ;; done
  (metro-bpm   [metro] (bps2bpm (@bps-fn (var-metro-now-beat metro)))) ;; done
  (metro-bpm   [metro new-bpm-fn]
    (let [cur-beat (metro-beat metro)
          new-bps-fn (fn [x] (bpm2bps (new-bpm-fn x)))
          new-mspb-fn (fn [x] (bpm2mspb (new-bpm-fn x)))
          new-start (- (metro-beat metro cur-beat) (integrate new-mspb-fn 0 cur-beat))]
      (reset! start new-start)
      (reset! bps-fn new-bps-fn)
      (reset! mspb-fn new-mspb-fn)
      nil)) ;; return changed...okay?

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
  ;; ??? (invoke [this _ new-bpm] (.bpm this new-bpm)))
  )

(defn variable-rate-metronome
  "A variable rate metronome is a beat management function that varies
  over time according to a tempo function. Give it a function of the
  current beat that returns the tempo (beats per minute) at that beat.
  Call the returned function with no arguments to get the next beat
  number, or pass it a beat number to get the timestamp to play a note
  at that beat.

  tempo function must be >= 0 or the universe explodes."

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
   ;;(use :reload-all 'explore_overtone.my_rhythm)
   (bpm2mspb 60)

   ;; very basic testing of normal vs. variable (when variable doesn't vary)
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
   (def nm (metronome 100.0)) (def vrm (variable-rate-metronome mytempo))
   (test-nm-vrm nm vrm)
   (Thread/sleep 2000)
   (metro-bpm nm 60.0) (metro-bpm vrm (fn [x] 60))
   (test-nm-vrm nm vrm)
   (Thread/sleep 2000)
   (metro-start nm (nm)) (metro-start vrm (vrm))
   (test-nm-vrm nm vrm)

   ;; ----------------------------------------------------------------------
   ;; trying a varying sine wave + offset works "okay" but the naive
   ;; integrator needs a bit of help.  We need to work around a
   ;; serious issue -- as time goes on and ((now) - start-beat) gets
   ;; large, the integrator gets more & more inaccurate & the beat can
   ;; go backwards in time.
   (defn mytempo [x] (+ 120 (* 60 (java.lang.Math/sin (/ x 3.1415)))))
   (def nm (metronome 100.0))
   (def vrm (variable-rate-metronome mytempo))
   (dotimes [i 20]
     (println (nm) (var-metro-now-beat vrm)))
   (dotimes [i 20]
     (println (nm) (vrm)))
   (dotimes [i 20]
     (println i (- (vrm i) (metro-start vrm))))
   
   (defn mytempo [x] (pwl-fn [0.0 60.0 12.0 180.0] x))
   (def vrm (variable-rate-metronome mytempo))
   (var-metro-now-beat vrm)
   (var-metro-time vrm 0)

   (defn song [m]
     (let [notes [:c3 :d3 :e3 :f3 :g3 :c3 :g3 :c3 :d3 :e3 :f3 :g3 :c3 :g3 :c3 :d3 :e3 :f3 :g3 :c3 :g3 ]
           _ (metro-start m (m)) ;; reset metronome to restart on next beat
           start-beat 0 ;; try starting at 0
           the-now (now)]
       (dotimes [i (count notes)]
         (println start-beat i (+ start-beat i) (m) (long (m (+ start-beat i))) the-now)
         (let [nx (at (m (+ start-beat i)) (sampled-piano :note (note (notes i)) :level 0.8))]
           (at (m (+ start-beat i 0.75)) (ctl nx :gate 0))))))
   (song vrm)

   ;; this one is really bad since it has a huge discontinuity at 10.0-10.1.
   (defn mytempo-bad [x] (pwl-fn [0.0 60.0 10.0 120.0] (mod x 10)))

)
