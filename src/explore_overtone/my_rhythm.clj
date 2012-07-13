(ns explore_overtone.my_rhythm
  (:use [overtone.music time]
        [overtone.music.rhythm]))

;; toying with the idea of a metronome that takes a tempo function and uses integration to find
;; the current beat.  Not finished & this may fail.

(defn bpm2mspb [bpm]
  "beats/min -> milliseconds/beat"
  (* 1000 (/ 60.0 (java.lang.Math/abs bpm))))

;; (now) ticks are in ms, not s.
(defn bpm2bpms [bpm]
  "beats/min -> beats/millisecond"
  (/ (/ (java.lang.Math/abs bpm) 60.0) 1000.0))

(defn bpms2bpm [bpms]
  (* (* bpms 1000) 60.0))

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
  (adaptive-rule-recurse booles-rule f a b 0.5)) ;; 1/2 ms should be enough accuracy

(defprotocol VRMetronome
  (var-metro-now-beat [metro]
    "convert (now) to a precise beat value")
  (var-metro-time [metro b]
    "convert b to a precise time value"))

(deftype VariableRateMetronome [start bpms-fn mspb-fn]

  VRMetronome
  ;; a metronome is like a rate * time = distance problem
  ;; a variable-rate metronome needs calculus...
  ;; distance (beats) = rate (beats/second) * time (seconds)
  ;; distance (beats) = integrate rate-fn(bps) dt
  (var-metro-now-beat [metro]
    "convert (now) to a precise beat value"
    (integrate @bpms-fn 0 (- (now) @start))) ;; beats start at 0
  ;; time (seconds) = (1/rate) (seconds/beat) * distance (beats)
  ;; time (seconds) = integrate rate-fn(spb) dBeats
  (var-metro-time [metro b]
    "convert b to a precise time value"
    (+ @start (integrate @mspb-fn 0 b)))

  IMetronome
  (metro-start [metro] @start)
  (metro-start [metro start-beat] ;; done (I think)
    (let [new-start (- (now) (var-metro-time metro start-beat))]
      (reset! start new-start)
      new-start))
  (metro-tick  [metro] (bpm2mspb (bpms2bpm (@bpms-fn (var-metro-now-beat metro)))))
  (metro-beat  [metro] (inc (long (var-metro-now-beat metro)))) ;; done
  (metro-beat  [metro b] (var-metro-time metro b)) ;; done
  (metro-bpm   [metro] (bpms2bpm (@bpms-fn (var-metro-now-beat metro)))) ;; done
  (metro-bpm   [metro new-bpm-fn]
    (let [cur-beat (metro-beat metro)
          new-bpms-fn (fn [x] (bpm2bpms (new-bpm-fn x)))
          new-mspb-fn (fn [x] (bpm2mspb (new-bpm-fn x)))
          new-start (- (metro-beat metro cur-beat) (integrate new-mspb-fn 0 cur-beat))]
      (reset! start new-start)
      (reset! bpms-fn new-bpms-fn)
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
        ;; beats-per-millisecond function since (now) is in ms, not s
        bpms-fn (atom (fn [x] (bpm2bpms (bpm-fn x))))
        ;; milliseconds-per-beat function
        mspb-fn (atom (fn [x] (bpm2mspb (bpm-fn x))))
        ]
    (VariableRateMetronome. start bpms-fn mspb-fn)))

;; ======================================================================
;; ======================================================================
;; ======================================================================
#_(
   (use 'overtone.live)
   (use 'overtone.inst.sampled-piano)
   (use 'explore_overtone.my_rhythm)
   (bpm2mspb 60)

   ;; very basic testing
   (defn mytempo [x] 100)
   (def nm (metronome 100.0)) (def vrm (variable-rate-metronome mytempo))
   (do
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
   (metro-bpm nm 60.0) (metro-bpm vrm (fn [x] 60))
   ;; copy-paste test above
   
   (defn mytempo [x] (+ 100 (* 10 (java.lang.Math/sin (/ x 3)))))
   (defn mytempo [x] (pwl-fn [0.0 60.0 100.0 180.0] x))
   (def vrm (variable-rate-metronome mytempo))
   (var-metro-now-beat vrm)
   (var-metro-time vrm 0)

   (defn song [m]
     (let [notes [:c3 :d3 :e3 :f3 :g3 :c3 :g3 :c3 :d3 :e3 :f3 :g3 :c3 :g3 :c3 :d3 :e3 :f3 :g3 :c3 :g3 ]
           start-beat (m)]
       (dotimes [i (count notes)]
         (let [nx (at (m (+ start-beat i)) (sampled-piano :note (note (notes i)) :level 0.8))]
           (at (m (+ start-beat i 0.75)) (ctl nx :gate 0))))))
   (song tst1)

   ;; this one is really bad since it has a huge discontinuity at 10.0-10.1.
   (defn mytempo-bad [x] (pwl-fn [0.0 60.0 10.0 120.0] (mod x 10)))

)
