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
    (long (+ @start (integrate @mspb-fn 0 b)))) ;; ??? should this be converted to long?

  IMetronome
  (metro-start [metro] @start) ;; done
  (metro-start [metro start-beat] ;; done (I think)
    (let [new-start (- (now) (var-metro-time metro start-beat))]
      (reset! start new-start)
      new-start))
  (metro-tick  [metro] (@bpms-fn (var-metro-now-beat metro))) ;; current tick value
  (metro-beat  [metro] (inc (long (var-metro-now-beat metro)))) ;; done
  (metro-beat  [metro b] (var-metro-time metro b)) ;; done
  (metro-bpm   [metro] (bpms2bpm (@bpms-fn (var-metro-now-beat metro)))) ;; done
  (metro-bpm   [metro new-bpm-fn]
    (let [cur-beat (metro-beat metro)
          new-bpms-fn (fn [x] (bpm2bpms (apply new-bpm-fn x)))
          new-mspb-fn (fn [x] (bpm2mspb (apply new-bpm-fn x)))
          new-start (- (metro-beat metro cur-beat) (integrate new-mspb-fn 0 cur-beat))]
      (reset! start new-start)
      (reset! bpms-fn (atom new-bpms-fn))
      (reset! mspb-fn (atom new-mspb-fn))
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

#_(
   (use 'overtone.live)
   (use 'explore_overtone.my_rhythm)
   (bpm2mspb 60)
   (defn mytempo [x] (+ 100 (* 10 (java.lang.Math/sin (/ x 3)))))
   (defn mytempo2 [x] (pwl-fn [0.0 60.0 100.0 180.0] x))
   (defn mytempo3 [x] (pwl-fn [0.0 60.0 10.0 120.0] (mod x 10)))
   (def tst1 (variable-rate-metronome mytempo))
   (def tst2 (variable-rate-metronome mytempo2))
   (metro-start tst1)
   (var-metro-now-beat tst1)
   (var-metro-time tst1 0)

   (use 'overtone.inst.sampled-piano)
   (defn song [m]
     (let [notes [:c3 :d3 :e3 :f3 :g3 :c3 :g3 :c3 :d3 :e3 :f3 :g3 :c3 :g3 :c3 :d3 :e3 :f3 :g3 :c3 :g3 ]
           start-beat (m)]
       (dotimes [i (count notes)]
         (let [nx (at (m (+ start-beat i)) (sampled-piano :note (note (notes i)) :level 0.8))]
           (at (m (+ start-beat i 0.75)) (ctl nx :gate 0))))))
   (song tst1)
   
)
