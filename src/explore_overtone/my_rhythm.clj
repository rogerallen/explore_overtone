(ns explore-overtone.my-rhythm
  (:use [overtone.music time]
        [overtone.music.rhythm]))

;; A Variable Rate (vr) metronome is a metronome that takes a tempo
;; function and uses numerical methods to find the current beat.
;;
;; A variable-rate metronome is a kind of rate * time = distance
;; problem, where the "distance' is measured in 'beats'.  Since rate
;; is a function of time, this metronome needs calculus and
;; differential equations to solve for the current beat or the current
;; time.
;;
;; The basic formula is:
;;   distance (beats) = rate (beats/second) * time (seconds)
;; 
;; But, the tempo (i.e. rate) is specified as a function of the current beat.
;;   tempo (beats/sec) = f(beat) = bps-rate-fn(beat)
;; which makes this a differential equation.
;;
;; So, to solve for beats,
;;   distance (beats) = integrate bps-rate-fn(beats) dt               [1]
;; requires Euler's method or perhaps the Runge-Kutta method to solve.
;;
;; But, to solve for time,
;;   time (seconds) = (1/rate) (seconds/beat) * distance (beats)
;;   time (seconds) = integrate spb-rate-fn(beat) dBeats              [2]
;; there is separation of variables, so we can use numerical integration.
;;
;; VariableRateMetronome and vr-metronome
;; 
;; Currently uses code based on John Lawrence Aspden's blog.
;; * Euler's method:
;;   http://www.learningclojure.com/2010/09/clojure-is-fast.html
;; * Numerical integration:
;;   http://www.learningclojure.com/2011/05/numerical-integration-better_26.html
;; The code could likely be improved, but it one was concise & solid enough
;; to work through some initial examples.
;;
;; As long as your tempo functions do not have discontinuities (e.g.
;; they change smoothly) then this method should work.  Be careful,
;; though, since discontinuities can hang the solver.
;;
;; Another thing to keep in mind is that the integration function will
;; take longer & longer as the current beat gets further from zero.
;; It may also become more imprecise, so try to work near beat 0.  Use
;; the function (start-beat m 0) to reset when you can.  This is the biggest
;; problem with the current implementation.
;;
;; VariableRateMetronome2 and pwl-metronome
;; 
;; For some functions, direct integration can be used to figure out
;; the current beat or time.  This removes the largest issue with
;; VariableRateMetronome--the iteration time required to solve.  With
;; direct integration, we can just call a function, and with handy
;; online sources like Wolfram Alpha, we can find these functions
;; easily enough. For simple functions like the piece-wise-linear
;; function, the integral function is straightforward.  We can pass
;; both the function and the integration function.
;;
;; This is the type of metronome used by pwl-metronome.
;;
;; I want to thank Anthony Grabowski for his help with the math.  I
;; couldn't have done it without him.
;;
;; Etc
;;
;; Found an old discussion on this:
;; http://lalists.stanford.edu/lad/1999/05/0127.html and I see that
;; there is something similar in the pythonsound project "timewarp"
;; http://pythonsound.sourceforge.net/guide/x240.html
;; 

(defn bpm2mspb
  "beats/min -> milliseconds/beat"
  [bpm]
  (* 1000 (/ 60.0 (Math/abs bpm))))

;; (now) ticks are in ms, not s.
(defn bpm2bps
  "beats/min -> beats/millisecond"
  [bpm]
  (/ (Math/abs bpm) 60.0))

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

(defn pwl-fn-integrate
  "y = m x + b.  'y = 1/2 m x^2 + b x + y0"
  [x0 y0 x1 y1 x]
  (let [m (/ (- y1 y0) (- x1 x0))
        dx (- x x0)]
    (+ 0 (* y0 dx) (* 1/2 m dx dx))))

;; need help to determine y0-y1 range in this case
(defn lin-time2beat
  "returns the beat corresponding to current time t (in
seconds). y0,v0 and y1,v1 are beat,tempo(in beats/second) start & end
points."
  [y0 v0 y1 v1 t]
  (let [a v0
        b (/ (- v1 v0) (- y1 y0))
        y (+ y0 (* (/ a b) (- (Math/exp (* b t)) 1.0)))]
    y))

;; easy to determine y0-y1 range in this case
(defn lin-beat2time
  "returns the time (in seconds) corresponding to current beat y.
y0,v0 and y1,v1 are beat,tempo(in beats/second) start & end
points."
  [y0 v0 y1 v1 y]
  (let [a v0
        b (/ (- v1 v0) (- y1 y0))
        u (+ a (* b (- y y0)))
        t (/ (Math/log (/ u a)) b)]
    t))

(defn pwl-time2beat
  "Given a list of [x0 y0 x1 y1 ... xN yN] tempo control points, along
  with a current x (time) value, solve for the beat value & return it.
  If x < x0 or x > xN, return nil"
  [control-points x]
  ;; correllate control points with time first.  solve for all t
  ;; values & store in ts first.  FIXME-optimize & do this once.
  (let [ts (reductions + (conj (map #(apply lin-beat2time %)
                                    (map vector
                                         (take-nth 2 control-points)
                                         (take-nth 2 (drop 1 control-points))
                                         (take-nth 2 (drop 2 control-points))
                                         (take-nth 2 (drop 3 control-points))
                                         (take-nth 2 (drop 2 control-points)))) 0.0))
        [ts0 ts1] (split-with #(<= % x) ts)
        [xs0 xs1] (split-at (count ts0) (take-nth 2 control-points))
        [ys0 ys1] (split-at (count ts0) (take-nth 2 (drop 1 control-points)))
        ]
    (if (empty? xs0) nil ;; x < x0
        (if (empty? xs1) nil ;; x >= xN
            (let [x0 (last xs0)
                  x1 (first xs1)
                  y0 (last ys0)
                  y1 (first ys1)
                  t0 (last ts0)]
              (lin-time2beat x0 y0 x1 y1 (- x t0)))))))

(defn pwl-beat2time
  "Given a list of [x0 y0 x1 y1 ... xN yN] tempo control points, along
  with a current x (beat) value, solve for the time value & return it.
  If x < x0 or x > xN, return nil"
  [control-points x]
  (let [[xs0 xs1] (split-with #(<= % x) (take-nth 2 control-points))
        [ys0 ys1] (split-at (count xs0) (take-nth 2 (drop 1 control-points)))]
    (if (empty? xs0)
      nil ;; x < x0
      (if (empty? xs1)
        nil ;; x >= xN ;; FIXME -- special case x = xN
        (let [x0 (last xs0)
              x1 (first xs1)
              y0 (last ys0)
              y1 (first ys1)
              t0 (reduce + (map #(apply lin-beat2time %)
                                (map vector
                                     xs0
                                     ys0
                                     (drop 1 xs0)
                                     (drop 1 ys0)
                                     (drop 1 xs0))))]
          (+ t0 (lin-beat2time x0 y0 x1 y1 x)))))))


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
  ;; accurate to 1/100 of a beat...good enough?
  (adaptive-rule-recurse booles-rule f a b 0.01))

(defn euler-iter [f t0 y0 h its]
  (let [zero (int 0)]
    (loop [t0 (double t0) y0 (double y0) h (double h) its (int its)]
      (if (> its zero) 
        (let [t1 (+ t0 h)
              y1 (+ y0 (* h (f y0)))] ;; note bps-fn does not depend on t
          (recur t1 y1 h (dec its)))
      y0))))

(defn euler [f t]
  (let [n   5000          ;; number of iterations/second...enough? too much?
        h   (/ 1.0 n)      ;; h is the iteration delta in seconds
        its (int (* n t))] ;; time expressed as total iterations
  (euler-iter f 0.0 0.0 h its)))

;; Add 2 functions to the Metronome protocol
(defprotocol VRMetronome
  (vr-metro-beat [metro ticks]
    "convert (now) to a precise floating-point beat value.  Not a whole number.")
  (vr-metro-now-beat [metro]
    "convert (now) to a precise floating-point beat value.  Not a whole number.")
  (vr-metro-time [metro b]
    "convert beat b to a precise time value in milliseconds (ticks)."))

;; ======================================================================
;; Variable Rate Metronome (first...not sure it works)
;; ======================================================================
(deftype VariableRateMetronome [start bps-fn mspb-fn]

  VRMetronome
  (vr-metro-beat [metro ticks]
    "convert ticks (in ms) to a precise floating-point beat value.  Not a whole number."
    (euler @bps-fn (* 0.001 (- ticks @start)))) ;; time in seconds, not ms
  (vr-metro-now-beat [metro]
    "convert (now) to a precise beat value"
    (vr-metro-beat metro (now)))
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
;; Variable Rate Metronome #2 -- using more direct methods
;; ======================================================================
(deftype VariableRateMetronome2 [start beat2bps-fn time2beat-fn beat2time-fn]

  VRMetronome
  (vr-metro-beat [metro ticks]
    "convert ticks (in ms) to a precise floating-point beat value.  Not a whole number."
    (let [delta-ticks (- ticks @start)
          delta-seconds (* 0.001 delta-ticks)]
      (@time2beat-fn delta-seconds)))
  (vr-metro-now-beat [metro]
    "convert (now) to a precise beat value"
    (vr-metro-beat metro (now)))
  (vr-metro-time [metro b]
    "convert b to a precise time in ticks (ms)"
    (+ @start (* 1000 (@beat2time-fn b))))

  IMetronome
  (metro-start [metro] @start)
  (metro-start [metro start-beat]
    (let [new-start (- (now) (- (vr-metro-time metro start-beat) @start))]
      (reset! start new-start)
      new-start))
  (metro-tick  [metro] (bpm2mspb (bps2bpm (@beat2bps-fn (vr-metro-now-beat metro)))))
  (metro-beat  [metro] (inc (long (vr-metro-now-beat metro))))
  (metro-beat  [metro b] (vr-metro-time metro b))
  (metro-bpm   [metro] (bps2bpm (@beat2bps-fn (vr-metro-now-beat metro))))
  (metro-bpm   [metro bpm-control-points]
    (let [cur-beat (metro-beat metro)
          bps-control-points (apply vector
                                    (flatten (map vector
                                                  (take-nth 2 bpm-control-points)
                                                  (map bpm2bps
                                                       (take-nth 2 (drop 1 bpm-control-points))))))
          new-beat2bps-fn (partial pwl-fn bps-control-points)
          new-time2beat-fn (partial pwl-time2beat bps-control-points)
          new-beat2time-fn (partial pwl-beat2time bps-control-points)
          new-start (- (metro-beat metro cur-beat) (* 1000 (beat2time-fn cur-beat)))]
      (reset! start new-start)
      (reset! beat2bps-fn new-beat2bps-fn)
      (reset! time2beat-fn new-time2beat-fn)
      (reset! beat2time-fn new-beat2time-fn)
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

(defn pwl-metronome 
  "A piecewise-linear (pwl) variable rate metronome is a beat
  management function that varies over time according to specified
  tempos and beats. Give it a list of tempo (beats per minute) and
  beat pairs to use.  Call the returned function with no arguments to get the
  next beat number, or pass it a beat number to get the timestamp to
  play a note at that beat.

  FIXME - Add more docs here.  See source for the moment."
  [bpm-control-points]
  (let [start (atom (now))
        bps-control-points
        (apply
         vector
         (flatten (map vector
                       (take-nth 2 bpm-control-points)
                       (map bpm2bps (take-nth 2 (drop 1 bpm-control-points))))))
        beat2bps-fn (atom (partial pwl-fn bps-control-points))
        time2beat-fn (atom (partial pwl-time2beat bps-control-points))
        beat2time-fn (atom (partial pwl-beat2time bps-control-points))]
    (VariableRateMetronome2. start beat2bps-fn time2beat-fn beat2time-fn)))

;; ======================================================================
;; Testing...
;; ======================================================================
#_(
   (use 'overtone.live)
   (use 'overtone.inst.sampled-piano)
   (use 'explore-overtone.my-rhythm)

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
   ;; slow transition with pwl-metronome
   (def vrm (pwl-metronome [0.0 60.0 16.0 240.0 32.0 60.0 1000.0 60.001]))
   (song vrm)
   ;; same, but use vr-metronome
   ;; Hmm. I see differences in beat time vs. above.  FIXME?
   (def vrm (vr-metronome
             #(pwl-fn [0.0 60.0 16.0 240.0 32.0 60.0 1000.0 60.001] %)))
   (song vrm)
   
   ;; fast transitions (not possible with vr-metronome)
   (def vrm (pwl-metronome [ 0.0  60.0   15.99  60.01
                            16.0 240.0   27.99 240.01
                            28.0  60.0 1000.00  60.01]))
   (song vrm)

   ;; repeat over 8 beats
   (def vrm (vr-metronome
             #(pwl-fn [0.0 60.0 4.0 240.0 6.0 240.0 8.0 60.0] (mod % 8))))
   (song vrm)
   
   ;; this has a nice "swing"...cosine wave with a period of 4 beats.
   (def vrm (vr-metronome
             #(+ 180 (* 90 (Math/cos (/ (* % 2 Math/PI) 4.0))))))
   (song vrm)

   ;; this one is really bad since it has a huge discontinuity at 10.0-10.1.
   ;; Just for illustration--don't do this unless you want to hang.
   ;; Happily C-c C-c seems to work.  :^)
   ;; (defn mytempo-bad [x] (pwl-fn [0.0 60.0 10.0 120.0] (mod x 10)))

   ;; ----------------------------------------------------------------------
   ;; notes & testing below...uninteresting for others, likely.
   ;; 
   ;; trying a varying sine wave + offset works "okay" but the naive
   ;; integrator can get messed up.  We need to work around a
   ;; serious issue -- as time goes on and ((now) - start-beat) gets
   ;; large, the integrator gets more & more inaccurate & the beat can
   ;; go backwards in time.
   (defn mytempo [x] (+ 120 (* 60 (Math/sin (/ x Math/PI)))))
   (def nm (metronome 100.0))
   (def vrm (vr-metronome mytempo))
   (def vrm2 (pwl-metronome [0.0 100.0 10000.0 100.01]))
   (dotimes [i 20]
     (println (nm) (vr-metro-now-beat vrm) (vr-metro-now-beat vrm2)))
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

   ;; ----------------------------------------------------------------------
   ;; this shows that vr-metro-time & vr-metro-beat match up.
   (defn test-b-t [name vrm vrm2]
     ;;(println "b" "dt" "b'")
     (println "(def" name "(to-dataset [")
     (dotimes [i 11]
       (let [cur-beat (* i 10.0)
             start-tick  (metro-start vrm)
             start-tick2 (metro-start vrm2)
             cur-tick    (long (vr-metro-time vrm cur-beat))
             cur-tick2   (long (vr-metro-time vrm2 cur-beat))
             delta-time (* 0.001 (- cur-tick start-tick))
             delta-time2 (* 0.001 (- cur-tick2 start-tick2))
             calc-beat (vr-metro-beat vrm cur-tick)
             calc-beat2 (vr-metro-beat vrm2 cur-tick2)
             delta-beat (- calc-beat2 calc-beat)
             ]
         ;;(println (format "%.2f %.3f %.3f %.3f %s"
         ;;                 s dst sb (Math/abs (- s sb)) (< (Math/abs (- s sb)) 0.01)))
         (println (format "{\"beat\" %.2f \"time\" %.3f \"time2\" %.3f \"calc-beat\" %.3f \"calc-beat2\" %.3f \"dbeat\" %.3f }"
                          cur-beat delta-time delta-time2 calc-beat calc-beat2 delta-beat))
         ))
     (println "]))")
     )
   (def vrm (vr-metronome #(pwl-fn [0.0 60.0 110.0 180.0] %)))
   (def vrm2 (pwl-metronome [0.0 60.0 110.0 180.0]))
   (test-b-t "dfaster" vrm vrm2)
   (def vrm (vr-metronome #(pwl-fn [0.0 60.0 110.0 20.0] %)))
   (def vrm2 (pwl-metronome [0.0 60.0 110.0 20.0]))
   (test-b-t "dslower" vrm vrm2)

   ;; incanter stuff...
   (use '(incanter core charts))
   (with-data dfaster
     (doto (xy-plot :time :calc-beat2 :series-label "good" :legend true)
       (add-lines :time :calc-beat :series-label "bad")
       (add-lines :time :dbeat :series-label "delta")
       (view)))

   ;; use 0 60bpm 100 120bpm => 0 1bps 100 2bps
   (def vrm (vr-metronome #(pwl-fn [0.0 60.0 100.0 120.0] %)))
   ;; bps -> beats
   (view (function-plot (fn [x] (+ (/ (* x x) -400) x)) 0 100))
   (with-data ds
     (doto (xy-plot :beat :time)
       (add-function (fn [x] (+ (/ (* x x) -400) x)) 0 100)
       (add-lines :beat2 :time)
       (view)))


)
