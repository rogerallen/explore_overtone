(ns
    ^{:doc "An oscilloscope style waveform viewer"
      :author "Jeff Rose & Sam Aaron"}  ;; + additions by Roger Allen
  explore-overtone.scope
  (:import [java.awt Graphics Dimension Color BasicStroke BorderLayout RenderingHints]
           [java.awt.event WindowListener ComponentListener]
           [java.awt.geom Rectangle2D$Float Path2D$Float]
           [javax.swing JFrame JPanel JSlider]
           [java.util.concurrent TimeoutException])
  (:use [clojure.stacktrace]
        [overtone.helpers lib]
        [overtone.libs event deps]
        [overtone.sc defaults server synth ugens buffer node foundation-groups]
        [overtone.studio core util])
  (:require [clojure.set :as set]
            [overtone.config.log :as log]
            [overtone.at-at :as at-at]
            [overtone.gui.control :as control]))
;; inspiration
;;   http://tekmuseum.ebaman.com/500/545a.html
;; TODO
;; x make width/height/fps parameters
;; x add trigger-level
;; x add controls 
;; - add trigger-slope 0/+/-
;; - display stereo
;; - add grids, ms/grid, N/grid

(defonce SCOPE-BUF-SIZE 4096)
(defonce scopes* (ref {}))
(defonce scope-pool (at-at/mk-pool))
(defonce scopes-running?* (ref false))
(defonce scope-group* (ref 0))

(defonce FPS 10)

(defonce DEFAULT-WIDTH 600)
(defonce DEFAULT-HEIGHT 400)
(defonce X-PADDING 5)
(defonce Y-PADDING 10)

(on-deps :studio-setup-completed
         ::create-scope-group #(dosync
                                (ref-set scope-group*
                                         (group "Scope" :tail (foundation-monitor-group)))
                                (satisfy-deps :scope-group-created)))

(defn- ensure-internal-server!
  "Throws an exception if the server isn't internal - scope relies on
  fast access to shared buffers with the server which is currently only
  available with the internal server. Also ensures server is connected."
  []
  (when (server-disconnected?)
    (throw (Exception. "Cannot use scopes until a server has been booted or connected")))
  (when (external-server?)
    (throw (Exception. (str "Sorry, it's  only possible to use scopes with an internal server. Your server connection info is as follows: " (connection-info))))))

(def TRIGGER-OFFSET 5) ;; some room to witness the transition
(defn- find-trigger-point
  "find where the data in the buffer travels over the trigger point"
  [buffer-data trigger-level]
  (loop [i TRIGGER-OFFSET]
    (let [d   (aget ^floats buffer-data i)
          d+1 (aget ^floats buffer-data (inc i))]
      (cond
       (= (- SCOPE-BUF-SIZE 2) i) 0
       (and (<= d trigger-level)
            (> d+1 trigger-level)) (- i TRIGGER-OFFSET)
       :else (recur (inc i))))))
(defn- scale-trigger-slider
  [t]
  (control/scale-val [0 99] [-0.5 0.5] t))

(defn- update-scope-data
  "Updates the scope by reading the current status of the buffer and repainting.
  Currently only updates bus scope as there's a bug in scsynth-jna which
  crashes the server after too many calls to buffer-data for a large
  buffer. As buffers tend to be large, updating the scope frequently
  will cause the crash to happen sooner. Need to remove this limitation
  when scsynth-jna is fixed."
  [s]
  (let [{:keys [buf buf2 size width height panel y-arrays x-array sliders]} s
        [x-slider y-slider t-slider] sliders
        frames    (if @(:update? s) (buffer-data buf) @(:frames s))
        frames2   (if @(:update? s) (buffer-data buf2) @(:frames s)) ;; FIXME :frames?
        step      (control/scale-val
                   [0 99]
                   [0.25 (/ (buffer-size buf) width)]
                   (.getValue x-slider))
        y-scale   (- height (* 2 Y-PADDING))
        trigger-level (scale-trigger-slider (.getValue t-slider))
        [y-a y-b y-c y-d] @y-arrays]

    (when-not (empty? frames)
      (let [trigger-point (find-trigger-point frames trigger-level)]
        (dotimes [x width]
          (let [ai (unchecked-add trigger-point
                                  (unchecked-multiply x step))]
            (aset ^ints y-b x
                  (if (>= ai SCOPE-BUF-SIZE)
                    (int 0)
                    (int (* y-scale (aget ^floats frames ai)))))
            (aset ^ints y-d x
                  (if (>= ai SCOPE-BUF-SIZE)
                    (int 0)
                    (int (* y-scale (aget ^floats frames2 ai)))))
            ))
        (reset! y-arrays [y-b y-a y-d y-c]) ;; swap front/back
        (.repaint panel)))

    (when (and (not (:bus-synth s)) ;; FIXME stereo?
               @(:update? s))
      (reset! (:frames s) frames)
      (reset! (:update? s) false))))

(defn- update-scopes []
  (dorun (map update-scope-data (vals @scopes*))))

(defn- paint-scope [^Graphics g id]
  (if-let [scope (get @scopes* id)]
    (let [{:keys [background width height color x-array y-arrays sliders]} scope
          [x-slider y-slider t-slider] sliders
          ;;sx-val (.getValue x-slider)
          sy-val (.getValue y-slider)
          y-zoom (if (> sy-val 49)
                   (+ 1 (* 0.1 (- sy-val 50)))
                   (+ (* 0.02 sy-val) 0.01))
          y-shift (+ (/ height 2.0) Y-PADDING)
          [y-a y-b y-c y-d] @y-arrays
          trigger-level (scale-trigger-slider (.getValue t-slider))
          ]
      (doto g
        (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        (.setColor ^Color background)
        (.fillRect 0 0 width height)
        (.setColor ^Color (Color. 100 100 100))
        (.drawRect 0 0 width height)
        (.translate 0 y-shift)
        (.scale 1 (* -1 y-zoom))
        (.setColor ^Color color)
        (.drawPolyline ^ints x-array ^ints y-a width)
        (.setColor ^Color (Color. 100 100 200))
        (.drawPolyline ^ints x-array ^ints y-c width)
        (.setColor ^Color (Color. 100 200 100))
        (.drawLine 0 (* trigger-level (- height (* 2 Y-PADDING)))
                   width (* trigger-level (- height (* 2 Y-PADDING))))
        ))))

(defn- scope-panel [id width height]
  (let [panel (proxy [JPanel] [true]
                (paint [g] (paint-scope g id)))
        _ (.setPreferredSize panel (Dimension. width height))]
    panel))

(defn- scope-frame
  "Display scope window. If you specify keep-on-top to be true, the
  window will stay on top of the other windows in your environment."
  ([panel sliders title keep-on-top width height]
     (let [f (JFrame. title)
           cp (.getContentPane f)
           side (JPanel. (BorderLayout.))
           [x-slider y-slider t-slider] sliders]
       (.add side x-slider BorderLayout/LINE_START)
       (.add side y-slider BorderLayout/CENTER)
       (.add side t-slider BorderLayout/LINE_END)
       (doto cp
         (.add side BorderLayout/WEST)
         (.add panel BorderLayout/CENTER))
       (doto f
         (.setPreferredSize (Dimension. width height))
         (.pack)
         (.show)
         (.setAlwaysOnTop keep-on-top)))))

(defn scopes-start
  "Schedule the scope to be updated every (/ 1000 FPS) ms (unless the
  scopes are already running in which case it does nothing."
  []
  (ensure-internal-server!)
  (dosync
   (when-not @scopes-running?*
     (at-at/every (/ 1000 FPS) update-scopes scope-pool :desc "Scope refresh fn")
     (ref-set scopes-running?* true))))

(defn- reset-data-arrays
  [scope]
  (let [width     (scope :width)
        x-array   (scope :x-array)
        height    (scope :height)
        [y-a y-b y-c y-d] @(scope :y-arrays)]

    (dotimes [i width]
      (aset x-array i i))

    (dotimes [i width]
      (aset y-a i (/ (- height (* 2 Y-PADDING)) 2))
      (aset y-b i (/ (- height (* 2 Y-PADDING)) 2))
      (aset y-c i (/ (- height (* 2 Y-PADDING)) 2))
      (aset y-d i (/ (- height (* 2 Y-PADDING)) 2)))))

(defn- empty-scope-data
  []
  (dorun (map reset-data-arrays (vals @scopes*))))

(defn scopes-stop
  "Stop all scopes from running."
  []
  (ensure-internal-server!)
  (at-at/stop-and-reset-pool! scope-pool)
  (empty-scope-data)
  (dosync (ref-set scopes-running?* false)))

(defn- start-bus-synth
  [bus buf]
  (bus->buf :target @scope-group* bus buf))

(defn- scope-bus
  "Set a bus to view in the scope."
  [s]
  (let [buf (buffer SCOPE-BUF-SIZE)
        buf2 (buffer SCOPE-BUF-SIZE)
        bus-synth (start-bus-synth (:num s) buf)
        bus-synth2 (start-bus-synth (inc (:num s)) buf2)
        ]
    (assoc s
      :size SCOPE-BUF-SIZE
      :bus-synth bus-synth
      :bus-synth2 bus-synth2
      :buf buf
      :buf2 buf2)))

;; FIXME – not updated for stereo!
(defn- scope-buf
  "Set a buffer to view in the scope."
  [s]
  (let [info (buffer-info (:num s))]
    (assoc s
      :size (:size info)
      :buf  info)))

(defn scope-close
  "Close a given scope. Copes with the case where the server has crashed
  by handling timeout errors when killing the scope's bus-synth."
  [s]
  (log/info (str "Closing scope: \n" s))
  (let [{:keys [id bus-synth bus-synth2]} s]
    (when (and bus-synth
               (server-connected?))
      (try
        (kill bus-synth)
        (catch Exception e)))
    (when (and bus-synth2
               (server-connected?))
      (try
        (kill bus-synth2)
        (catch Exception e)))
    (dosync (alter scopes* dissoc id))))

(defn- mk-scope
  [num kind keep-on-top width height]
  (let [id    (uuid)
        name  (str kind ": " num)
        panel (scope-panel id width height)
        sliders [(JSlider. JSlider/VERTICAL 0 99 50)
                 (JSlider. JSlider/VERTICAL 0 99 50)
                 (JSlider. JSlider/VERTICAL 0 99 50)]
        frame (scope-frame panel sliders name keep-on-top width height)
        x-array (int-array width)
        y-a     (int-array width) ;; front buffer, left channel
        y-b     (int-array width) ;; back, left
        y-c     (int-array width) ;; front buffer, right channel
        y-d     (int-array width) ;; back, right
        scope {:id id
               :name name
               :size 0
               :num num
               :panel panel
               :sliders sliders
               :kind kind
               :color (Color. 0 130 226)
               :background (Color. 50 50 50)
               :frame frame
               :width width
               :height height
               :x-array x-array
               :y-arrays (atom [y-a y-b y-c y-d])
               :update? (atom true)
               :frames (atom [])}

        _ (reset-data-arrays scope)]
    (.addWindowListener frame
      (reify WindowListener
        (windowActivated [this e])
        (windowClosing [this e]
                       (scope-close (get @scopes* id)))
        (windowDeactivated [this e])
        (windowDeiconified [this e])
        (windowIconified [this e])
        (windowOpened [this e])
        (windowClosed [this e])))

    (case kind
          :bus (scope-bus scope)
          :buf (scope-buf scope))))

(defn scope
  "Create a scope for either a bus or a buffer. Defaults to scoping bus 0.
   Example use:
   (scope :bus 1)
   (scope :buf 10)"
  ([&{:keys [bus buf keep-on-top width height]
      :or {bus 0
           buf -1
           keep-on-top false
           width DEFAULT-WIDTH
           height DEFAULT-HEIGHT}}]
     (ensure-internal-server!)
     (let [buf (if (buffer? buf) (:id buf) buf)
           kind (if (= -1 buf) :bus :buf)
           num  (if (= -1 buf) bus buf)
           s (mk-scope num kind keep-on-top width height)]
       (dosync (alter scopes* assoc (:id s) s))
       (scopes-start))))

(defn pscope
  "Creates a 'perminent' scope, i.e. one where the window is always kept
  on top of other OS windows. See scope."
  ([& args]
     (ensure-internal-server!)
     (apply scope (concat args [:keep-on-top true]))))

(defn- reset-scopes
  "Restart scopes if they have already been running"
  []
  (ensure-internal-server!)
  (dosync
   (ref-set scopes*
            (reduce (fn [new-scopes [k v]]
                      (let [new-scope (if (= :bus (:kind v))
                                        (scope-bus v)
                                        v)]
                        (assoc new-scopes k new-scope)))
                    {}
                    @scopes*))
   (scopes-start)))


(on-deps #{:synthdefs-loaded :scope-group-created} ::reset-scopes #(when (internal-server?)
                                                                     (reset-scopes)))
(on-sync-event :shutdown (fn [event-info]
                           (when (internal-server?)
                             (scopes-stop)
                             (dorun
                              (map (fn [s] scope-close s) @scopes*))))
               ::stop-scopes)

