(ns explore_overtone.quil_display
  (:use [overtone.live]
        [overtone.inst.sampled-piano])
  (:require [quil.core]))

;; ======================================================================
;; snapshot of code from https://github.com/rogerallen/irso that
;; displays a list of sequences in a window and follows the beat with
;; a cursor and highlighted notes.
;;
;; when I thought to do this, the timing was off & looking at it standalone
;; was enough to show me where it went wrong.
;;
;; thanks to http://www.seekingclojure.com/post/22144855523/reversi for
;; showing how to use partial with the sketch

;;(use 'overtone.live)
;;(use 'overtone.inst.sampled-piano)
;;(require 'quil.core)

;; ======================================================================
;; sequence manipulation
(defn linear-map
  "given points (x0,y0), (x1,y1) calculate linear relation y given x"
  [x0 x1 y0 y1 x]
  (let [dydx (/ (- y1 y0) (- x1 x0))
        dx (- x x0)]
    (+ y0 (* dydx dx))))
        
(defn velocity2attack
  "sampled-piano uses attack & level, not velocity"
  [v]
  (linear-map 0 127 0.2 0.05 v))

(defn velocity2level
  "sampled-piano uses attack & level, not velocity"
  [v]
  (linear-map 0 127 0.0 0.8 v))

(defn max-beat
  "how long is a snote sequence? last duration + last beat"
  [snote-seq]
  (let [last-snote (last snote-seq)]
    (+ (:beat last-snote) (:duration last-snote))))

(defn min-beat
  "starting beat"
  [snote-seq]
  (:beat (first snote-seq)))

(defn num-beats
  "how long is a snote sequence?"
  [snote-seq]
  (- (max-beat snote-seq) (min-beat snote-seq)))

(defn play-seq
  "play a list of (pitch velocity duration curbeat) where snote-seq is offset by in-beat"
  [inst m in-beat lazy-snote-seq]
  (last ; return beat following sequence
   (let [snote-seq (doall lazy-snote-seq)] ;; remove laziness here
     (for [cur-snote snote-seq]
       (let [cur-pitch (:pitch cur-snote)
             cur-attack (velocity2attack (:velocity cur-snote))
             cur-level (velocity2level (:velocity cur-snote))
             cur-dur (:duration cur-snote)
             cur-beat (+ in-beat (:beat cur-snote))
             k-beat 1.6
             cur-inst (at (m cur-beat) (inst :note cur-pitch
                                             :level cur-level
                                             :attack cur-attack))]
         (at (m (+ cur-beat (* k-beat cur-dur))) (ctl cur-inst :gate 0))
         (+ cur-beat cur-dur))))))

(defn play-seqs
  "play a list of snote-seq"
  [inst m in-beat snote-seqs]
  (last
   (for [snote-seq snote-seqs]
     (play-seq inst m in-beat snote-seq))))

;; ======================================================================
;; sequence display

;; colors from the solarized theme
(def base-colors
  (hash-map
   :base03    '(0x00 0x2b 0x36)
   :base02    '(0x07 0x36 0x42)
   :base01    '(0x58 0x6e 0x75)
   :base00    '(0x65 0x7b 0x83)
   :base0     '(0x83 0x94 0x96)
   :base1     '(0x93 0xa1 0xa1)
   :base2     '(0xee 0xe8 0xd5)
   :base3     '(0xfd 0xf6 0xe3)))
(def fore-colors
  (hash-map
   :yellow    '(0xb5 0x89 0x00)
   :orange    '(0xcb 0x4b 0x16)
   :red       '(0xdc 0x32 0x2f)
   :magenta   '(0xd3 0x36 0x82)
   :violet    '(0x6c 0x71 0xc4)
   :blue      '(0x26 0x8b 0xd2)
   :cyan      '(0x2a 0xa1 0x98)
   :green     '(0x85 0x99 0x00)))
(defn nth-fore-color [i]
  (let [num-colors (count fore-colors)
        nth-key (nth (keys fore-colors) (mod i num-colors))]
    (fore-colors nth-key)))

;; quil ftw!
(defn window-setup []
  (quil.core/smooth)
  (quil.core/frame-rate 60)
  (apply quil.core/background (:base1 base-colors)))

(defn window-draw [snote-seqs the-metronome offset-beat] ;; offset to give time for window to popup
  (let [seq-space 10
        seq-space2 4
        w (quil.core/width)
        draw-w (- w (* 2 seq-space))
        seq-w draw-w ;; FIXME?
        seq-h 50
        draw-h (- (* (+ seq-h seq-space2) (count snote-seqs)) seq-space2)
        h (+ (* 2 seq-space) draw-h)
        max-seq-beats (reduce max (map max-beat snote-seqs))
        cur-beat (- (/ (- (now) (start the-metronome)) (tick the-metronome)) offset-beat)]
    ;; background
    (apply quil.core/fill (:base1 base-colors))
    (quil.core/no-stroke)
    (quil.core/rect 0 0 w h)
    ;; drawing area background
    (apply quil.core/fill (:base00 base-colors))
    (quil.core/rect seq-space seq-space draw-w draw-h)  ;; FIXME round-rect
    ;; draw the sequences...
    (doseq [[i cur-seq] (map-indexed vector snote-seqs)]
      (let [cur-min-beat (min-beat cur-seq)
            cur-num-beats (num-beats cur-seq)
            x0 (+ seq-space (* seq-w (/ cur-min-beat max-seq-beats)))
            x1 (* seq-w (/ cur-num-beats max-seq-beats))
            y0 (+ seq-space (* (+ seq-h seq-space2) i))
            y1 seq-h]
        ;;(println i "drawRect" x0 y0 x1 y1)
        (quil.core/stroke-weight 1.2)
        (apply quil.core/fill (:base2 base-colors)) 
        (apply quil.core/stroke (:base02 base-colors))
        (quil.core/rect x0 y0 x1 y1)
        ;; draw the notes...
        (doseq [snote cur-seq]
          (let [nx0 (+ seq-space
                       (* seq-w (/ (:beat snote) max-seq-beats)))
                nx1 (+ seq-space
                       (* seq-w (/ (+ (:beat snote) (:duration snote))
                                   max-seq-beats)))
                ny (- (+ y0 y1)
                      (* seq-h (/ (:pitch snote) 127)))]
            ;;(println "drawStroke" nx0 nx1 ny)
            (if (and (>= cur-beat (:beat snote)) (<= cur-beat (+ (:beat snote) (:duration snote))))
              (do (quil.core/stroke-weight 3.0)
                  (apply quil.core/stroke (:base00 base-colors)))
              (do
                (quil.core/stroke-weight 1.5)
                (apply quil.core/stroke (nth-fore-color i))))
            (quil.core/line nx0 ny nx1 ny)))
        ))
    ;; draw line for metronome
    (if (and (>= cur-beat 0) (<= cur-beat max-seq-beats))
      (let [bx (+ seq-space (* seq-w (/ cur-beat max-seq-beats)))]
        (quil.core/stroke-weight 2.0)
        (apply quil.core/stroke (:base03 base-colors))
        (quil.core/line bx seq-space bx (+ seq-space draw-h))))
    ;; draw top rect outline
    (quil.core/stroke-weight 2.0)
    (quil.core/no-fill)
    (apply quil.core/stroke (:base02 base-colors))
    (quil.core/rect seq-space seq-space draw-w draw-h)))

(defn draw-seqs [snote-seqs the-metronome offset-beat window-name]
  (let [seq-h 50
        seq-space 10
        seq-space2 4
        draw-h (- (* (+ seq-h seq-space2) (count snote-seqs)) seq-space2)
        h (+ (* 2 seq-space) draw-h)]
    (quil.core/defsketch window-sketch
      :title "window-name" ;; FIXME
      :setup window-setup
      :draw (partial window-draw snote-seqs the-metronome offset-beat)
      :size [(* 0.95 (.width (.getScreenSize (java.awt.Toolkit/getDefaultToolkit)))) h])))

;; ======================================================================
(defn play-song []
  (let [m (metronome 120)
        seq-list '(({:pitch 65, :duration 1.0, :velocity 99, :beat  0}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat  1}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat  2}
                    {:pitch 65, :duration 1.0, :velocity 99, :beat  4}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat  5}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat  6}
                    {:pitch 65, :duration 1.0, :velocity 99, :beat  8}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat  9}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 10}

                    {:pitch 65, :duration 1.0, :velocity 99, :beat 40}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat 41}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 42}
                    {:pitch 65, :duration 1.0, :velocity 99, :beat 44}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat 45}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 46}
                    {:pitch 65, :duration 1.0, :velocity 99, :beat 48}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat 49}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 50})

                   ({:pitch 65, :duration 1.0, :velocity 99, :beat 20}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat 21}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 22}
                    {:pitch 65, :duration 1.0, :velocity 99, :beat 24}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat 25}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 26}
                    {:pitch 65, :duration 1.0, :velocity 99, :beat 28}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat 29}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 30}

                    {:pitch 69, :duration 1.0, :velocity 99, :beat 40}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 41}
                    {:pitch 65, :duration 1.0, :velocity 99, :beat 42}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat 44}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 45}
                    {:pitch 65, :duration 1.0, :velocity 99, :beat 46}
                    {:pitch 69, :duration 1.0, :velocity 99, :beat 48}
                    {:pitch 77, :duration 1.0, :velocity 99, :beat 49}
                    {:pitch 65, :duration 1.0, :velocity 99, :beat 50}
                    ))
        final-beat (play-seqs sampled-piano m 8 seq-list)
        the-frame (draw-seqs seq-list m 8 "quil overtone timing test")
        ]
    m))
;; (play-song)

