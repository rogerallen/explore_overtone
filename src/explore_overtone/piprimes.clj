(ns explore_overtone.piprimes)
(do
  (use 'overtone.core)
  (connect-external-server 57110))

;; ======================================================================
;; okay what about pi?
(do 
  (use 'overtone.inst.piano)
  (def pi8 '(3 1 4 1 5 9 2 6))
  (defn nth-note-of-scale 
    "given a digit 'n' in range 0..9 find index in scale defined by tonic & type
     e.g
     user> (nth-note-of-scale :c4 :major 1)
     62"
    [tonic type n]
    (nth (vec (scale tonic type (range 1 10))) (mod n 10)))
  (defn playpi8 [m beat tonic type]
    (do
      (println "beat: " beat " " pi8)
      (doseq [[index cur-index]
              (map vector (iterate inc 0) pi8 )]
        (at (m (+ beat index)) (piano (nth-note-of-scale tonic type cur-index)))))))

;;(def metro (metronome 120))
;;(playpi8 metro (metro) :a3 :minor)
;;(playpi8 metro (metro) :c3 :major)
(let [b (metro)]
  (playpi8 metro b :a3 :minor)
  (playpi8 metro (+ b 4) :c3 :major))
;;(stop)

;; ======================================================================
;; having constant velocity & duration sucks...
;; mo pi...

;; pi to 1000 digits
(def pi1000 '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3
  2 7 9 5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3 7 5 1 0 5 8 2 0 9 7 4 9 4 4 5
  9 2 3 0 7 8 1 6 4 0 6 2 8 6 2 0 8 9 9 8 6 2 8 0 3 4 8 2 5 3 4 2 1 1
  7 0 6 7 9 8 2 1 4 8 0 8 6 5 1 3 2 8 2 3 0 6 6 4 7 0 9 3 8 4 4 6 0 9
  5 5 0 5 8 2 2 3 1 7 2 5 3 5 9 4 0 8 1 2 8 4 8 1 1 1 7 4 5 0 2 8 4 1
  0 2 7 0 1 9 3 8 5 2 1 1 0 5 5 5 9 6 4 4 6 2 2 9 4 8 9 5 4 9 3 0 3 8
  1 9 6 4 4 2 8 8 1 0 9 7 5 6 6 5 9 3 3 4 4 6 1 2 8 4 7 5 6 4 8 2 3 3
  7 8 6 7 8 3 1 6 5 2 7 1 2 0 1 9 0 9 1 4 5 6 4 8 5 6 6 9 2 3 4 6 0 3
  4 8 6 1 0 4 5 4 3 2 6 6 4 8 2 1 3 3 9 3 6 0 7 2 6 0 2 4 9 1 4 1 2 7
  3 7 2 4 5 8 7 0 0 6 6 0 6 3 1 5 5 8 8 1 7 4 8 8 1 5 2 0 9 2 0 9 6 2
  8 2 9 2 5 4 0 9 1 7 1 5 3 6 4 3 6 7 8 9 2 5 9 0 3 6 0 0 1 1 3 3 0 5
  3 0 5 4 8 8 2 0 4 6 6 5 2 1 3 8 4 1 4 6 9 5 1 9 4 1 5 1 1 6 0 9 4 3
  3 0 5 7 2 7 0 3 6 5 7 5 9 5 9 1 9 5 3 0 9 2 1 8 6 1 1 7 3 8 1 9 3 2
  6 1 1 7 9 3 1 0 5 1 1 8 5 4 8 0 7 4 4 6 2 3 7 9 9 6 2 7 4 9 5 6 7 3
  5 1 8 8 5 7 5 2 7 2 4 8 9 1 2 2 7 9 3 8 1 8 3 0 1 1 9 4 9 1 2 9 8 3
  3 6 7 3 3 6 2 4 4 0 6 5 6 6 4 3 0 8 6 0 2 1 3 9 4 9 4 6 3 9 5 2 2 4
  7 3 7 1 9 0 7 0 2 1 7 9 8 6 0 9 4 3 7 0 2 7 7 0 5 3 9 2 1 7 1 7 6 2
  9 3 1 7 6 7 5 2 3 8 4 6 7 4 8 1 8 4 6 7 6 6 9 4 0 5 1 3 2 0 0 0 5 6
  8 1 2 7 1 4 5 2 6 3 5 6 0 8 2 7 7 8 5 7 7 1 3 4 2 7 5 7 7 8 9 6 0 9
  1 7 3 6 3 7 1 7 8 7 2 1 4 6 8 4 4 0 9 0 1 2 2 4 9 5 3 4 3 0 1 4 6 5
  4 9 5 8 5 3 7 1 0 5 0 7 9 2 2 7 9 6 8 9 2 5 8 9 2 3 5 4 2 0 1 9 9 5
  6 1 1 2 1 2 9 0 2 1 9 6 0 8 6 4 0 3 4 4 1 8 1 5 9 8 1 3 6 2 9 7 7 4
  7 7 1 3 0 9 9 6 0 5 1 8 7 0 7 2 1 1 3 4 9 9 9 9 9 9 8 3 7 2 9 7 8 0
  4 9 9 5 1 0 5 9 7 3 1 7 3 2 8 1 6 0 9 6 3 1 8 5 9 5 0 2 4 4 5 9 4 5
  5 3 4 6 9 0 8 3 0 2 6 4 2 5 2 2 3 0 8 2 5 3 3 4 4 6 8 5 0 3 5 2 6 1
  9 3 1 1 8 8 1 7 1 0 1 0 0 0 3 1 3 7 8 3 8 7 5 2 8 8 6 5 8 7 5 3 3 2
  0 8 3 8 1 4 2 0 6 1 7 1 7 7 6 6 9 1 4 7 3 0 3 5 9 8 2 5 3 4 9 0 4 2
  8 7 5 5 4 6 8 7 3 1 1 5 9 5 6 2 8 6 3 8 8 2 3 5 3 7 8 7 5 9 3 7 5 1
  9 5 7 7 8 1 8 5 7 7 8 0 5 3 2 1 7 1 2 2 6 8 0 6 6 1 3 0 0 1 9 2 7 8
  7 6 6 1 1 1 9 5 9 0 9 2 1 6 4 2 0 1 9 8 9))

(do 
  (defn s3v [s]
    "given a list s, make it into a list of 3-vectors"
    (map vector
         (doall (take-nth 3 s))
         (doall (take-nth 3 (nthrest s 1)))
         (doall (take-nth 3 (nthrest s 2)))))

  (defn nth-note-of-scale [tonic type n]
    "given a digit 'n' in range 0..9 find index in scale defined by
     tonic & type e.g
     user> (nth-note-of-scale :c4 :major 1)
     62"
    (nth (vec (scale tonic type (range 1 10))) (mod n 10)))

  (defn i2v [n]
    "given a digit 'n' in range 0..9 find a velocity to play"
    (+ 60 (* 3 n)))

  (defn i2b [n]
    "given a digit 'n' in range 0..9 find a length in beats"
    (cond
      (= 0 n) 1
      (= 1 n) 1
      (= 2 n) 1.5
      (= 3 n) 1.5
      (= 4 n) 1.75
      (= 5 n) 1.75
      (= 6 n) 2
      (= 7 n) 2
      (= 8 n) 2.5
      (= 9 n) 2.5))

  (defn playmopi [m beat tonic type num-notes offset]
    "play some pi in a certain key"
    (do
      ;;(println "beat: " beat "num-notes: " num-notes "offset: " offset)
      (doseq [cur-3v
              (take 1 (nthrest (s3v pi1000) offset))]
        (let [cur-note (nth-note-of-scale tonic type (nth cur-3v 0))
              cur-vel (i2v (nth cur-3v 1))
              cur-dur (i2b (nth cur-3v 2))
              next-measure-beat (+ beat cur-dur)
              next-num-notes (- num-notes 1)
              next-offset (+ offset 1)]
          (println "mopi:" beat cur-note cur-vel cur-dur)
          (at (m beat) (piano cur-note 1 cur-vel))
          (if (> num-notes 0)
            (apply-at (m next-measure-beat)
                      #'playmopi m next-measure-beat tonic type next-num-notes next-offset [])))))))


;;(def metro (metronome 120))
;;(playmopi metro (metro) :e3 :pentatonic 16 0)
;;(stop)

;; ======================================================================
;; larger sequences.  use primes...
(do
  (playmopi metro (metro) :d3 :pentatonic 13 0)
  (playmopi metro (+ (metro) (* 3 13)) :d3 :pentatonic 13 0))
;; primes to 1000
(def primes1000 '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67
  71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157
  163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251
  257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353
  359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457
  461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571
  577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673
  677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797
  809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911
  919 929 937 941 947 953 967 971 977 983 991 997))

