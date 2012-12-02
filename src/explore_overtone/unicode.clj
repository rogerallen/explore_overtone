(comment
  Using ♯ and ♭ in clojure code and emacs.
  
  1. To enter these glyphs in emacs, use M-x set-input-method which is
  bound to C-x RET C-\. Enter "TeX" (Note, there are other options you
  can use.  See the docs.)

  2. Turn this input mode on/off with C-\.

  3. Type \sharp for ♯ and \flat for ♭.  Other options include greek
  letters, \natural for ♮, etc.

  4. Now, Clojure/Java needs to know your text file is UTF-8.  So, add
  this to your lein project.clj:
  
        :jvm-opts ["-Dfile.encoding=UTF-8"]

  5. Enjoy using the right characters for the job.

  This works for me on my Mac.  But, one thing I already have an issue
  with is that some chars do not show up in the italic version of my
  Monaco font.  So, they disappear in comments, which is annoying.

  I worry that this leads to problems on other platforms. So, the
  question is--does this work for you on your platform?

  both of these lines have the same characters...
;;♯ ♭ ♮ α β λ ω ∑ ∏ (Terminal works)
  ♯ ♭ ♮ α β λ ω ∑ ∏
  U+266D,E,F
  )

(def Ω 60)
(def pitches {:c 0
              :c♯ 1 :d♭ 1
              :d 2
              :d♯ 3 :e♭ 3
              :e 4
              :f 5
              :f♯ 6 :g♭ 6
              :g 7 
              :g♯ 8 :a♭ 8
              :a 9
              :a♯ 10 :b♭ 10
              :b 11
              })
(println "Check out these strings: C♯ and E♭.  Even G♮")

(defn ▵ [κ] ;; That's \triangle and \kappa 
  (+ (pitches κ) Ω))

(▵ :g♯) ;; -> 68
(▵ (keyword "E♭")) ;; -> 61
