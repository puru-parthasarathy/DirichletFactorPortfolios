(ns geteoddata.utils
(:require
             [incanter.zoo :as zoo]
             [clj-time.format :as tf]
             [clj-time.core :as tt]
             [clj-time.local :as l]
             [clojure.java.io :as io]
             [clojure.string :as str]
              [faker.generate :as gen]
            ;[aerial.hanami.common :as hc]
            ;[aerial.hanami.templates :as ht]
            ;[aerial.hanami.core :as hmi]

             [incanter [core :refer [$]
                             :as incanter$]
                       [core :as incanter]
                       [stats :as stats]
                       [io :as io2]
                       [charts :as charts]
                       [datasets :as dataset]])
(:gen-class)
  )

(defn abs[x](if (> x 0) (identity x) (- x )) )




(defn first-non-nil-index 
"self explan"
[coll]
(first (map first (remove #(nil? (second %)) 
                    (map vector (range (count coll))  coll )
                    )))
)



(defn formatlocal [n offset]
  (let [nlocal (tt/to-time-zone n (tt/time-zone-for-offset offset))]
    (tf/unparse (tf/formatter-local "yyyy-MM-dd hh:mm:ss aa")
               nlocal)))

(
defn currentTime[]
(formatlocal (tt/now) -4)
)

;(currentTime)

(defn csum[s]
(take (count s) (reductions + s))
)

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))


(defn round
  "Round a double to the given precision (number of significant digits)"
  [d]
(round2 2 d)
  )


(defn round0
  "Round a double to the given precision (number of significant digits)"
  [d]
(round2 0 d)
  )


(defn round-negative
  "Round a double to the given precision (number of significant digits)"
  [d]
(- (round2 2 d))
  )



(defn sign[x]
  (cond (< x 0)   (- 1)
        (> x 0)   (identity 1)
        :else     (identity 0)

))

(defn- read-one
  [r]
  (try
    (read r)
    (catch java.lang.RuntimeException e
      (if (= "EOF while reading" (.getMessage e))
        ::EOF
        (throw e)))))

(defn read-seq-from-file
  "Reads a sequence of top-level objects in file at path."
  [path]
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader path))]
    (binding [*read-eval* false]
      (doall (take-while #(not= ::EOF %) (repeatedly #(read-one r)))))))


(defn log-diff[x]
(let[p (map incanter/log x) out (map - (rest p) (drop-last p) ) ](identity out))
)

(defn positive-else-zero [x]
(cond (pos? x) x :else 0)
)

(defn between-else-zero [ll ul x]
(cond (and (< x ul) (>= x ll)) x :else 0)
)




(defn
  diff_[x] 
  "reduces size by 1"
  (map - (rest x) (drop-last x) ))

(defn sign_[x]
 (if (zero? x) (identity 0) (/ x (stats/scalar-abs x)))
)

(defn correlation_[x y]
(let [ L (min
         (count x)
         (count y) )  ]
(stats/correlation (take L x) (take L y)))
)

(defn signalize[x]
  (cond (<  (stats/scalar-abs x) 0.95)  (identity 0)
        (> x 0.95) (identity 1)
        (< x (- 0.95)) (- 1)
        )
  )

;(defn sharpe[x] (/ (stats/mean x) (stats/sd x) ))


(defn sharpe[x]
  (let [
      m  (stats/mean x)
    s (stats/sd x)
        ]
(if (zero? s) (identity 0) (round (/ m s)) )
    )
  )

(defn pnl-sharpe
"sharpe pnl-avg N min max"
  [x]
  (let [
      m  (stats/mean x)
    s (stats/sd x)
        ]
    (if (zero? s) (identity [0 0 0 0 0])
(zipmap [:sharpe :pnl :non-zeros :min :max] (map round [ (round (/ m s))  m (count (filter #(not (zero? %)) x )   ) (apply min x) (apply max x)    ]))
          )
    )
  )

;(pnl-sharpe [1 2 3 4 5])


(defn ema[HL values]
 (cond  (< (count (remove nil? values))  (+ 1 HL) ) nil
:else

(last (map float (reductions (fn [running v]
                          (let [ f (/ 2 (+ 1 HL))
                                one-minus-F (- 1 f)] ;naming intermediate results can help with the readability of non-associative operators.
                            (+ (* f v)
                               (* one-minus-F running))))
                        values)))
         )
  )


(defn ema-all
"returns entire seq"
[HL values]
 (cond  (< (count (remove nil? values))  (+ 1 HL) ) nil
:else

(last (map float (reductions (fn [running v]
                          (let [ f (/ 2 (+ 1 HL))
                                one-minus-F (- 1 f)] ;naming intermediate results can help with the readability of non-associative operators.
                            (+ (* f v)
                               (* one-minus-F running))))
                        values)))
         )
  )



;(ema 10 '(1 2 3 4 5 6 7 8 9 10 11 ))

(defn lPlot2[one two]
;(incanter/view (charts/line-chart (range 0 (count x)) x))


(doto (charts/scatter-plot (range (count one)) one)
(charts/add-lines (range 0 (count two) ) two)
 incanter/view)



  )

(defn lPlot3[one two three]
;(incanter/view (charts/line-chart (range 0 (count x)) x))


(doto (charts/scatter-plot (range (count one)) one)
(charts/add-lines (range 0 (count two) ) two)
(charts/add-lines (range 0 (count three) ) three)

incanter/view)



  )







(defn lPlot[x title]
(incanter/view (charts/set-title (charts/line-chart (range 0 (count x)) x)
title

 ) 
 )
)



(
defn msec
 "msecs since 9:30"
[tstamp]
(let [
      hours

(. Integer parseInt
(str
(nth (str/split tstamp #"") 8 )
(nth (str/split tstamp #"") 9 )
))

minutes
(. Integer parseInt
(str
(nth (str/split tstamp #"") 10 )
(nth (str/split tstamp #"") 11 )
))

seconds
(. Integer parseInt
(str
(nth (str/split tstamp #"") 12 )
(nth (str/split tstamp #"") 13 )
))

mseconds

(. Integer parseInt
 (str
(nth (str/split tstamp #"") 14 )
(nth (str/split tstamp #"") 15 )
(nth (str/split tstamp #"") 16 )
))

      ]
  ;(identity mseconds)
  (int (+ mseconds (int (* 1000 (reduce + [(* 3600 (- hours 9)) (* 60 minutes) seconds]) ))))
)
)

(
defn tradingTime
 "actual time of the time stamp in AT"
[tstamp]
(let [
      hours

(. Integer parseInt
(str
(nth (str/split tstamp #"") 8 )
(nth (str/split tstamp #"") 9 )
))

minutes
(. Integer parseInt
(str
(nth (str/split tstamp #"") 10 )
(nth (str/split tstamp #"") 11 )
))

seconds
(. Integer parseInt
(str
(nth (str/split tstamp #"") 12 )
(nth (str/split tstamp #"") 13 )
))

mseconds

(. Integer parseInt
 (str
(nth (str/split tstamp #"") 14 )
(nth (str/split tstamp #"") 15 )
(nth (str/split tstamp #"") 16 )
))

      ]
  (str/join " " [hours minutes seconds])
  ;(int (+ mseconds (int (* 1000 (reduce + [(* 3600 (- hours 9)) (* 60 minutes) seconds]) ))))
)
)

(defn contextual-eval [ctx expr]
    (eval                                           
        `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)] 
             ~expr)))
(defmacro local-context []
    (let [symbols (keys &env)]
        (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))
(defn readr [prompt exit-code]
    (let [input (clojure.main/repl-read prompt exit-code)]
        (if (= input ::tl)
            exit-code
             input)))
;;make a break point
(defmacro break []
  `(clojure.main/repl
    :prompt #(print "debug=> ")
    :read readr
    :eval (partial contextual-eval (local-context))))







(defn zscore [coll]
"divide series by its own sd"
  (if (nil? coll) (identity nil) (let  [m (stats/mean coll)
                                        s (stats/sd coll)
                  s-clean (cond (zero? s) (identity nil) :else (identity s) )
                  normalized (map #(/ (- % m) s-clean) coll)

                  ] (map (partial round2 2) normalized) ))
 )




;(conj [1 2] '())

;(println (filter #(< % (- 3)) (zscore rets)))
(use 'clojure.data)

(defn winsorize [K coll]
  "winsorize at K sigma"
  (if (nil? coll) (identity nil)

      (if (nil? (zscore coll))  (identity nil)
          (let [z (zscore coll)
                s (stats/sd coll)
                m (stats/mean coll)

                original  (map vector (range (count z)) z)

                bigs (filter #(> (abs (second %)) K ) original )
                result (cond (empty? bigs) (identity coll)
                      :else (let [


          bigs-complement (filter #(not (> (abs (second %)) K )) original )

          ;;replace second element with 3 sigma
         bigs-mod (if (not (empty? bigs))  (map #(assoc % 1 (* 3 (sign (second %))) ) bigs) )
         new-zscore (sort-by first (remove nil? (concat bigs-mod bigs-complement)))
         reconstructed (map (partial round2 2)  (map #(+ (* (second % )  s) m) new-zscore))

                                  ] (identity reconstructed) )
                      )
                ] (identity result) )


          )
      )
  )



(defn normalize [coll]
"divide series by its own sd"
 (if (nil? coll) (identity nil) (let  [ s (stats/sd coll)
                  s-clean (cond (zero? s) (identity Double/NaN) :else (identity s) )
                  normalized (map #(/ % s) coll)

                  ] (identity normalized) ))
  )


(defn return_bps[x y]
(if (or (nil? x) (nil? y) (zero? y)) (identity 0)
(round2 2 (* 10000 (/ (- x y) y)))
)
)

(defn return-bps
"y is reference"
  [x y]
(if (or (nil? x) (nil? y) (zero? y)) (identity 0)
(round2 2 (* 10000 (/ (- x y) y)))
)
)

(defn return-bps-col[col]

  (cond (not= (count col) 2)  nil :else
        (let [x (second col)
              y (first col)
              ]
(if (or (nil? x) (nil? y) (zero? y)) (identity 0)
(round2 2 (* 10000 (/ (- x y) y)))
)
          )
        )

)

(defn sd
"more forgiving sd"
  [coll]

  (round2 2 (stats/sd coll))


  )


(defn sanitize [word] (apply str (filter (fn [x] (Character/isLetter x)) word)))

(defn random-word-unix [] (sanitize (rand-nth (str/split (slurp "/usr/share/dict/words") #"\n"))))

(defn random-word [] (sanitize (rand-nth (str/split (gen/word) #"\n"))))


;(repeatedly 100 #(random-word ))

(defn coll->string
"write coll to string with new line"
  [coll] (str (str/join "," coll) "\n"))


(defn winsorize-returns
"if x in bps cutoff at 10000 bps, 100% return"
  [x]
(cond  (> (incanter/abs x) 10000)
(* (sign x) 10000) :else x
         )
  )

(defn lag
"if x [ 1 2 3] inc in time, lag 1 x is [NaN 1 2]"
  [n x]
  (let [
        N (apply min [n (count x) ] )
        pad (repeat N Double/NaN)


        ]
(concat pad (drop-last n x)  )
    )


  )
;(lag 1 [1 2 3])

(defn lookahead
"if x [ 1 2 3] inc in time, lookahead 1 x is [2 3 NaN]"
  [n x]
  (let [
        N (apply min [n (count x) ] )
        pad (repeat N Double/NaN)


        ]
(concat (drop n x) pad  )
    )


  )

;(lookahead 10 [1 2 3 4 5 6])

(defn yyyymmdd->epoch [date] (.getMillis (tt/date-time (. Integer parseInt (subs (str date) 0 4))
                                                   (. Integer parseInt(subs (str date) 4 6))
                                                   (. Integer parseInt(subs (str date) 6 8))
                                                   )))

(defn view-indicator [indicator-map]
  (let [
        dates (map yyyymmdd->epoch (:dates indicator-map ))
        data (:data indicator-map )

        ]
(incanter/view (charts/time-series-plot dates data
                          :x-label "Year"))

    )





  )


(defn coll->string
"write coll to string with new line"
  [coll] (str (str/join "," coll) "\n"))

(defn coll->string2
"write coll to string with new line"
  [coll] (str (str/join " " coll) "\n"))


(def HOME (. System getProperty "user.home"))



(defn notnan?[x] (not(Double/isNaN x)))

(def nil-or-negative? #(or (nil? %) (neg? %)))

(defn locf
"na locf"
[coll]
(cond 
(empty? 
         (keep-indexed #(if (nil-or-negative? %2) %1) coll  )
        )
       (identity coll)
       (zero? (first 
         (keep-indexed #(if (nil-or-negative? %2) %1) coll  )
         ))
       (throw (Exception. "locf cannot be called on nil starting sequence, check 
your seqs for a valid start value"))
:else 

       (let [  idx (first (keep-indexed #(if (nil-or-negative? %2) %1) coll  ) )
             last-value    (last (take idx coll))
             recur-on-this (seq (assoc (vec coll) idx last-value))
             ;dummy (pprint recur-on-this)
             ]
         (recur recur-on-this)
         )
       ))



(defn locf-clean
"nil or negative prices are replaced by last value"
 [coll]

(let [

drop-these-many (first (keep-indexed #(if (not (nil-or-negative? %2)) %1)  coll))

output (concat (repeat drop-these-many nil) (locf (drop drop-these-many coll)))
]
output
)
)



(def coll [-10 nil 10 20 30 nil 25 -999 nil nil] )

(locf-clean coll)

(replace {nil Double/NaN} coll)

(defn locf-clean2 [coll]
  (->> coll locf-clean reverse locf-clean reverse)
  )


(defn scatter [x y]

(doto (charts/scatter-plot x y)
      (charts/set-stroke-color java.awt.Color/blue)
      incanter/view)


)

(defn Mean [x]
(stats/mean (remove nil? x))
)


(defn Sd2 [x]
(stats/sd (remove nil? x))
)



(defn nan->zero [coll]
(map #(cond (notnan? %) % :else 0) coll)
)


(defn write-to-psv 
"pipe separated values"
[sep header coll-of-coll]
(let [
filename (random-word)
print-nil (println filename)
]
(do (spit (str filename ".data") (str/join "\n"
(map #(str/join sep %) (apply map list (map cons header coll-of-coll))    ))))
)
)


(defn write-to-psv2
"pipe separated values"
[separator header-coll coll-of-coll]
(let [
filename (random-word)
print-nil (println filename)
;data-writeable (conj coll-of-coll (str/join "|" header-coll)) 
data-writeable (map #(str/join separator %)  (conj coll-of-coll header-coll)
)

]
(do (spit (str filename ".dat") 
(str/join "\n"  data-writeable  )))
)
)



(def data '((1 2 3) (3 4 5))    )


(def header '("aapl" "googl" "bkx"))

                                        ;(apply map list data)

                                        ;(str/join "\n"  (map #(str/join "|" %) data))




                                        ;(str/join "\n"  (map #(str/join "|" %) data)    )



(map #(str/join "|" %)  (conj data header))



(defn log-minus [x y]
(cond (or (zero? x) (zero? y)) 0.0 :else (- (incanter/log x) (incanter/log y)))
)

(defn position-nth [coll k]
(cond (== 1 (* -1 k))
(repeat (count (first coll)) 0.0 )
:else 
(clojure.core/nth coll k)
)
)

;(position-nth [[1 2 3 4]] -1  )



(defn third [coll]
(nth coll 2)
)

;(third [1 2 3 ])
(defn fourth [coll]
(nth coll 3)
)


(defn fifth [coll]
(nth coll 4)
)



(defn sixth [coll]
(nth coll 5)
)


(defn valid-price? [coll]
(try (and (every? number? coll )
(every? #(not (zero? %)) coll )
      (not (zero? (stats/sd coll)))
      )
(catch Exception e (print "nil or some issue, not prices") (identity false))
)
)

;(valid-price? [1 2 3 4 nil] )





(defn write-to-psv3
  "pipe separated values"
  [folder separator coll-of-coll]
  (let [
        filename (str folder (random-word))
        print-nil (println filename)
        ;data-writeable (conj coll-of-coll (str/join "|" header-coll))
        data-writeable (map #(str/join separator %)  coll-of-coll
                            )

        ]
    (do (spit (str filename ".csv")
              (str/join "\n"  data-writeable  )))
    )
  )
