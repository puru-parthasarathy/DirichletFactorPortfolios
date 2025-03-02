(ns geteoddata.summary
  (:require
   [geteoddata.utils :as utils]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clj-http.client :as client]
   [cheshire.core :as cheshire]
   [clj-time.format :as tf]
   [clj-time.core :as tt]
   [incanter
    [core :as incanter]
    [stats :as stats]]
   [incanter.stats :as stats])
  (:use clojure.pprint)
  (:gen-class))


(def HOME (str (System/getProperty "user.dir")))
(def data-folder (str "/" (str/join "/" (rest (take 3 (str/split HOME #"/")))) "/data/"))
(pprint HOME)


;; if you are already in a spread position set this to false
;; for fresh opens set this to true
(def recompute-spread true)
(def previous-spread-info 
(read-string (slurp (str data-folder "spread-info"  ".dat")))
  )

(def p-low-previous (second previous-spread-info ) )
(def p-high-previous (second (reverse previous-spread-info )))
(identity p-low-previous)
(identity p-high-previous)


(def es
  (first
   (read-string
    (slurp  (str data-folder "close-data.dat")))))

;;
(def es (conj (vec es) 3985))



(identity
 (first
  (read-string
   (slurp  (str data-folder "price-data-header.dat")))))

(def daily-moves (utils/diff_ es))

(def duration 1)

(def price-moves
  (take-last 60
             (map #(reduce + %)  (partition duration 1 (utils/diff_ es)))))

(take-last 20 price-moves)

(def price-moves-long
  (take-last 200
             (map #(reduce + %)  (partition duration 1 (utils/diff_ es)))))



;; spread at 20th and 80th percentile
(def spreads
  (stats/quantile price-moves :probs  [0.1 0.9]))

(def p (last es))


;; reset only when not in position already
(when recompute-spread
  (def p-low (+ p (first spreads)))
  (def p-high (+ p (second spreads))))
(declare p-low-previous)
(declare p-high-previous)

(when (not recompute-spread)
  (def p-low p-low-previous)
  (def p-high p-high-previous))


(identity p)
(identity p-low)
(identity p-high)

;; conditional moves?

;; lets say price at 3650 tomorrow

;; then 
;; short exit?


(def short-move-distribution (filter #(<= % (first spreads))  price-moves-long))
(def breach-conditional-short-move (first (stats/quantile short-move-distribution  :probs  [0.75])))
;; short exit
(def short-exit (+ p breach-conditional-short-move))



(def long-move-distribution (filter #(>= % (second spreads))  price-moves-long))
(identity long-move-distribution)
(def breach-conditional-long-move (first (stats/quantile long-move-distribution  :probs  [0.25])))
;; short exit
(identity p)
(+ p breach-conditional-long-move)
(def long-exit (+ p breach-conditional-long-move))

[short-exit p-low p p-high long-exit]

;; defined by hand for extra control
;(def result [3615.0 3640.0 3720 3790 3810.00])
;(spit (str data-folder "spread-info"  ".dat") (pr-str result))


