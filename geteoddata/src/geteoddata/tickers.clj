; this file is used to get nasdaq100 data
(ns geteoddata.tickers
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
    [stats :as stats]])
  (:use clojure.pprint)
  (:gen-class))


(def HOME (str (System/getProperty "user.dir")))
(def data-folder (str "/" (str/join "/" (rest (take 3 (str/split HOME #"/")))) "/data/"))
(pprint HOME)
(def api-token "5ffde981a31603.50022638")

;; get nasdaq100 tickers from a flat file
(def nasdaq100 (str/split-lines  (slurp (str HOME "/nasdaq100Tickers20230606.txt"))))

 


(def trading-dates (map second (map first (cheshire/parse-string (:body
                                                                  (client/get (str
                                                                               "https://eodhistoricaldata.com/api/eod/AAPL.US?from=2001-01-01&to=2030-01-01&api_token=" api-token "&period=d&fmt=json")))))))
(defn trading-dates-after
  "starting from a dates, what dates of data are there"
  [yyyy-mm-dd]
  (map second (map first (cheshire/parse-string (:body (client/get
                                                        (str "https://eodhistoricaldata.com/api/eod/AAPL.US?from=" yyyy-mm-dd "&api_token=OeAFFmMliFG5orCUuwAKQ8l4WWFQ67YX&period=d&fmt=json")))))))
(def dummy-dates (zipmap (map keyword (take-last 10 (trading-dates-after "1980-01-01"))) (repeat Double/NaN)))

(defn eod-data-to-memo
  " this is api calls efficient, returns unordered hashmap - not now this queries starting 2001 so is expected to be slow"
  ([& {:keys [exchange ticker]}]
   (let [from-date   "2001-01-01"
         data        (cheshire/parse-string (:body (client/get (format (str "https://eodhistoricaldata.com/api/eod/" ticker "." exchange "?from=%s&api_token=" api-token "&period=d&fmt=json") from-date))))
         dates       (map keyword (map #(% "date") data))
         F           (fn [key] (map #(cond (nil?  (% key))
                                           nil
                                           :else
                                           (cond (zero? (% key)) nil :else (% key))) data))

         [o h l c v] (map F ["open" "high" "low" "adjusted_close" "volume"])]

     (zipmap dates (map vector o h l c v)))))


(def eod-data (memoize eod-data-to-memo))

(defn eod-data-adapted
  "returns eod data map fills in missing trading dates with NaN"
  ([ticker]
   (let [dummy-dates (zipmap (map keyword (trading-dates-after "2000-01-01")) (repeat nil))]

     (merge dummy-dates (eod-data :exchange "US" :ticker ticker)))))

(def custom-formatter (tf/formatter "yyyy-MM-dd"))

(defn final-date [ticker]
  (name (last (map first (sort-by
                          #(tf/parse custom-formatter (name (first %)))
                          (eod-data-adapted ticker))))))

(def data-available-till (final-date "SPY"))
(pprint "Last print on market data...")
(pprint data-available-till)

(def eod-data-final (memoize eod-data-adapted))

(defn eod-data-final-dates
  "a map of dates and prices, in order left to right"
  [ticker]
  (map name (map first (sort-by
                        #(tf/parse custom-formatter (name (first %)))
                        (eod-data-adapted ticker)))))

;; This is what is used in the final call
(defn eod-data-tickers
  "on ticker group, adapted"
  ([ticker-coll]
   (map eod-data-final
        ticker-coll)))

(defn date-price-map->ordered-prices-deprecated
  "use this if you want to back and front fill prices. this is not a good idea for the backtest"
  [key-string coll]
  (let [java-dates (map #(tf/parse custom-formatter (name (first %)))
                        coll)
        technical-data (map second (sort-by first (map vector java-dates (map second coll))))
        output (condp = key-string
                 "open" (utils/locf-clean2 (map first technical-data))
                 "high" (utils/locf-clean2 (map second  technical-data))
                 "low" (utils/locf-clean2 (map utils/third  technical-data))
                 "close" (utils/locf-clean2 (map utils/fourth  technical-data))
                 "volume" (utils/locf-clean2 (map utils/fifth  technical-data)))]
    output))

(defn date-price-map->ordered-prices
  "use this if you want to back and front fill prices. this is not a good idea for the backtest"
  [key-string coll]
  (let [java-dates (map #(tf/parse custom-formatter (name (first %)))
                        coll)
        technical-data (map second (sort-by first (map vector java-dates (map second coll))))
        output (condp = key-string
                 "open" (map first technical-data)
                 "high" (map second  technical-data)
                 "low" (map utils/third  technical-data)
                 "close" (map utils/fourth  technical-data)
                 "volume" (map utils/fifth  technical-data))]
    output))

(defn date-price-map->ordered-dates [coll]
  (let [dates (map #(name (first %))
                   coll)
        java-dates (map #(tf/parse custom-formatter (name (first %)))
                        coll)
        output (sort-by first (map vector java-dates dates (map second coll)))]
    java-dates

    (map second output)))

(defn eod-data-length
  "returns eod data map fills in missing trading dates with NaN"
  [ticker]
  (let [dates   (map first (sort-by
                            #(tf/parse custom-formatter (name (first %)))
                            (eod-data ticker)))]

    [(first dates) (count dates)]))


(defn clean-date-price-map
  [date-price-map]
  (let [;date-price-map (first opens-updated)
        ordered-o (date-price-map->ordered-prices "open" date-price-map)
        ordered-h (date-price-map->ordered-prices "high" date-price-map)
        ordered-l (date-price-map->ordered-prices "low" date-price-map)
        ordered-c (date-price-map->ordered-prices "close" date-price-map)
        ordered-v (date-price-map->ordered-prices "volume" date-price-map)

          ;;keep track of possibility that leading nans are of different length
        ordered-o-clean ordered-o
        ordered-h-clean ordered-h
        ordered-l-clean ordered-l
        ordered-c-clean ordered-c
        ordered-v-clean ordered-v

        these-many (apply min (map count [ordered-o-clean
                                          ordered-h-clean
                                          ordered-l-clean
                                          ordered-c-clean
                                          ordered-v-clean]))
        ordered-dates (map keyword (take-last these-many (date-price-map->ordered-dates date-price-map)))




        clean-map (into {} (zipmap ordered-dates
                                   (map vector
                                        (take-last these-many ordered-o-clean)
                                        (take-last these-many ordered-h-clean)
                                        (take-last these-many ordered-l-clean)
                                        (take-last these-many ordered-c-clean)
                                        (take-last these-many ordered-v-clean))))]
    clean-map))


(def calculate-trading-universe? true)
;; legit us stocks
(when
 calculate-trading-universe?

  (def exchange-traded-0 (cheshire/parse-string (:body (client/get (format
                                                                    "https://eodhistoricaldata.com/api/exchanges/US?api_token=%s&fmt=json" api-token)))))

  (def permitted-exchanges '("BATS" "NYSE" "AMEX" "NASDAQ" "NYSE ARCA" "NYSE MKT" "IEX"))
  (def types '("Common Stock"))
  (take 3 exchange-traded-0)

  (def exchange-traded (filter #(> 5 (count (% "Code"))) exchange-traded-0))
  (def exchange-traded-clean (filter #(and (not (nil? (% "Name"))) (contains? (set types) (% "Type"))
                                           (contains? (set permitted-exchanges) (% "Exchange"))) exchange-traded))


  (count exchange-traded-clean)
  (count exchange-traded)

  (def exchange-traded-etfs (filter #(and (contains? (set permitted-exchanges) (% "Exchange"))
                                          (= "ETF" (% "Type"))) exchange-traded-0))

  (def etfs (map #(% "Code") exchange-traded-etfs))

  (def funds (remove nil? (map #(re-matches
                                 #".*(?i)Fund.*|.*(?i)Yield.*|.*\sNT\s.*|.*(?i)Debenture.*|.*\s(?i)Note\s.*|.*\s(?i)Equity\s.*"  (% "Name")) exchange-traded-clean)))

  (def exchange-traded-cleaner (filter #(not (contains? (set funds) (% "Name")))  exchange-traded-clean))

  (def e-traded (map #(% "Code") exchange-traded-cleaner))
  (count e-traded)

  (def lookup (zipmap (map keyword e-traded) exchange-traded-cleaner))

;; do a bulk last day call to which guys are trading in the list above
  (def latest-trading-data (cheshire/parse-string (:body (client/get
                                                          (str
                                                           "https://eodhistoricaldata.com/api/eod-bulk-last-day/US?api_token="
                                                           api-token "&fmt=json&date=" (last (drop-last trading-dates)))))))

  (def relevant-trading-data-exchange-traded-1  (filter #(contains? (set e-traded) (% "code")) latest-trading-data))

  (def prices (map #(% "low") relevant-trading-data-exchange-traded-1))

  (def names (map #(% "code") relevant-trading-data-exchange-traded-1))

;; tickers that have market cap higher than 300 and price greater than 10
  (def permissable-names (map first (sort-by second (filter #(and (>= 300.0 (second %)) (< 10.0 (second %)))  (map vector names prices)))))
  (take 10 permissable-names)
  (lookup :NTES)

  (def relevant-trading-data  (filter #(contains? (set permissable-names) (% "code")) relevant-trading-data-exchange-traded-1))

  (take 2 relevant-trading-data)
  (def daily-range (map (fn [x] (/ (- (x "high") (x "low")) (x "low"))) relevant-trading-data))

  (def daily-adv (map (fn [x] (/ (* (x "adjusted_close") (x "volume")) 1000000)) relevant-trading-data))

  (def daily-names (map (fn [x] (x "code")) relevant-trading-data))

  (count daily-range)
  (def range-score (utils/zscore  daily-range))
  (def adv-score (utils/zscore  daily-adv))
  (def bottom-quartile (utils/round0 (/ (count daily-names) 4.0)))
  (def good-names-1 (map first (drop (* 1 bottom-quartile) (sort-by second (map vector daily-names adv-score)))))
  (def good-names-2 (map first (drop (* 1 bottom-quartile) (sort-by second (map vector daily-names range-score)))))
  (def trading-universe (into [] (clojure.set/intersection (set good-names-1) (set good-names-2))))

  (count trading-universe)
  (pprint "trading universe...count")
  (pprint (count trading-universe))
  (pprint "cmd no-of-stocks min-timeseries-length-required-to-be-eligible ")

  (def opens-market (eod-data-tickers  '("SPY")))


  (def most-recent-date-now (last
                             (date-price-map->ordered-dates (first opens-market))))

  (pprint "Fetching data...will take a while...")
  (def stale-data? true)
  (def N-stocks 5000)
  (def trading-universe (vec (take N-stocks (sort trading-universe)))))


;; overwrite 
(def opens-market (eod-data-tickers  '("SPY")))

;(def test (eod-data-tickers  '("SPY")))

(def trading-universe  (take 500 nasdaq100))
(pprint "trading universe")
(pprint "trading universe...count")
(pprint (count trading-universe))

;(identity opens-market) 
(def most-recent-date-now (last (date-price-map->ordered-dates (first opens-market))))
(pprint most-recent-date-now)
;; if download was not full, merge the two 





(defn exec-1 []
(pprint "***************Calling exec-1 ***********************")
  (def opens-updated
    (eod-data-tickers (take 500 trading-universe)))

  (take 10 trading-universe)
  (count opens-updated)

;; apply locf on ordered prices, remove leading nils and recover
;; more parsimonious map

  #_{:clj-kondo/ignore [:inline-def]}
  (def opens-updated-clean
    (map clean-date-price-map opens-updated))

  (def open-a
    (map (partial date-price-map->ordered-prices "open") opens-updated-clean))

;;force the work to be done here
  ;(pprint (map #(take-last 5 %) open-a))

  (def high-a
    (map (partial date-price-map->ordered-prices "high")
         opens-updated-clean))


  #_{:clj-kondo/ignore [:inline-def]}
  (def low-a (map (partial date-price-map->ordered-prices "low") opens-updated-clean))

  (def close-a (map (partial date-price-map->ordered-prices "close") opens-updated-clean))


  (def volume-a (map (partial date-price-map->ordered-prices "volume") opens-updated-clean))

  (def dates-a  (date-price-map->ordered-dates (first opens-updated-clean)))


  (def T 100)
;;remove very small histories

;;weve made sure all histories for a stock are same, so ok to filter only 
;; on open
  (def trading-universe-clean (map first (filter #(<= T (count (second %))) (map vector trading-universe open-a))))
  (pprint "After filter on history*************************************")
  (pprint (count trading-universe-clean))

  (def open-b (map second (filter #(<= T (count (second %))) (map vector trading-universe open-a))))

  (def high-b (map second (filter #(<= T (count (second %))) (map vector trading-universe high-a))))

  (def low-b (map second (filter #(<= T (count (second %))) (map vector trading-universe low-a))))

  (def close-b (map second (filter #(<= T (count (second %))) (map vector trading-universe close-a))))

  (def volume-b (map second (filter #(<= T (count (second %))) (map vector trading-universe volume-a))))



  (def smallest-timeseries-length (apply min (map count open-b)))
  (pprint smallest-timeseries-length)
  (def open (map #(vec (take-last smallest-timeseries-length %)) open-b))
  (def high (map #(vec (take-last smallest-timeseries-length %)) high-b))
  (def low (map #(vec (take-last smallest-timeseries-length %)) low-b))
  (def close (map #(vec (take-last smallest-timeseries-length %)) close-b))
  (def volume (map #(vec (take-last smallest-timeseries-length %)) volume-b))

  ;(pprint volume)

  (def dates (take-last smallest-timeseries-length dates-a))
  (count trading-universe-clean)


  (def starting-points (sort-by second (zipmap trading-universe-clean (map utils/first-non-nil-index open))))
  (def max-history (apply max (map count open)))
  (pprint max-history)
  (def days-of-history (zipmap (map first starting-points)  (map #(- max-history (second %)) starting-points)))
  (pprint days-of-history)
  (identity days-of-history)
)

;(def result (exec-1))

;; examine here
;(pprint (sort-by second result))








(defn exec-2 []
(pprint "***************Calling exec-2...***********************")
 
  ;; 4000 trading days is 18+ years, so a decent cut of the price data
  (def MIN-TRADING-DAYS 4000 )
  (pprint "MIN Trading Trading used ****************************************************")
  (pprint MIN-TRADING-DAYS)
  (def days-of-history-filtered (filter #(<= MIN-TRADING-DAYS (second %)) days-of-history))

  (pprint days-of-history-filtered)

  (def tmp (map vector trading-universe-clean (map #(take-last MIN-TRADING-DAYS %)  open)
                (map #(take-last MIN-TRADING-DAYS %) high)
                (map #(take-last MIN-TRADING-DAYS %) low)
                (map #(take-last MIN-TRADING-DAYS %) close)
                (map #(take-last MIN-TRADING-DAYS %) volume)))
  (count tmp)
  (def tmp-filtered (filter #(contains? (set (map first days-of-history-filtered)) (first %)) tmp))

  (def trading-universe-clean (map first tmp-filtered))
  (def open (map second tmp-filtered))
  (def high (map utils/third tmp-filtered))
  (def low (map utils/fourth tmp-filtered))
  (def close (map utils/fifth tmp-filtered))
  (def volume (map utils/sixth tmp-filtered))

  ;(pprint trading-universe-clean)

  (pprint "Count of assets before final write...")
  (pprint (count trading-universe-clean))


  (pprint "writing price data here...")

  (def data-folder (str "/" (str/join "/" (rest (take 3 (str/split HOME #"/")))) "/data/"))

  (def token (utils/random-word))
  (pprint (map #(take-last 5 %) volume))

  (spit (str data-folder "open-data"   ".dat") (pr-str open))
  (spit (str data-folder "high-data"   ".dat") (pr-str high))
  (spit (str data-folder "low-data"   ".dat") (pr-str low))
  (spit (str data-folder "close-data"   ".dat") (pr-str close))
  (spit (str data-folder "volume-data"  ".dat") (pr-str volume))

  (spit (str data-folder "dates-data"    ".dat") (pr-str dates))

  (spit (str data-folder "price-data-header"  ".dat") (pr-str (vec trading-universe-clean)))
  (pprint "writing ticker data here...")
  (pprint "these many tickers...")

  (def ticker-info (zipmap (map keyword trading-universe-clean)  (map #(% lookup) (map keyword trading-universe-clean))))
  (spit (str data-folder "ticker-info"  ".dat") (pr-str ticker-info))
  (pprint (count ticker-info))

  (def test-prices (zipmap (map keyword trading-universe-clean) close))
  (def random-ticker (first (shuffle trading-universe-clean)))
  (pprint random-ticker)
  (pprint (lookup (keyword random-ticker)))
  (pprint (take 10 (test-prices (keyword random-ticker))))

  (pprint (take-last 10 (test-prices (keyword random-ticker))))

  (spit "test.data" (pr-str random-ticker))
  (spit "test.data" (pr-str (lookup (keyword random-ticker))) :append true)
  (spit "test.data" (pr-str (take-last 10 (test-prices (keyword random-ticker)))) :append true)


  (pprint "Finished without errors...")
  (pprint "price token...")
  (pprint "Writing csv data for prices and dates data that is R readable ==> This feeds into an R script for data preparation.")
  (utils/write-to-psv3 data-folder "," close)
  (utils/write-to-psv3 data-folder "," [dates])
  (utils/write-to-psv3 data-folder "," [trading-universe-clean])


  (pprint "trading universe...count"))

;(exec-2)
