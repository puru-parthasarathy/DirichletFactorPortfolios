(defproject geteoddata "0.1.0-SNAPSHOT"
  :description "to visualize stooq data"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jvm-opts ["-Xms2048m" "-Xmx4096m"]
  :dependencies [
                 [clj-http "3.12.1"]

                 [org.clojure/clojure "1.10.3"]

                 [incanter "1.5.3"]

                 ;[aerial.hanami "0.8.0"]

                 [clj-time "0.15.0"]
                 [proto-repl-charts "0.3.2"]
                 [cheshire "5.10.0"]
                 [helpshift/faker "0.2.0"]
                 ]



  











  :Plugins [
            ;[lein2-eclipse "2.0.0"]
            ;[lein-codox "0.9.4"]
            ;[lein-gorilla "0.4.0"]
            [lein-marginalia "0.9.1"]


            [lein-bin "0.3.4"]]


  :bin

  { :name "geteoddata"

   :bin-path "~/.local/bin"}
                                        ;:bootclasspath true


                                        ;:main gorilla-test.core
  :main geteoddata.tickers)
                                        ;  :main cointegrate.backtest
                                        ;:aot :all
                                        ;:profiles {:uberjar {:aot :all}}
