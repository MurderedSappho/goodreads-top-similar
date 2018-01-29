(defproject goodreads "0.1.0-SNAPSHOT"
  :description "Books recommendation tool"
  :license {:name "Proprietary"}
  :jvm-opts ^:replace ["-server"
                       "-XX:+UseConcMarkSweepGC"
                       "-Xmx256m"
                       "-XX:-OmitStackTraceInFastThrow"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [aleph "0.4.4"]
                 [org.clojure/tools.cli "0.3.5"]
                 [clj-http "3.7.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]]
  :main ^:skip-aot goodreads.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
