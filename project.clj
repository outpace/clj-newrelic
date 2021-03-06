(defproject com.climate/clj-newrelic "0.2.2-SNAPSHOT"
  :description "Mark functions for newrelic tracing"
  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"
            :distribution :repo}
  :url "http://www.github.com/TheClimateCorporation/clj-newrelic"
  :plugins [[lein-ancient "0.6.2"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.newrelic.agent.java/newrelic-api "3.13.0"]])
