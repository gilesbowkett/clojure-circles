(defproject circles "0.1.0-SNAPSHOT"
  :main circles.core
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [quil "2.0.0-SNAPSHOT"]
                 [overtone "0.9.1"]]
  :profiles {:dev {:plugins [[lein-kibit "0.0.8"]]}})

