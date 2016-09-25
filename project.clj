(defproject genetic_algorithm "0.1.0-SNAPSHOT"
  :description "A Simple implementation for genetic algorithm using clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter "1.5.7"]
                 [com.taoensso/timbre "4.3.1"]]
  :main ^:skip-aot demo.demo_genetic_alg
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
