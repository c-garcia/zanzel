(defproject zanzel "0.2.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojars.achim/multiset "0.1.0-SNAPSHOT"]
                 [org.clojars.pallix/analemma "1.0.0" :exclusions [org.clojure/clojure]]
                 [tikkba "0.5.0" :exclusions [org.clojure/clojure]]
                 [incanter/incanter-core "1.5.6"]
                 [incanter/incanter-charts "1.5.6"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.7.0"]
                                  [clj-factory "0.2.1"]]}})
