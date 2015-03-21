(defproject zanzel "0.3.2"
  :description "Some experiments on using lazy constructions to explore a search space"
  :url "http://www.obliquo.eu"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojars.achim/multiset "0.1.0"]
                 [org.clojars.pallix/analemma "1.0.0" :exclusions [org.clojure/clojure]]
                 [tikkba "0.5.0" :exclusions [org.clojure/clojure]]
                 [jfree/jfreechart "1.0.13"]
                 [org.jfree/jcommon "1.0.23"]])
