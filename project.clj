(defproject cddlj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.cli "0.4.1"]
                 [dk.ative/docjure "1.12.0"]
                 [expound "0.7.1"]]
  :main ^:skip-aot cddlj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
