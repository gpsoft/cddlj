(defproject cddlj "0.1"
  :description "Just another private tool."
  :url "https://github.com/gpsoft/cddlj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.cli "0.4.1"]
                 [dk.ative/docjure "1.12.0"]
                 [expound "0.7.1"]
                 [korma "0.4.3"]
                 [mysql/mysql-connector-java "5.1.47"]
                 [ch.qos.logback/logback-classic "1.2.3"]]
  :java-source-paths ["javasrc"]
  :main ^:skip-aot cddlj.core
  :target-path "target/%s"
  ; :jvm-opts ["-Djdbc.drivers=com.mysql.jdbc.Driver"]
  :profiles {:uberjar {:aot :all}})
