(ns cddlj.core
  (:require
    [clojure.tools.cli :refer [parse-opts]]
    [mount.core :as mount]
    [cddlj.util :refer :all]
    [cddlj.config :refer [config]]
    [cddlj.ddl :refer [sql]]
    [cddlj.xls :refer [xls]]
    [cddlj.db :refer [diff]])
  (:gen-class))

(def cli-options
  [["-H" "--host DBHOST" "DB server"
    :default "localhost"]
   ["-p" "--port PORT" "Port number of the server"
    :default 3306
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   [nil "--db DBNAME" "Name of database"]
   ["-U" "--user USER" "Account for DB"]
   ["-P" "--pass PASSWORD" "Password for DB"]
   ["-h" "--help"]])

(defn- exit-with-usage
  [summary]
  (println "Usage:")
  (println "  cddlj [OPTS] COMMAND ARGS...")
  (println "")
  (println "COMMAND:")
  (println "    sql: Output sql file for DDL.")
  (println "    xls: Output excel document.")
  (println "   diff: Show the difference between schema(edn) and DB in html format.")
  (println "")
  (println "Example:")
  (println "  cddlj sql schema.edn out.sql")
  (println "  cddlj xls schema.edn out.xlsx")
  (println "  cddlj diff schema.edn out.html --db cddlj -U root -P mysql")
  (println "")
  (println " OPTS:")
  (println summary)
  (System/exit 0)
  )

(defn- invalid-args?
  [args]
  (or
    (not= 3 (count args))
    (not (#{"sql" "xls" "diff"} (first args)))))

(defn- invalid-opts?
  [[cmd] opts]
  false)

(defn -main
  ""
  [& args]
  (mount/start)
  (let [{:keys [options arguments summary]}
        (parse-opts args cli-options)]
    (if (or (:help options)
            (invalid-args? arguments)
            (invalid-opts? arguments options))
      (exit-with-usage summary)
      (let [[command & args] arguments]
        (case command
          "sql" (sql args options)
          "xls" (xls args options)
          "diff" (diff args options))))))

(comment
  (sql ["schema.edn" "out.sql"] {})
  (xls ["schema.edn" "out.xlsx"] {})
  (diff ["schema.edn" "out.html"]
        {:host "localhost"
         :db "cddlj"
         :port "3306"
         :user "root"
         :pass "mysql"
         })
  )

