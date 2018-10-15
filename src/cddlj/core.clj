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

  ;; 解析
  (require '[cddlj.schema :as schema])
  (let [schs (schema/load-schemas "schema.edn")
        cols (fn [{:keys [table columns]}]
               (map-vals #(assoc % :table-name table) (apply hash-map columns)))
        schs (map-vals count (group-by first (mapcat seq (map cols schs))))]
    schs)

  ;; ER diaglam
  (use 'tangle.core)
  (require '[clojure.java.io :as io])
  (let [table-1 {:id "tbl1"
                 :label [:TABLE {:border 0 :cellspacing 0}
                         [:TR [:TD {:border 0} "TABLE1"]]
                         [:TR [:TD {:border 1} "id"]]
                         [:TR [:TD {:border 1} "name"]]]}
        table-2 {:id "tbl2"
                 :label [:TABLE {:border 0 :cellspacing 0}
                         [:TR [:TD {:border 0} "TABLE2"]]
                         [:TR [:TD {:border 1} "name"]]
                         [:TR [:TD {:border 1 :port 2} "table1_id"]]]}
        table-3 {:id "tbl3"
                 :label [:TABLE {:border 0 :cellspacing 0}
                         [:TR [:TD {:border 0} "TABLE3"]]
                         [:TR [:TD {:border 1} "name"]]
                         [:TR [:TD {:border 1 :port 3} "table1_id"]]
                         [:TR [:TD {:border 1 :port 6} "table4_id"]]]}
        table-4 {:id "tbl4"
                 :label [:TABLE {:border 0 :cellspacing 0}
                         [:TR [:TD {:border 0} "TABLE4"]]
                         [:TR [:TD {:border 1} "name"]]
                         [:TR [:TD {:border 1 :port 4} "table1_id"]]
                         [:TR [:TD {:border 1 :port 7} "table2_id"]]
                         [:TR [:TD {:border 1 :port 5} "table3_id"]]]}
        dot (tap (graph->dot
              [table-1 table-2 table-3 table-4]
              [[:tbl2:2 :tbl1] [:tbl3:3 :tbl1]
               [:tbl4:4 :tbl1] [:tbl4:5 :tbl3]
               [:tbl3:6 :tbl4] [:tbl4:7 :tbl2]]
              {:graph {:rankdir :LR}
               :node {:shape :box}
               :node->id #(:id %)
               :node->descriptor identity
               :directed? true
               }))]
    (io/copy (dot->svg dot) (io/file "out.svg")))
  )

