(ns cddlj.core
  (:require
    clojure.edn
    [clojure.spec.alpha :as s]
    [clojure.tools.cli :refer [parse-opts]]
    [dk.ative.docjure.spreadsheet :as x]
    [korma.core :as korma]
    [korma.db :as db]
    [aero.core :refer [read-config]]
    [cddlj.util :refer :all]
    [cddlj.spec :refer [validate-schemas apply-default]])
  (:import
    name.fraser.neil.plaintext.diff_match_patch)
  (:gen-class))

(def config (read-config (clojure.java.io/resource "config.edn")))

(comment
  (let [wb (x/create-workbook "Price List"
                              [["Name" "Price"]
                               ["Foo Widget" 100]
                               ["Bar Widget" 200]])
        sheet (x/select-sheet "Price List" wb)
        header-row (first (x/row-seq sheet))]
    (x/set-row-style! header-row (x/create-cell-style! wb {:background :yellow
                                                           :font {:bold true}}))
    (x/save-workbook! "test.xlsx" wb))
  (let [wb (x/load-workbook "test.xlsx")
        s (.cloneSheet wb (.getSheetIndex wb "Price List"))
        _ (.setSheetName wb (.getSheetIndex wb (.getSheetName s)) "hoge")
        cell (->> s
                  (x/select-cell "B3"))]
    (x/set-cell! cell "YES!")
    (.removeSheetAt wb (.getSheetIndex wb "Price List"))
    (x/save-workbook! "yes.xlsx" wb))
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader "schema.edn"))]
    (let [edn-seq (repeatedly #(clojure.edn/read {:eof :the-end} in))]
      (doall (take-while #(not= :the-end %) edn-seq))))
  (validate-schemas (read-edn-all "schema.edn"))
  (sql ["sql" "schema.edn" "out.sql"] {})
  (xls ["xls" "schema.edn" "out.xlsx"] {})
  (diff ["diff" "schema.edn" "diff.html"]
        {:host "localhost"
         :db "cddlj"
         :port "3306"
         :user "root"
         :pass "mysql"
         })
  (let [dmp (diff_match_patch.)
        diff (.diff_main dmp "Hello World" "Goobye World")
        _ (.diff_cleanupEfficiency dmp diff)
        html (.diff_prettyHtml dmp diff)]
    (spit "diff.html" html))
  )


(def ^:private default-collation :utf8_unicode_ci)

(defn- render-engine
  [engine]
  "InnoDB")

(defn- custom-col-type?
  [k]
  (let [ts #{:date-str
             :datetime-str
             :time-str4
             :time-str6}]
    (ts k)))

(defn- char-type?
  [k]
  (let [ts #{:char
             :varchar
             :text}]
    (ts k)))

(defn- default-to-null?
  [k]
  (let [ts #{:blob
             :tinyblob
             :text}]
    (not (ts k))))

(defn- charset-from-collation
  [c]
  (let [cs {:utf8_general_ci :utf8
            :utf8mb4_general_ci :utf8mb4}]
    (c cs)))

(defn- custom-col-type
  [k]
  (let [l (k {:date-str 8
              :datetime-str 14
              :time-str4 4
              :time-str6 6})]
    [:char l default-collation]))

(defn- col-type-name
  [k]
  (name k))

(defn- col-type
  [k_or_v]
  (let [v (if (keyword? k_or_v)
            (if (custom-col-type? k_or_v)
              (custom-col-type k_or_v)
              [k_or_v nil nil])
            k_or_v)]
    (let [[t l c] v]
      (if (char-type? t)
        [t l (or c default-collation)]
        v))))

(defn- render-charset-or-collation
  [c]
  (if-let [cset (charset-from-collation c)]
    (concat-toks "CHARACTER SET" (name cset))
    (concat-toks "COLLATE" (name c))))

(defn- render-type
  [[t l c]]
  (str
    (name t)
    (when l (wrap-par l))
    (when c (str " " (render-charset-or-collation c)))))

(defn- render-default
  [[t l c] fs d]
  (if d
    (concat-toks
      "DEFAULT"
      (if (string? d)
        (wrap-sq d)
        (str d)))
    (when (and (default-to-null? t)
               (:nullable? fs))
      "DEFAULT NULL")))

(defn- render-table-comment
  [lnm cm]
  (str
    "COMMENT="
    (wrap-sq (if cm cm (str lnm "テーブル")))))

(defn- col-comment
  [lnm cm]
  (if cm (str lnm (wrap-par cm)) lnm))

(defn- render-col-comment
  [lnm cm]
  (str
    "COMMENT "
    (wrap-sq (col-comment lnm cm))))

(defn- render-column
  [[nm {lnm :name t :type flags :flags default :default cm :comment}]]
  (let [type-v (col-type t)]
    (concat-toks
      (wrap-bt (name nm))
      (render-type type-v)
      (when-not (:nullable? flags) "NOT NULL")
      (render-default type-v flags default)
      (render-col-comment lnm cm))))

(defn- render-indices
  [cols]
  (let [pk (->> cols
                (filter (fn [[nm {flags :flags}]] (:pk? flags)))
                (map (comp wrap-bt name first))
                (clojure.string/join ",")
                wrap-par
                (concat-toks "PRIMARY KEY"))
        k nil]    ;; TODO: normal index key
    (filter some? [pk k])))

(defn- render-create-def
  [col-seq]
  (let [cols (partition 2 col-seq)
        col-lines (map render-column cols)
        key-lines (render-indices cols)]
    (->> (concat col-lines key-lines)
         (join-lines "    " ",")
         eol)))

(defn- append-deleted
  [del? col-seq]
  (if del?
    (concat col-seq (:col-deleted config))
    col-seq))

(defn- append-timestamps
  [ts? col-seq]
  (if ts?
    (concat col-seq (:cols-timestamp config))
    col-seq))

(defn- sql-render
  [{:keys [table collation engine deleted? timestamp? columns] :as sch}]
  (let [columns (append-deleted deleted? columns)
        columns (append-timestamps timestamp? columns)]
    (str (concat-toks
           "CREATE TABLE"
           (wrap-bt (name table))
           (eol "("))
         (render-create-def columns)
         (concat-toks
           ")"
           (str "ENGINE=" (render-engine engine))
           "DEFAULT CHARSET=utf8"
           (str "COLLATE=" (name collation))
           (str (render-table-comment (:name sch) (:comment sch))))
         (eol ";"))))

(defn- sql-render*
  [schs]
  (->> schs
       (map sql-render)
       (join-lines "" "")))

(defn- sql
  [[_ edn-path out-file] opts]
  (let [schs (read-edn-all edn-path)]
    (when (validate-schemas schs)
      (->> schs
           apply-default
           sql-render*
           (spit out-file)))))


(defn- out-val
  [s c r v]
  (-> s
      (.getRow r)
      (.createCell c)
      (x/set-cell! v)))

(defn- out-column
  [s [nm {lnm :name t :type flags :flags cm :comment}]]
  (let [[t l c] (col-type t)]
    (x/add-row! s [nil lnm (name nm) (name t) l (col-comment lnm cm)])))

(defn- xls-out
  [schs out-file opts]
  (let [wb (x/load-workbook-from-resource "template.xlsx")]
    (dorun
      (for [{:keys [table collation engine deleted? timestamp? columns] :as sch} schs]
        (let [s (.cloneSheet wb (.getSheetIndex wb "table"))]
          (.setSheetName wb (.getSheetIndex wb (.getSheetName s)) (:name sch))
          (out-val s 2 1 (:name sch))
          (out-val s 2 2 (name table))
          (let [cols (partition 2 columns)]
            (dorun
              (map #(out-column s %) cols)))
          )))
    (.removeSheetAt wb (.getSheetIndex wb "table"))
    (x/save-workbook! out-file wb)))

(defn- xls
  [[_ edn-path out-file] opts]
  (let [schs (read-edn-all edn-path)]
    (when (validate-schemas schs)
      (xls-out (apply-default schs)
               out-file
               opts))) )


(defn- diff-html
  [left right]
  (let [dmp (diff_match_patch.)
        diff (.diff_main dmp left right)
        _ (.diff_cleanupEfficiency dmp diff)]
    (.diff_prettyHtml dmp diff)))

(defn- diff-ddl
  [table]
  (let [rs (korma/exec-raw (str "SHOW CREATE TABLE " table) :results)]
    ((keyword "Create Table") (first rs))))

(defn- diff
  [[_ edn-path out-file] opts]
  (let [schs (read-edn-all edn-path)]
    (when (validate-schemas schs)
      (let [sql (sql-render* (apply-default schs))
            {:keys [user pass host port db]} opts
            connect-map {:user user
                         :password pass
                         :classname "com.mysql.jdbc.Driver"
                         :subprotocol "mysql"
                         :subname (str "//" host ":" port "/" db "?useSSL=false")}
            _ (db/defdb conn (db/mysql connect-map))
            ddl (->> schs
                     apply-default
                     (map (comp #(str % (eol ";")) diff-ddl name :table))
                     (join-lines "" ""))]
        (spit out-file
              (str "<html lang=\"ja\"><head><meta charset=\"utf-8\"></head><body>"
                   (diff-html sql ddl)
                   "</body></html>") )))))

(def cli-options
  ;; An option with a required argument
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
  (println "  cddlj --db cddlj -U root -P mysql diff schema.edn diff.html")
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
  (let [{:keys [options arguments summary]}
        (parse-opts args cli-options)]
    (if (or (:help options)
            (invalid-args? arguments)
            (invalid-opts? arguments options))
      (exit-with-usage summary)
      (case (first arguments)
        "sql" (sql arguments options)
        "xls" (xls arguments options)
        "diff" (diff arguments options)))))
