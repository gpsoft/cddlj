(ns cddlj.core
  (:require
    clojure.edn
    [clojure.tools.cli :refer [parse-opts]]
    [dk.ative.docjure.spreadsheet :as x]
    [korma.core :as korma]
    [korma.db :as db]
    [mount.core :as mount]
    [cddlj.util :refer :all]
    [cddlj.config :refer [config]]
    [cddlj.spec :refer [validate-schemas apply-default]])
  (:import
    name.fraser.neil.plaintext.diff_match_patch
    org.apache.poi.ss.util.CellReference
    com.mysql.jdbc.exceptions.jdbc4.MySQLSyntaxErrorException)
  (:gen-class))


(def ^:private default-collation :utf8_unicode_ci)

;;; --------------------------------------
;; スキーマ解釈
(defn- custom-col-type?
  [k]
  (let [ts #{:date-str      ; "yyyymmdd"
             :datetime-str  ; "yyyymmddhhmmss"
             :time-str4     ; "hhmm"
             :time-str6}]   ; "hhmmss"
    (ts k)))

(defn- char-type?
  [k]
  (let [ts #{:char
             :varchar
             :text}]
    (ts k)))

(defn- default-to-null-type?
  [k]
  (let [ts #{:blob
             :tinyblob
             :text}]
    (not (ts k))))

(defn- custom-col-type
  "正規形へ"
  [k]
  (let [l (k {:date-str 8
              :datetime-str 14
              :time-str4 4
              :time-str6 6})]
    [:char l default-collation]))

(defn- col-type
  "正規形へ"
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


;;; --------------------------------------
;; DDLレンダリング
(defn- render-engine
  [engine]
  "InnoDB")

(defn- maybe-charset
  "collationよりcharsetを使う(かも)"
  [c]
  (let [cs {:utf8_general_ci :utf8
            :utf8mb4_general_ci :utf8mb4}]
    (c cs)))

(defn- render-charset-or-collation
  [c]
  (if-let [cset (maybe-charset c)]
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
    (when (and (default-to-null-type? t)
               (:nullable? fs))
      "DEFAULT NULL")))

(defn- render-table-comment
  [lnm cm]
  (str
    "COMMENT="
    (wrap-sq (if cm cm lnm))))

(defn- render-col-comment
  [lnm cm]
  (str
    "COMMENT "
    (wrap-sq (if cm cm lnm))))

(defn- render-column
  [[nm {lnm :name t :type flags :flags default :default cm :comment}]]
  (let [type-v (col-type t)]
    (concat-toks
      (wrap-bt (name nm))
      (render-type type-v)
      (when-not (:nullable? flags) "NOT NULL")
      (render-default type-v flags default)
      (render-col-comment lnm cm))))

(defn- render-key
  [pk? ix-nm cols]
  (let [cs (map (comp wrap-bt name) cols)]
    (concat-toks
      (when pk? "PRIMARY")
      "KEY"
      (when ix-nm (wrap-bt ix-nm))
      (wrap-par (clojure.string/join "," cs)))))

(defn- render-index
  [[ix-nm vs]]
  (render-key
    false
    ix-nm
    (map second vs)))

(defn- render-indices
  [cols]
  (let [pk (->> cols
                (filter (fn [[_ {flags :flags}]] (:pk? flags)))
                (map first)
                (render-key true nil))
        ks (->> cols
                (map (fn [[nm {index :index}]] (when index [index nm])))
                (filter some?)
                (group-by first)
                (map render-index))]
    (filter some? (conj ks pk))))

(defn- render-create-def
  [col-seq]
  (let [cols (partition 2 col-seq)
        col-lines (map render-column cols)
        key-lines (render-indices cols)]
    (->> (concat col-lines key-lines)
         (join-lines "    " ",")
         eol)))

(defn- render-table
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

(defn- render-table*
  [schs]
  (->> schs
       (map render-table)
       (join-lines "" "")))

(defn- sql
  [[_ edn-path out-file] opts]
  (let [schs (read-edn-all edn-path)]
    (when (validate-schemas schs)
      (->> schs
           apply-default
           render-table*
           (spit out-file)))))


;;; --------------------------------------
;; テーブル定義書(Excel)出力
(defn- coerce-cell-value
  [v]
  (cond
    (nil? v) ""
    (number? v) (if (= v (double (int v))) (int v) v)
    :else v))

(defn- read-cell
  [sh addr]
  (let [cl (x/select-cell addr sh)
        v (x/read-cell cl)]
    (coerce-cell-value v)))

(defn- select-columns
  [sh m]
  (let [es (x/select-columns m sh)]
    (mapv #(map-vals coerce-cell-value %) es)))

(defn- get-or-create-row
  [sh r]
  (let [row (.getRow sh r)]
    (if (nil? row)
      (.createRow sh r)
      row)))

(defn- out-val
  [s c r v]
  (let [c (if (number? c)
            c
            (CellReference/convertColStringToIndex c))]
    (-> s
        (get-or-create-row r)
        (.createCell c)
        (x/set-cell! v))))

(defn- col-map-to-values
  [m]
  (for [c (range 100)
        :let [cname (keyword (CellReference/convertNumToColString c))]]
    (cname m)))

(defn- out-column
  [s ix [nm {lnm :name t :type flags :flags default :default cm :comment}]]
  (let [[t l c] (col-type t)]
    (x/add-row! s (col-map-to-values
                    {:A (inc ix)
                     :C lnm
                     :N (name nm)
                     :Y (when (:pk? flags) "○")
                     :AH (when-not (:nullable? flags) "○")
                     :AU (name t)
                     :AY l
                     :BC default
                     :BH cm}))))

(defn- xls-out
  [schs out-file opts]
  (let [wb (x/load-workbook-from-resource "template.xlsx")
        {{project-name :name project-code :code} :project} config]
    (dorun
      (for [{:keys [table collation engine deleted? timestamp? columns] :as sch} schs]
        (let [s (.cloneSheet wb (.getSheetIndex wb "table"))
              columns (append-deleted deleted? columns)
              columns (append-timestamps timestamp? columns)]
          (.setSheetName wb (.getSheetIndex wb (.getSheetName s)) (:name sch))
          ;; A B C D E
          (out-val s "A" 0 (str project-name " エンティティ設計"))
          (out-val s "AG" 0 (name project-code))
          (out-val s "AG" 1 (name project-name))
          (out-val s "N" 3 (name table))
          (out-val s "AQ" 3 (:name sch))
          (out-val s "N" 4 (:comment sch))
          (let [cols (partition 2 columns)]
            (dorun
              (map-indexed #(out-column s %1 %2) cols)))
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


;;; --------------------------------------
;; DB比較
(defn- diff-html
  [left right]
  (let [dmp (diff_match_patch.)
        diff (.diff_main dmp left right)
        _ (.diff_cleanupEfficiency dmp diff)]
    (.diff_prettyHtml dmp diff)))

(defn- connect-db
  [{:keys [user pass host port db]}]
  (let [connect-map {:user user
                     :password pass
                     :classname "com.mysql.jdbc.Driver"
                     :subprotocol "mysql"
                     :subname (str "//" host ":" port "/" db "?useSSL=false")}]
    (db/defdb conn (db/mysql connect-map))))

(defn- query-ddl*
  [table]
  (try
    (let [rs (korma/exec-raw (str "SHOW CREATE TABLE " table) :results)]
      ((keyword "Create Table") (first rs)))
    (catch MySQLSyntaxErrorException ex
      "The table doesn't seem to exist")))

(defn- query-ddl
  [sch]
  (-> sch
      :table
      name
      query-ddl*
      (str (eol ";"))))

(defn- mk-html
  [inner-body]
  (str "<html lang=\"ja\"><head><meta charset=\"utf-8\"></head><body>"
       inner-body
       "</body></html>"))
(defn- mk-left-header
  [caption]
  (str "<h3 style=\"background:#ffe6e6;\">エンティティ定義: " caption "</h3>"))
(defn- mk-right-header
  [caption]
  (str "<h3 style=\"background:#e6ffe6;\">実際のDB: " caption "</h3>"))

(defn- diff
  [[_ edn-path out-file] {:keys [host db] :as opts}]
  (let [schs (read-edn-all edn-path)]
    (when (validate-schemas schs)
      (connect-db opts)
      (let [diffs (for [sch (apply-default schs)
                        :let [sql (render-table sch)
                              ddl (query-ddl sch)]]
                    (diff-html sql ddl))
            html (mk-html (str
                            (mk-left-header edn-path)
                            (mk-right-header (str db "@" host))
                            (clojure.string/join "<br/>" diffs)))]
        (spit out-file html)))))


;;; --------------------------------------
;; メイン
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
  (mount/start)
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
  (let [wb (x/load-workbook-from-resource "template.xlsx")
        sh (x/select-sheet "table" wb)]
    #_(read-cell sh "C2")
    (select-columns sh {:B :hoge :C :fuga}))
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


