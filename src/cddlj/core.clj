(ns cddlj.core
  (:require
    clojure.edn
    [clojure.spec.alpha :as s]
    [clojure.tools.cli :refer [parse-opts]]
    [dk.ative.docjure.spreadsheet :as x]
    [cddlj.spec :refer [validate-schemas]])
  (:gen-class))


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
  )

(defn- read-edn-all
  [edn-path]
  (with-open [in (-> edn-path
                     clojure.java.io/reader
                     (java.io.PushbackReader.))]
    (let [edn-seq (repeatedly #(clojure.edn/read {:eof :the-end} in))]
      (doall (take-while #(not= :the-end %) edn-seq)))))

(defn- render-engine
  [engine]
  "InnoDB")

(defn- custom-type?
  [k]
  (#{:date-str :datetime-str :time-str4 :time-str6} k))

(defn- custom-type
  [k]
  (let [l (k {:date-str 8
              :datetime-str 14
              :time-str4 4
              :time-str6 6})]
    ["VARCHAR" l]))

(defn- col-type-name
  [k]
  (clojure.string/upper-case (name k)))

(defn- col-type
  [k_or_v]
  (if (keyword? k_or_v)
    (if (custom-type? k_or_v)
      (custom-type k_or_v)
      [(col-type-name k_or_v) nil])
    (let [[t l c] k_or_v]
      [(col-type-name t) l c])))

(defn- render-type
  [k_or_v]
  (let [[t l c] (col-type k_or_v)]
    (str
      t
      (when l
        (str "(" l ")"))
      (when c
        (str " COLLATE " (name c))))))

(defn- render-table-comment
  [lnm cm]
  (str
    " COMMENT '"
    (if cm cm (str lnm "テーブル"))
    "'"))

(defn- col-comment
  [lnm cm]
  (if cm cm lnm))

(defn- render-col-comment
  [lnm cm]
  (str
    " COMMENT '"
    (col-comment lnm cm)
    "'"))

(defn- render-column
  [[nm {lnm :name t :type flags :flags cm :comment}]]
  (str
    (name nm)
    " "
    (render-type t)
    (render-col-comment lnm cm)))

(defn- join-lines
  [lines]
  (->> lines
   (map #(str "    " %))
   (clojure.string/join (str "," \newline))))

(defn- render-columns
  [col-seq]
  (let [cols (partition 2 col-seq)]
    (str
      (->> cols
           (map render-column)
           join-lines)
      \newline)))

(defn- sql-render
  [{:keys [table collation engine del-kbn? timestamp? columns] :as sch}]
  (str "CREATE TABLE "
       (name table)
       " (" \newline
       (render-columns columns)
       ")" \newline
       " ENGINE=" (render-engine engine)
       " DEFAULT COLLATE " (name collation)
       (render-table-comment (:name sch) (:comment sch))
       ";"
       \newline \newline))

(defn- sql
  [[_ edn-path out-file] opts]
  (let [schs (read-edn-all edn-path)]
    (when (validate-schemas schs)
      (->> schs
           (map sql-render)
           (apply str)
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
    (x/add-row! s [nil lnm (name nm) t l (col-comment lnm cm)])))

(defn- xls-out
  [schs out-file opts]
  (let [wb (x/load-workbook-from-resource "template.xlsx")]
    (dorun
      (for [{:keys [table collation engine del-kbn? timestamp? columns] :as sch} schs]
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
      (xls-out schs out-file opts))) )

(def cli-options
  ;; An option with a required argument
  [["-p" "--port PORT" "Port number"
    :default 80
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ;; A non-idempotent option (:default is applied first)
   ["-v" nil "Verbosity level"
    :id :verbosity
    :default 0
    :update-fn inc] ; Prior to 0.4.1, you would have to use:
                   ;; :assoc-fn (fn [m k _] (update-in m [k] inc))
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])

(defn- exit-with-usage
  [summary]
  (println "Usage:")
  (println "  cddlj [OPTS] ARGS...")
  (println "")
  (println "Example:")
  (println "  cddlj sql schema.edn out.sql")
  (println "  cddlj xls schema.edn out.xlsx")
  (println "")
  (println " OPTS:")
  (println summary)
  (System/exit 0)
  )

(defn- invalid-args?
  [args]
  (or
    (empty? args)
    (not (#{"sql" "xls"} (first args)))))

(defn -main
  ""
  [& args]
  (let [{:keys [options arguments summary]}
        (parse-opts args cli-options)]
    (if (or (:help options) (invalid-args? arguments))
      (exit-with-usage summary)
      (if (= (first arguments) "sql")
        (sql arguments options)
        (xls arguments options)))))
