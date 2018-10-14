(ns cddlj.db
  (:require
    [korma.core :as korma]
    [korma.db :as db]
    [cddlj.util :refer :all]
    [cddlj.config :refer [config]]
    [cddlj.schema :as schema]
    [cddlj.ddl :refer [render-table]])
  (:import
    name.fraser.neil.plaintext.diff_match_patch
    com.mysql.jdbc.exceptions.jdbc4.MySQLSyntaxErrorException))

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

(defn diff
  [[edn-path out-file] {:keys [host db] :as opts}]
  (when-let [schs (schema/load-schemas edn-path)]
    (connect-db opts)
    (let [diffs (for [sch schs
                      :let [sql (render-table sch)
                            ddl (query-ddl sch)]]
                  (diff-html sql ddl))
          html (mk-html (str
                          (mk-left-header edn-path)
                          (mk-right-header (str db "@" host))
                          (clojure.string/join "<br/>" diffs)))]
      (spit out-file html))))




(comment
  (let [dmp (diff_match_patch.)
        diff (.diff_main dmp "Hello World" "Goobye World")
        _ (.diff_cleanupEfficiency dmp diff)
        html (.diff_prettyHtml dmp diff)]
    (spit "out.html" html))
  )
