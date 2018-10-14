(ns cddlj.db
  (:require
    [korma.core :as korma]
    [korma.db :as db]
    [hiccup.core :refer [html]]
    [cddlj.util :refer :all]
    [cddlj.config :refer [config]]
    [cddlj.schema :as schema]
    [cddlj.ddl :refer [render-table table-name-list]])
  (:import
    name.fraser.neil.plaintext.diff_match_patch
    com.mysql.jdbc.exceptions.jdbc4.MySQLSyntaxErrorException))

(def ^:private left-color "#ffe6e6")
(def ^:private right-color "#e6ffe6")

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

(defn- query-tables
  []
  (let [rs (korma/exec-raw "SHOW TABLES" :results)]
    (mapv (comp second first) rs)))
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
  (html [:html {:lang "ja"}
         [:head
          [:meta {:charset "utf-8"}]]
         [:body
          inner-body]]))
(defn- mk-header
  [caption bg-color]
  (html [:h3 {:style (str "background:" bg-color)} caption]))
(defn- mk-left-header
  [caption]
  (mk-header
    (str "エンティティ定義: " caption)
    left-color))
(defn- mk-right-header
  [caption]
  (mk-header
    (str "実際のDB: " caption)
    right-color))
(defn- mk-diff-row
  [tbl lefts rights]
  (let [border {:style "border:1px solid"}]
    (html [:tr
           [:th border
            [:a {:href (str "#" tbl)} tbl]]
           [:td border (when (lefts tbl) "○")]
           [:td border (when (rights tbl) "○")]])))
(defn- mk-diff-table
  [[all lefts rights]]
  (let [border "border:1px solid"]
    (html [:h1 "テーブル一覧"]
          [:table {:style "border-collapse:collapse"}
           [:tr
            [:th {:style border} "テーブル名"]
            [:th {:style (str border ";background:" left-color)} "エンティティ定義"]
            [:th {:style (str border ";background:" right-color)} "実際のDB"]]
           (for [tbl all]
             (mk-diff-row tbl lefts rights))])))
(defn- mk-diff-div
  [diffs]
  (html (for [[tbl diff] diffs]
          [:div
           [:h2 {:id tbl} tbl]
           [:div diff]])))

(defn- diff-tables
  [schs]
  (let [lefts (table-name-list schs)
        rights (query-tables)
        only-rights (->
                      (apply sorted-set rights)
                      (clojure.set/difference lefts))
        all (into lefts only-rights)]
    [all (set lefts) (set rights)]))

(defn diff
  [[edn-path out-file] {:keys [host db] :as opts}]
  (when-let [schs (schema/load-schemas edn-path)]
    (connect-db opts)
    (let [tbl-diff (diff-tables schs)
          diffs (for [sch schs
                      :let [tbl (name (:table sch))
                            sql (render-table sch)
                            ddl (query-ddl sch)]]
                  [tbl (diff-html sql ddl)])
          htm (mk-html (str
                         (mk-diff-table tbl-diff)
                         (html [:h1 "テーブル比較"])
                         (mk-left-header edn-path)
                         (mk-right-header (str db "@" host))
                         (mk-diff-div diffs)))]
      (spit out-file htm))))




(comment
  (let [dmp (diff_match_patch.)
        diff (.diff_main dmp "Hello World" "Goobye World")
        _ (.diff_cleanupEfficiency dmp diff)
        htm (.diff_prettyHtml dmp diff)]
    (spit "out.html" htm))
  (query-tables)
  )
