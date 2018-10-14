(ns cddlj.xls
  (:require
    [dk.ative.docjure.spreadsheet :as x]
    [cddlj.util :refer :all]
    [cddlj.config :refer [config]]
    [cddlj.schema :as schema])
  (:import
    org.apache.poi.ss.util.CellReference))

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
  (let [[t l c] (schema/col-type t)]
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
      (for [sch schs
            :let [sch (schema/append-cols sch)
                  {:keys [table collation engine deleted? timestamp? columns]} sch]]
        (let [s (.cloneSheet wb (.getSheetIndex wb "table"))]
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

(defn xls
  [[edn-path out-file] opts]
  (when-let [schs (schema/load-schemas edn-path)]
    (xls-out schs
             out-file
             opts)) )



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

  )
