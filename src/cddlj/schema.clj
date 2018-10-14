(ns cddlj.schema
  (:require
    [clojure.spec.alpha :as s]
    [expound.alpha :refer [expound]]
    [cddlj.util :refer :all]
    [cddlj.config :refer [config]]))


(def ^:private default-collation :utf8_unicode_ci)

;;; --------------------------------------
;; spec
(s/def ::table keyword?)

(s/def ::collation
  #{:utf8_unicode_ci :utf8_general_ci})

(s/def ::name string?)
(s/def ::comment string?)

(s/def ::schema
  (s/keys :req-un [::table ::name ::collation]))

(s/def ::schemas
  (s/* ::schema))


;;; --------------------------------------
;; 

(defn- validate-schemas
  [schs]
  (if (s/valid? ::schemas schs)
    true
    (expound ::schema schs)))

(defn- apply-default
  [schs]
  (let [def-m {:deleted? true
               :timestamp? true}]
    (map #(deep-merge-with (fn [x y] y) def-m %)
         schs)))

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

(defn default-to-null-type?
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

(defn col-type
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

(defn load-schemas
  [edn-path]
  (let [schs (read-edn-all edn-path)]
    (when (validate-schemas schs)
      (apply-default schs))))

(defn append-cols
  [{:keys [table collation engine deleted? timestamp? columns] :as sch}]
  (let [columns (append-deleted deleted? columns)
        columns (append-timestamps timestamp? columns)]
    (assoc sch :columns columns)))



(comment
  (validate-schemas (read-edn-all "schema.edn"))
  )
