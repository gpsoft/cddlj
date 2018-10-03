(ns cddlj.spec
  (:require
    [clojure.spec.alpha :as s]
    [expound.alpha :refer [expound]]
    [cddlj.util :refer :all]))

(s/def ::table keyword?)

(s/def ::collation
  #{:utf8_unicode_ci :utf8_general_ci})

(s/def ::name string?)
(s/def ::comment string?)

(s/def ::schema
  (s/keys :req-un [::table ::name ::collation]))

(s/def ::schemas
  (s/* ::schema))

(defn validate-schemas
  [schs]
  (if (s/valid? ::schemas schs)
    true
    (expound ::schema schs)))

(defn apply-default
  [schs]
  (let [def-m {:deleted? true
               :timestamp? true}]
    (map #(deep-merge-with (fn [x y] y) def-m %)
         schs)))
