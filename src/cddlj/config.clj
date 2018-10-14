(ns cddlj.config
  (:require
    [aero.core :refer [read-config root-resolver]]
    [mount.core :refer [defstate]]))

;; リソースのconfig.ednと
;; カレントディレクトリのproject.edn
(defstate config
          :start (read-config
                   (clojure.java.io/resource "config.edn")
                   {:resolver root-resolver}))
