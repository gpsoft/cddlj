(ns cddlj.util
  (:require
    [clojure.string :as s]))

(defn read-edn-all
  [edn-path]
  (with-open [in (-> edn-path
                     clojure.java.io/reader
                     (java.io.PushbackReader.))]
    (let [edn-seq (repeatedly #(clojure.edn/read {:eof :the-end} in))]
      (doall (take-while #(not= :the-end %) edn-seq)))))

(defn wrap-par
  [s]
  (str "(" s ")"))

(defn wrap-sq
  [s]
  (str "'" s "'"))

(defn wrap-bt
  [s]
  (str "`" s "`"))

(defn concat-toks
  [& toks]
  (s/join " " toks))

(defn crlf
  ([s] (crlf s 1))
  ([s n]
   (apply str s (repeat n "\n"))))

(defn join-lines
  [indent sepa lines]
  (->> lines
       (map #(str indent %))
       (clojure.string/join (crlf sepa))))
