(ns cddlj.util
  (:require
    [clojure.string :as s]))

(defn tap
  [e]
  (prn e)
  e)

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
  (->> toks
       (remove empty?)
       (s/join " ")))

(defn eol
  ([s] (eol s 1))
  ([s n]
   (apply str s (repeat n \newline))))

(defn join-lines
  [indent sepa lines]
  (->> lines
       (map #(str indent %))
       (clojure.string/join (eol sepa))))

(defn deep-merge-with [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))
