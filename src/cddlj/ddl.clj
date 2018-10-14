(ns cddlj.ddl
  (:require
    [cddlj.util :refer :all]
    [cddlj.schema :as schema]))

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
    (when (and (schema/default-to-null-type? t)
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
  (let [type-v (schema/col-type t)]
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

(defn render-table
  [{:keys [table collation engine deleted? timestamp? columns] :as sch}]
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
       (eol ";")))

(defn- render-table*
  [schs]
  (->> schs
       (map (comp render-table schema/append-cols))
       (join-lines "" "")))

(defn table-name-list
  [schs]
  (mapv (comp name :table) schs))

(defn sql
  [[edn-path out-file] opts]
  (when-let [schs (schema/load-schemas edn-path)]
    (->> schs
         render-table*
         (spit out-file))))

