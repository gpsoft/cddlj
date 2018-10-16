(ns cddlj.erd
  (:require
    [tangle.core :as tangle]
    [clojure.java.io :as io]
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

(defn- col-v
  [ix [col {nm :name flags :flags}]]
  [:tr [:td {:border 1 :bgcolor (if (:pk? flags) :azure :transparent) :port (name col) #_(inc ix)} (str (name col) (wrap-par nm))]])

(defn- table-v
  [{:keys [table columns] :as sch}]
  (-> [:table {:border 0 :cellspacing 0}]
      (into [[:tr [:td {:border 0} (name table)]]])
      (into (map-indexed col-v (partition 2 columns)))))

(defn- edge-v
  [table [col {join :join}]]
  [(keyword (str (name table) (str col))) join])

(defn- table-node
  [{table :table :as sch}]
  (let [v (table-v sch)]
    {:id (name table) :label v}))

(defn- join-edges
  [{:keys [table columns]}]
  (->> (partition 2 columns)
       (filter #(:join (second %)))
       (map #(edge-v table %))))

(defn erd
  [[edn-path out-file] opts]
  (when-let [schs (schema/load-schemas edn-path)]
    (let [nodes (map (comp table-node schema/append-cols) schs)
          edges (mapcat join-edges schs)
          dot (tangle/graph->dot
                nodes
                edges
                {:graph {:rankdir :LR}
                 :node {:shape :box}
                 :node->id #(:id %)
                 :node->descriptor identity
                 :directed? true})]
      (io/copy (tangle/dot->svg dot) (io/file out-file))
      (shutdown-agents))))





(comment
  (erd ["schema.edn" "out.svg"] {})
  )
