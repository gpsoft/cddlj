# cddlj

ednに記述したテーブル定義情報に基づいて、DDL用のSQLファイルやテーブル定義書(Excel)を自動生成する試み。

MySQL専用。


# テーブル定義

`schema.edn`参照。

```clojure
{
 ;; テーブル名
 :table :t_lesson

 ;; テーブルの論理名
 :name "レッスン"

 ;; テーブルのcollation
 :collation :utf8_unicode_ci

 ;; 各カラムの仕様
 :columns [
           ;; カラム名と属性
           ;; 属性に指定できるのは:
           ;;   :name       カラムの論理名
           ;;   :type       [型 桁数 (collation)]
           ;;               または、型のみ
           ;; 以下、省略可能:
           ;;   :flags      :pk?, :seq?, nullable?
           ;;   :default    デフォルト値
           ;;   :index      インデックスにするなら、その名前
           ;;   :comment    カラムのコメント
           :lesson_id {:name "レッスンID"
                       :type [:int 11]
                       :flags #{:pk? :seq?}
                       }
           :lesson_nm {:name "レッスン名"
                       :type [:varchar 20]}
           :sta_date {:name "開始日"
                      :type :date-str}
           :end_date {:name "終了日"
                      :type :date-str}
           :lesson_fee {:name "料金"
                        :type :int
                        :flags #{:nullable?}}
           :deadline {:name "申込締切日時"
                      :type :datetime-str}
           ]

 ;; 以下、省略可能:
 ;; :engine      :innodbのみサポート
 ;; :deleted?    論理削除用のカラムが必要か? デフォルトはtrue
 ;; :timestamp?  タイムスタンプ用のカラムが必要か? デフォルトはtrue
 ;; :comment     テーブルのコメント
 }


{:table :m_teacher
 :name "講師マスター"
 :collation :utf8_unicode_ci
 :engine :innodb
 :deleted? false
 :timestamp? false
 :comment "暫定バージョンです。"
 :columns [:teacher_no {:name "講師No"
                        :type [:int 11]
                        :flags #{:pk? :seq?}
                        }
           :teacher_nm {:name "講師名"
                        :type :blob
                        :flags #{:nullable?}
                        :comment "暗号化して保存"
                        }
           :gr_nm {:name "所属名"
                   :type [:varchar 10 :utf8_general_ci]
                   :default ""
                   :index "idx1"
                   }
           :birthdate {:name "生年月日"
                       :type :date-str
                       :flags #{:nullable?}}
           ]
 }
```

# Usage

```
Usage:
  cddlj [OPTS] COMMAND ARGS...

COMMAND:
    sql: Output sql file for DDL.
    xls: Output excel document.
   diff: Show the difference between schema(edn) and DB in html format.

Example:
  cddlj sql schema.edn out.sql
  cddlj xls schema.edn out.xlsx
  cddlj --db cddlj -U root -P mysql diff schema.edn diff.html

 OPTS:
  -H, --host DBHOST    localhost  DB server
  -p, --port PORT      3306       Port number of the server
      --db DBNAME                 Name of database
  -U, --user USER                 Account for DB
  -P, --pass PASSWORD             Password for DB
  -h, --help
```

# 開発

`config-sample.edn`を参考にして`config.edn`を作り、`resources/`に置く。

```clojure
{
 ;; プロジェクト属性
 :project #include "./project.edn"

 ;; 論理削除用のカラム
 :col-deleted [:deleted {:name "削除フラグ"
                         :type :int
                         :default 0}]

 ;; タイムスタンプ用のカラム群
 :cols-timestamp [:created_at {:name "作成日時"
                               :type :datetime}
                  :modifed_at {:name "更新日時"
                               :type :datetime}]}
```

`project.edn`を編集。

```clojure
{:name "サンプルプロジェクト"
 :code :cddlj}
```

REPLで`go`。

```
$ lein repl
cddlj.core=> (user/go)
```
