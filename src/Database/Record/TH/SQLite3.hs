{-# LANGUAGE TemplateHaskell #-}

-- | 各関係に対応する代数的データ型を生成する'defineTable'を定義しているモジュール
module Database.Record.TH.SQLite3
    ( defineTable
    ) where

import Database.HDBC.SQLite3 (connectSQLite3)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Schema.Driver (typeMap)
import GHC.Generics (Generic)
import Language.Haskell.TH (Dec, Q)

type DatabaseName = String -- ^ データベース名を示す識別子
type TableName    = String -- ^ 関係を示す識別子


-- | データベース名を示す識別子と関係を示す識別子を入力とし、その関係に対応する代数的データ型を出力する。
defineTable :: DatabaseName -- ^ SQLite3内に定義されているデータベース名を示す識別子
            -> TableName -- ^ 接続したデータベース内に定義されている関係の識別子
            -> Q[Dec] -- ^ 関係に対応する代数的データ型
defineTable databaseName tableName = defineTableFromDB
    where
        catalog = connectSQLite3 databaseName
        driver = driverSQLite3 { typeMap = [("INTEGER", [t|Int|])] }
        schemaName = "main"
        deriver = [''Show, ''Generic]
