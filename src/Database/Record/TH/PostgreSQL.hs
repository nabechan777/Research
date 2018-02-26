{-# LANGUAGE TemplateHaskell #-}

-- | 各関係に対応する代数的データ型を出力する関数'defineTable'を定義しているモジュール
module Database.Record.TH.PostgreSQL
    ( defineTable
    ) where

import Data.Int (Int64)
import Database.HDBC.PostgreSQL        (connectPostgreSQL)
import Database.HDBC.Query.TH          (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import GHC.Generics                    (Generic)
import Language.Haskell.TH             (Dec, Q)

type DatabaseName = String -- ^ データベース名を示す識別子
type TableName = String -- ^ 関係を示す識別子

-- | データベース名を示す識別子と関係を示す識別子を入力とし、関係に対応する代数的データ型を出力する。
defineTable :: DatabaseName -- ^ PostgreSQLに定義されているデータベース名を示す識別子
            -> TableName -- ^ 接続したデータベース内に定義されている関係を示す識別子
            -> Q [Dec] -- ^ 関係に対応する代数的データ型
defineTable databaseName tableName = defineTableFromDB catalog driver schemaName tableName derive
    where
        catalog = connectPostgreSQL ("dbname=" ++ databaseName)
        driver = driverPostgreSQL
        schemaName = "public"
        derive = [''Show, ''Generic]
