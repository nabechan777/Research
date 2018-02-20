{-# LANGUAGE TemplateHaskell #-}
module Database.Record.TH.SQLite3
    ( defineTable
    ) where

import Database.HDBC.SQLite3 (connectSQLite3)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Schema.Driver (typeMap)
import GHC.Generics (Generic)
import Language.Haskell.TH (Dec, Q)

type DatabaseName = String
type TableName    = String

defineTable :: DatabaseName -> TableName -> Q[Dec]
defineTable databaseName tableName = defineTableFromDB
    where
        catalog = connectSQLite3 databaseName
        driver = driverSQLite3 { typeMap = [("INTEGER", [t|Int|])] }
        schemaName = "main"
        deriver = [''Show, ''Generic]
