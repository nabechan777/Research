{-# LANGUAGE TemplateHaskell #-}

module Database.Record.TH.PostgreSQL
    ( defineTable
    ) where

import GHC.Generics (Generic)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Language.Haskell.TH (Q, Dec)

type DatabaseName = String
type TableName = String

defineTable :: DatabaseName -> TableName -> Q [Dec]
defineTable databaseName tableName = defineTableFromDB catalog driver schemaName tableName derive
    where
        catalog = connectPostgreSQL ("dbname=" ++ databaseName)
        driver = driverPostgreSQL -- { typeMap = [("INTEGER", [t|Int|])] }
        schemaName = "public"
        derive = [''Show, ''Generic]
