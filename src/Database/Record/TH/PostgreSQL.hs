{-# LANGUAGE TemplateHaskell #-}

module Database.Record.TH.PostgreSQL
    ( defineTable
    ) where

import Data.Int (Int64)
import Database.HDBC.PostgreSQL        (connectPostgreSQL)
import Database.HDBC.Query.TH          (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import GHC.Generics                    (Generic)
import Language.Haskell.TH             (Dec, Q)

type DatabaseName = String
type TableName = String

defineTable :: DatabaseName -> TableName -> Q [Dec]
defineTable databaseName tableName = defineTableFromDB catalog driver schemaName tableName derive
    where
        catalog = connectPostgreSQL ("dbname=" ++ databaseName)
        driver = driverPostgreSQL
        schemaName = "public"
        derive = [''Show, ''Generic]
