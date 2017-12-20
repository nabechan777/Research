{-# LANGUAGE FlexibleContexts #-}

module Database.Relational.Query.PostgreSQL
    ( module Database.HDBC
    , module Database.HDBC.Query.TH
    , module Database.HDBC.Record
    , module Database.HDBC.Session
    , module Database.HDBC.PostgreSQL
    , module Database.Record
    , module Database.Relational.Query
    , runRelation
    ) where

import Database.HDBC hiding (execute, finish, run)
import Database.HDBC.Query.TH
import Database.HDBC.Record
import Database.HDBC.Session
import Database.HDBC.PostgreSQL
import Database.Record hiding (unique)
import Database.Relational.Query hiding (unique)

runRelation :: (ToSql SqlValue p, IConnection conn, FromSql SqlValue a)
            => conn -> Relation p a -> p -> IO [a]
runRelation conn q p = runQuery conn (relationalQuery q) p
