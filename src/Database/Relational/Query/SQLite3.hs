{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Query.SQLite3
    ( module Database.HDBC
    , moudle Database.HDBC.SQLite3
    , moudle Database.HDBC.Session
    , moudle Database.Relational.Query
    , moudle Database.Record
    , runRelation
    ) where

import Database.HDBC
    ( IConnection
    , SqlValue
    )
import Database.HDBC.SQLite3
    ( connectSQLite3
    )
import Database.HDBC.Session
import Database.HDBC.Record
    ( runQuery
    )
import Database.Record
    ( ToSql
    , FromSql
    )
import Database.Relational.Query
    ( Relation
    , relationalQuery
    )

runRelation :: (ToSql SqlValue p, IConnection conn, FromSql SqlValue a) =>
               conn -> Relation p a -> p -> IO [a]
runRelation conn q p = runQuery conn (relationalQuery q) p
