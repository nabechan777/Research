{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Query.PostgreSQL
    ( module Database.HDBC
    , module Database.HDBC.PostgreSQL
    , module Database.HDBC.Session
    , module Database.Relational.Query
    , module Database.Record
    , runRelation
    ) where

import Database.HDBC
    ( IConnection
    , SqlValue
    )
import Database.HDBC.PostgreSQL
    ( connectPostgreSQL
    )
-- import Database.HDBC.Query.TH
import Database.HDBC.Record
    ( runQuery
    )
import Database.HDBC.Session
    ( handleSqlError'
    , withConnectionIO'
    )
import Database.Record
    ( ToSql
    , FromSql
    )
import Database.Relational.Query
    ( Relation
    , relationalQuery
    )

runRelation :: (ToSql SqlValue p, IConnection conn, FromSql SqlValue a)
            => conn -> Relation p a -> p -> IO [a]
runRelation conn q p = runQuery conn (relationalQuery q) p
