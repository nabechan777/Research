{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | relational-recordで定義されている関数群を用いて作成したクエリーを実行する関数'runRelation'を定義するモジュール
module Database.Relational.Query.SQLite3
    ( module Database.HDBC
    , module Database.HDBC.SQLite3
    , module Database.HDBC.Session
    , module Database.Relational.Query
    , module Database.Record
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

-- | データベースとのコネクションと実行するクエリーを入力とし、クエリーの実行結果を出力とする。
runRelation :: (ToSql SqlValue p, IConnection conn, FromSql SqlValue a)
            => conn -- ^ SQLite3において定義されているデータベースとのコネクション
            -> Relation p a -- ^ 実行するクエリーを示すデータ型
            -> p -- ^ クエリーの入力とする値
            -> IO [a] -- ^ クエリーの実行によって出力された値のリスト
runRelation conn q p = runQuery conn (relationalQuery q) p
