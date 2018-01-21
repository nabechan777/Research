module Main
    ( main
    ) where

import Database.HDBC.Types (commit)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Session (handleSqlError', withConnectionIO')
import Library.InsertDB

main :: IO ()
main = handleSqlError' $ withConnectionIO' (connectPostgreSQL "dbname=research") $ \conn -> do
    progressInsert $ insertStudents conn
    progressInsert $ insertLectures conn
    progressInsert $ insertGrades conn
    commit conn
    where
        progressInsert :: IO () -> IO ()
        progressInsert action = do
            putStrLn "start"
            action
            putStrLn "finished"
