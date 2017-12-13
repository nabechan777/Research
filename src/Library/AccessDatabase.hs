module Library.AccessDatabase where

import Database.Relational.Query
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Database.Relational.Query.PostgreSQL
import Database.Relations.Lecture (Lecture)
import qualified Database.Relations.Lecture as L

run :: IO ()
run = handleSqlError' $ withConnectionIO (connectPostgreSQL "dbname=research") $ \conn -> do
    -- lectures <- runRelation conn selectLecture ()
    lectureNames <- runRelation conn selectLectureName ()
    -- mapM_ print lectures
    mapM_ putStrLn lectureNames

selectLecture :: Relation () Lecture
selectLecture = relation $ do
    l <- query L.lecture
    return l

selectLectureName :: Relation () String
selectLectureName = relation $ do
    l <- query L.lecture
    return $ l ! L.name'
