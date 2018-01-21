module Library.AccessDatabase
    ( selectAllFromLecture
    , selectAllFromLectureWherePeriod
    , selectAllFromStudentWhereStudentNumber
    ) where

import GHC.Int
import Database.Relational.Query
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Database.Relational.Query.PostgreSQL
import Database.Relations.Student as Student
import Database.Relations.Lecture as Lecture
import Database.Relations.Course as Course

run :: IO ()
run = handleSqlError' $ withConnectionIO (connectPostgreSQL "dbname=research") $ \conn -> do
    -- s <- runRelation conn (selectAllFromStudentWhereStudentNumber 12345) ()
    s <- runRelation conn selectAllFromStudentWhereStudentNumber 12345
    mapM_ print s

selectAllFromLecture :: Relation () Lecture
selectAllFromLecture = relation $ do
    l <- query Lecture.lecture
    return l

type Period = String
selectAllFromLectureWherePeriod :: Relation Period Lecture
selectAllFromLectureWherePeriod = relation' $ do
    l <- query Lecture.lecture
    (ph, ()) <- placeholder $ \period -> do
        wheres $ l ! Lecture.period' .=. period
    return (ph, l)

type StudentNumber = GHC.Int.Int32
selectAllFromStudentWhereStudentNumber :: Relation StudentNumber Student
selectAllFromStudentWhereStudentNumber = relation' $ do
    s <- query Student.student
    (ph, ()) <- placeholder $ \studentNumber -> do
        wheres $ s ! Student.studentNumber' .=. studentNumber
    return (ph, s)
