module Library.AccessDatabase where

import GHC.Int
import Database.Relational.Query
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Database.Relational.Query.PostgreSQL
import Database.Relations.Student (Student(..))
import qualified Database.Relations.Student as Student
import Database.Relations.Lecture (Lecture(..))
import qualified Database.Relations.Lecture as Lecture

run :: IO ()
run = handleSqlError' $ withConnectionIO (connectPostgreSQL "dbname=research") $ \conn -> do
    s <- runRelation conn (selectAllFromStudentWhereStudentNumber 12345) ()
    mapM_ print s

selectAllFromLecture :: Relation () Lecture
selectAllFromLecture = relation $ do
    l <- query Lecture.lecture
    return l

type Period = String
selectAllFromLectureWherePeriod :: Period -> Relation () Lecture
selectAllFromLectureWherePeriod p = relation $ do
    l <- query Lecture.lecture
    wheres $ l ! Lecture.period' .=. value p
    return l

selectLectureName :: Relation () String
selectLectureName = relation $ do
    l <- query Lecture.lecture
    return $ l ! Lecture.name'

type StudentNumber = GHC.Int.Int32
selectAllFromStudentWhereStudentNumber :: StudentNumber -> Relation () Student
selectAllFromStudentWhereStudentNumber n = relation $ do
    s <- query Student.student
    wheres $ s ! Student.studentNumber' .=. value n
    return s
