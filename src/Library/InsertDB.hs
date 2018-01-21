{-# LANGUAGE FlexibleContexts, TemplateHaskell, DeriveGeneric #-}
module Library.InsertDB (
    insertStudents,
    insertLectures,
    insertGrades,
    ) where

import Control.Applicative
import Database.HDBC.Types (IConnection)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Record (mapInsert)
import qualified Database.Relations.Student as Student
import Database.Relations.Student (Student(..), insertStudent)
import qualified Database.Relations.Lecture as Lecture
import Database.Relations.Lecture (Lecture(..), insertLecture)
import qualified Database.Relations.Grade as Grade
import Database.Relations.Grade (Grade(..), insertGrade)

students :: [Student]
students = getZipList $ (\a b c d e f -> Student a b c d e f) <$> studentIds <*> studentNumbers <*> names <*> faculties <*> departments <*> semesters
    where
        studentIds = ZipList [1..]
        studentNumbers = ZipList [12345, 23421, 23453]
        names = ZipList ["A", "B", "C"]
        faculties = ZipList $ cycle ["コンピュータ理工"]
        departments = ZipList ["コンピュータサイエンス", "ネットワークメディア", "インテリジェントシステム"]
        semesters = ZipList ["8セメ", "6セメ", "4セメ"]

lectures :: [Lecture]
lectures = getZipList $ (\a b c d -> Lecture a (fst b) c (snd b) d) <$> lectureIds <*> namesAndFields <*> periods <*> credits
    where
        lectureIds = ZipList [1..]
        namesAndFields = ZipList $ cycle [("共通講義", "共通"), ("専門講義", "専門")]
        periods = ZipList $ cycle $ (\x y z -> x ++ y ++ z) <$> ["S", "A"]
                                                            <*> ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
                                                            <*> ["1", "2", "3", "4", "5"]
        -- fields = ZipList $ cycle ["共通", "専門"]
        credits = ZipList $ cycle [2]

grades :: [Grade]
grades = getZipList $ (\a b c d -> Grade a b c d) <$> gradeIds <*> studentIds <*> commons <*> specials
    where
        gradeIds = ZipList [1..]
        studentIds = ZipList [1]
        commons = ZipList [70]
        specials = ZipList [50]

insertStudents :: (IConnection conn) => conn -> IO ()
insertStudents conn = mapInsert conn insertStudent students >>= (\x -> return ())

insertLectures :: (IConnection conn) => conn -> IO ()
insertLectures conn = mapInsert conn insertLecture (take 200 lectures) >>= (\x -> return ())

insertGrades :: (IConnection conn) => conn -> IO ()
insertGrades conn = mapInsert conn insertGrade grades >>= (\x -> return ())

-- run :: IO ()
-- run = handleSqlError' $ withConnectionIO' (connectPostgreSQL "dbname=research") $ \conn -> do
--     mapInsert conn insertStudent students
--     mapInsert conn insertLecture $ take 200 lectures
--     mapInsert conn insertGrade grades
--     return ()
