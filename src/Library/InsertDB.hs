{-# LANGUAGE FlexibleContexts, TemplateHaskell, DeriveGeneric #-}
module Library.InsertDB where

import Control.Applicative
import Database.HDBC (commit)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Record (mapInsert)
import Database.HDBC.Session (handleSqlError', withConnectionIO')
import qualified Database.Relations.Student as Student
import Database.Relations.Student (Student(..), insertStudent)
import qualified Database.Relations.Lecture as Lecture
import Database.Relations.Lecture (Lecture(..), insertLecture)
import Database.Relational.Query

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
lectures = getZipList $ (\a b c d e -> Lecture a b c d e) <$> lectureIds <*> names <*> periods <*> fields <*> credits
    where
        lectureIds = ZipList [1..]
        names = ZipList $ cycle ["AAA", "BBB", "CCC", "DDD"]
        periods = ZipList $ cycle $ (\x y z -> x ++ y ++ z) <$> ["S", "A"]
                                                            <*> ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
                                                            <*> ["1", "2", "3", "4", "5"]
        fields = ZipList $ cycle ["共通", "専門"]
        credits = ZipList $ cycle [2]

run :: IO ()
run = handleSqlError' $ withConnectionIO' (connectPostgreSQL "dbname=research") $ \conn -> do
    mapInsert conn insertStudent students
    mapInsert conn insertLecture $ take 80 lectures
    return ()
