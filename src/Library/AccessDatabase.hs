module Library.AccessDatabase
    ( module Database.Relational.Query.PostgreSQL
    , selectAllFromLectureWherePeriod
    , selectAllFromStudentWhereStudentNumber
    , selectAllFromGradeWhereStudentId
    ) where

import GHC.Int
import Database.Relational.Query
import Database.Relational.Query.PostgreSQL
import Database.Relations.Student as Student
import Database.Relations.Lecture as Lecture
import Database.Relations.Course as Course
import Database.Relations.Grade as Grade

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

type StudentId = GHC.Int.Int32
selectAllFromGradeWhereStudentId :: Relation StudentId Grade
selectAllFromGradeWhereStudentId = relation' $ do
    g <- query Grade.grade
    (ph, ()) <- placeholder $ \studentId -> do
        wheres $ g ! Grade.studentId' .=. studentId
    return (ph, g)
