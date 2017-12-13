{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.Relations.Student where

import Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "student")

{-
data Student = Student {
    Database.Relations.Student.studentId :: !GHC.Int.Int32,
    Database.Relations.Student.name :: !String,
    Database.Relations.Student.gradeId :: !Maybe GHC.Int.Int32,
    Database.Relations.Student.semesterId :: !Maybe GHC.Int.Int32,
    Database.Relations.Student.facultyId :: !Maybe GHC.Int.Int32
}
-}
