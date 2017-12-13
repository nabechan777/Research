{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}

module Database.Relations.Lecture where

import Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "lecture")

{-
data Lecture = Lecture {
    Database.Relations.Lecture.lectureId :: !GHC.Int.Int32,
    Database.Relations.Lecture.name :: !String,
    Database.Relations.Lecture.coursePeriodId :: !Maybe GHC.Int.Int32,
    Database.Relations.Lecture.fieldId :: !Maybe GHC.Int.Int32,
    Database.Relations.Lecture.gradeId :: !Maybe GHC.Int.Int32,
    credit :: !GHC.Int.Int32
}
-}
