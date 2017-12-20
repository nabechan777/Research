{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}

module Database.Relations.Lecture where

import Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "lecture")
