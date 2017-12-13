{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.Relations.Course where

import Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "course")
