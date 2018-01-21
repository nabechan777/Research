{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.Relations.Grade where

import Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "grade")
