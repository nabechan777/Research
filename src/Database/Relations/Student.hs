{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.Relations.Student where

import Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "student")
