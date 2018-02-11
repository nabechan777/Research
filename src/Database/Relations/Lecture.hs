{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Relations.Lecture where

import           Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "lecture")
