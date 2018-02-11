{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Relations.Course where

import           Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "course")
