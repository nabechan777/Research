{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Relations.Grade where

import           Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "grade")
