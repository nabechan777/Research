{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Relations.Student where

import Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "student")
