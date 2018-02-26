{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

{-|
 関係Courseに対応する代数的データ型とその代数的データ型に関連する操作を定義しているモジュール。
 これらの代数的データ型および関数はTemplateHaskellによって定義される。
-}
module Database.Relations.Course where

import           Database.Record.TH.PostgreSQL (defineTable)

$(defineTable "research" "course")
