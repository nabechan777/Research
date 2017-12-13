module Main where

import qualified Library.Interface as Interface
import qualified Library.InsertDB as InsertDB
import qualified Library.AccessDatabase as AccessDatabase

mainImpl :: String -> IO ()
mainImpl "insert data" = InsertDB.run
mainImpl "main" = Interface.run
mainImpl _ = do
    putStrLn "choice \"insert data\" or \"main\""
    mode <- getLine
    mainImpl mode

main :: IO ()
main = do
    mode <- getLine
    mainImpl mode
