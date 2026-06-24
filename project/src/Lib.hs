module Lib
    ( module AST
    , someFunc
    ) where

import AST

someFunc :: IO ()
someFunc = putStrLn "BlockChainLang VM"
