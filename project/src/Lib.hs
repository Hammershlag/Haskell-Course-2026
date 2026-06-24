module Lib
    ( module AST
    , parseContract
    , someFunc
    ) where

import AST
import Parser

someFunc :: IO ()
someFunc = putStrLn "BlockChainLang VM"
