module Lib
    ( module AST
    , parseContract
    , initContractState
    , executeTransaction
    , validateTransaction
    , someFunc
    ) where

import AST
import Parser
import VM

someFunc :: IO ()
someFunc = putStrLn "BlockChainLang VM"
