module Lib
    ( module AST
    , parseContract
    , initContractState
    , executeTransaction
    , validateTransaction
    , genesisBlock
    , emptyLedger
    , validateBlockHeader
    , processTransactions
    , addBlock
    , evaluateLedgerState
    , verifyLedgerIntegrity
    , someFunc
    ) where

import AST
import Parser
import VM
import Ledger

someFunc :: IO ()
someFunc = putStrLn "BlockChainLang VM"
