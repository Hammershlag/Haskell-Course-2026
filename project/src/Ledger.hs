module Ledger
  ( genesisBlock
  , emptyLedger
  , validateBlockHeader
  , processTransactions
  , addBlock
  , evaluateLedgerState
  , verifyLedgerIntegrity
  ) where

import AST
import VM
import Control.Monad (foldM_)

genesisBlock :: Block
genesisBlock = Block 0 Nothing []

emptyLedger :: Ledger
emptyLedger = Ledger [genesisBlock]

validateBlockHeader :: Ledger -> Block -> Either String ()
validateBlockHeader (Ledger blocks) block =
  case parentId block of
    Nothing ->
      if blockId block == 0
        then Right ()
        else Left "Invalid genesis block ID"
    Just pId ->
      if any (\b -> blockId b == pId) blocks
        then if any (\b -> blockId b == blockId block) blocks
          then Left $ "Block ID " ++ show (blockId block) ++ " already exists in ledger"
          else Right ()
        else Left $ "Parent block " ++ show pId ++ " not found in ledger"

processTransactions :: Contract -> [Transaction] -> ContractState -> (ContractState, [Either String ContractState])
processTransactions contract txs initialState =
  let (finalState, reversedResults) = foldl step (initialState, []) txs
  in (finalState, reverse reversedResults)
  where
    step (state, acc) tx = case executeTransaction contract tx state of
      Left err -> (state, Left err : acc)
      Right newState -> (newState, Right newState : acc)

addBlock :: Ledger -> Block -> Either String Ledger
addBlock ledger block = do
  validateBlockHeader ledger block
  return $ Ledger (getBlocks ledger ++ [block])

evaluateLedgerState :: Contract -> String -> Ledger -> Either String ContractState
evaluateLedgerState contract deployer (Ledger blocks) = do
  initialState <- initContractState contract deployer
  let folder state block =
        let (nextState, _) = processTransactions contract (blockTx block) state
        in nextState
  return $ foldl folder initialState blocks

verifyLedgerIntegrity :: Ledger -> Either String ()
verifyLedgerIntegrity (Ledger []) = Left "Empty ledger"
verifyLedgerIntegrity (Ledger (genesis:rest)) = do
  if blockId genesis /= 0 || parentId genesis /= Nothing
    then Left "Invalid genesis block"
    else return ()
  foldM_ checkBlock (Ledger [genesis]) rest
  where
    checkBlock tempLedger block = do
      validateBlockHeader tempLedger block
      return $ Ledger (getBlocks tempLedger ++ [block])
