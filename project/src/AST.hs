module AST
  ( Type(..)
  , Value(..)
  , Op(..)
  , Expr(..)
  , Statement(..)
  , StateVar(..)
  , TransactionDef(..)
  , Contract(..)
  , Transaction(..)
  , Block(..)
  , Ledger(..)
  , ContractState
  ) where

import qualified Data.Map as Map

data Type
  = TInt
  | TBool
  | TAddress
  | TMap Type Type
  deriving (Show, Eq, Ord)

data Value
  = VInt Integer
  | VBool Bool
  | VAddress String
  | VMap (Map.Map Value Value)
  deriving (Show, Eq, Ord)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Equal
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | And
  | Or
  deriving (Show, Eq, Ord)

data Expr
  = Var String
  | Lit Value
  | BinOp Op Expr Expr
  | Index Expr Expr
  | Sender
  | EmptyMap
  deriving (Show, Eq, Ord)

data Statement
  = Assign Expr Expr
  | Require Expr
  | If Expr [Statement] [Statement]
  deriving (Show, Eq, Ord)

data StateVar = StateVar
  { svName :: String
  , svType :: Type
  , svInit :: Expr
  } deriving (Show, Eq, Ord)

data TransactionDef = TransactionDef
  { txName   :: String
  , txParams :: [(String, Type)]
  , txBody   :: [Statement]
  } deriving (Show, Eq, Ord)

data Contract = Contract
  { contractName         :: String
  , contractStateVars    :: [StateVar]
  , contractTransactions :: [TransactionDef]
  } deriving (Show, Eq, Ord)

data Transaction = Transaction
  { txTarget :: String
  , txArgs   :: [Value]
  , txSender :: String
  } deriving (Show, Eq, Ord)

data Block = Block
  { blockId  :: Int
  , parentId :: Maybe Int
  , blockTx  :: [Transaction]
  } deriving (Show, Eq, Ord)

newtype Ledger = Ledger { getBlocks :: [Block] }
  deriving (Show, Eq, Ord)

type ContractState = Map.Map String Value
