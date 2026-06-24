module VM
  ( VMEnv(..)
  , initContractState
  , executeTransaction
  , validateTransaction
  ) where

import qualified Data.Map as Map
import Control.Monad.State
import AST

data VMEnv = VMEnv
  { envState :: ContractState
  , envParams :: Map.Map String Value
  , envSender :: String
  , envTypes :: Map.Map String Type
  } deriving (Show, Eq)

defaultValue :: Type -> Value
defaultValue TInt = VInt 0
defaultValue TBool = VBool False
defaultValue TAddress = VAddress ""
defaultValue (TMap _ _) = VMap Map.empty

typeOfExpr :: Map.Map String Type -> Expr -> Either String Type
typeOfExpr env expr = case expr of
  Var name -> case Map.lookup name env of
    Just t -> Right t
    Nothing -> Left $ "Variable not found: " ++ name
  Lit (VInt _) -> Right TInt
  Lit (VBool _) -> Right TBool
  Lit (VAddress _) -> Right TAddress
  Lit (VMap _) -> Left "Map literal type is unknown"
  BinOp op _ _ -> case op of
    Add -> Right TInt
    Sub -> Right TInt
    Mul -> Right TInt
    Div -> Right TInt
    _   -> Right TBool
  Index e1 _ -> do
    t1 <- typeOfExpr env e1
    case t1 of
      TMap _ tVal -> Right tVal
      _ -> Left "Indexing a non-map type"
  Sender -> Right TAddress
  EmptyMap -> Left "Empty map type is unknown"

evalExpr :: Expr -> StateT VMEnv (Either String) Value
evalExpr expr = case expr of
  Var name -> do
    env <- get
    case Map.lookup name (envParams env) of
      Just v -> return v
      Nothing -> case Map.lookup name (envState env) of
        Just v -> return v
        Nothing -> lift $ Left $ "Variable not found: " ++ name
  Lit val -> return val
  BinOp op e1 e2 -> do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case op of
      Add -> case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (i1 + i2)
        _ -> lift $ Left "Type error: expected integers for Add"
      Sub -> case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (i1 - i2)
        _ -> lift $ Left "Type error: expected integers for Sub"
      Mul -> case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (i1 * i2)
        _ -> lift $ Left "Type error: expected integers for Mul"
      Div -> case (v1, v2) of
        (VInt _, VInt 0) -> lift $ Left "Division by zero"
        (VInt i1, VInt i2) -> return $ VInt (i1 `div` i2)
        _ -> lift $ Left "Type error: expected integers for Div"
      Equal -> return $ VBool (v1 == v2)
      NotEqual -> return $ VBool (v1 /= v2)
      LessThan -> case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 < i2)
        (VAddress a1, VAddress a2) -> return $ VBool (a1 < a2)
        _ -> lift $ Left "Type error: expected integers or addresses for LessThan"
      LessEqual -> case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 <= i2)
        (VAddress a1, VAddress a2) -> return $ VBool (a1 <= a2)
        _ -> lift $ Left "Type error: expected integers or addresses for LessEqual"
      GreaterThan -> case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 > i2)
        (VAddress a1, VAddress a2) -> return $ VBool (a1 > a2)
        _ -> lift $ Left "Type error: expected integers or addresses for GreaterThan"
      GreaterEqual -> case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 >= i2)
        (VAddress a1, VAddress a2) -> return $ VBool (a1 >= a2)
        _ -> lift $ Left "Type error: expected integers or addresses for GreaterEqual"
      And -> case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool (b1 && b2)
        _ -> lift $ Left "Type error: expected booleans for And"
      Or -> case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool (b1 || b2)
        _ -> lift $ Left "Type error: expected booleans for Or"
  Index e1 e2 -> do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case v1 of
      VMap m -> case Map.lookup v2 m of
        Just val -> return val
        Nothing -> do
          env <- get
          case typeOfExpr (envTypes env) expr of
            Right t -> return (defaultValue t)
            Left err -> lift $ Left err
      _ -> lift $ Left "Type error: expected a map for indexing"
  Sender -> do
    env <- get
    return $ VAddress (envSender env)
  EmptyMap -> return $ VMap Map.empty

updateVar :: String -> Value -> VMEnv -> Either String VMEnv
updateVar name val env =
  if Map.member name (envState env)
    then Right env { envState = Map.insert name val (envState env) }
    else if Map.member name (envParams env)
      then Right env { envParams = Map.insert name val (envParams env) }
      else Left $ "Variable not found: " ++ name

assignToExpr :: Expr -> Value -> VMEnv -> Either String VMEnv
assignToExpr lhs val env = case lhs of
  Var name -> updateVar name val env
  Index e1 e2 -> do
    (v1, _) <- runStateT (evalExpr e1) env
    (v2, _) <- runStateT (evalExpr e2) env
    case v1 of
      VMap m ->
        let updatedMap = VMap (Map.insert v2 val m)
        in assignToExpr e1 updatedMap env
      _ -> Left "Type error: assignment target is not a map"
  _ -> Left "Type error: invalid assignment target"

execStatement :: Statement -> StateT VMEnv (Either String) ()
execStatement stmt = case stmt of
  Assign lhs rhs -> do
    val <- evalExpr rhs
    env <- get
    case assignToExpr lhs val env of
      Right newEnv -> put newEnv
      Left err -> lift $ Left err
  Require cond -> do
    val <- evalExpr cond
    case val of
      VBool True -> return ()
      VBool False -> lift $ Left "Require assertion failed"
      _ -> lift $ Left "Type error: require condition must be boolean"
  If cond thenBranch elseBranch -> do
    val <- evalExpr cond
    case val of
      VBool True -> mapM_ execStatement thenBranch
      VBool False -> mapM_ execStatement elseBranch
      _ -> lift $ Left "Type error: if condition must be boolean"

initContractState :: Contract -> String -> Either String ContractState
initContractState contract deployer =
  let vars = contractStateVars contract
      initialEnv = VMEnv
        { envState = Map.empty
        , envParams = Map.empty
        , envSender = deployer
        , envTypes = Map.fromList [ (svName v, svType v) | v <- vars ]
        }
      runInit = mapM_ initVar vars
      initVar (StateVar name _ initExpr) = do
        val <- evalExpr initExpr
        env <- get
        put env { envState = Map.insert name val (envState env) }
  in case runStateT runInit initialEnv of
       Left err -> Left err
       Right ((), finalEnv) -> Right (envState finalEnv)

matchType :: Value -> Type -> Bool
matchType (VInt _) TInt = True
matchType (VBool _) TBool = True
matchType (VAddress _) TAddress = True
matchType (VMap m) (TMap kT vT) =
  all (\k -> matchType k kT) (Map.keys m) && all (\v -> matchType v vT) (Map.elems m)
matchType _ _ = False

validateTransaction :: Contract -> Transaction -> Either String TransactionDef
validateTransaction contract tx = do
  let target = txTarget tx
  txDef <- case filter (\d -> txName d == target) (contractTransactions contract) of
    [d] -> Right d
    []  -> Left $ "Transaction not defined in contract: " ++ target
    _   -> Left $ "Multiple transactions with name: " ++ target
  let expectedParams = txParams txDef
      args = txArgs tx
  if length expectedParams /= length args
    then Left $ "Argument count mismatch for " ++ target ++ ": expected " ++ show (length expectedParams) ++ ", got " ++ show (length args)
    else return ()
  let typeChecks = zipWith (\arg (pName, pType) -> (arg, pName, pType)) args expectedParams
  mapM_ (\(arg, pName, pType) ->
    if matchType arg pType
      then Right ()
      else Left $ "Type mismatch for parameter " ++ pName ++ ": expected " ++ show pType ++ ", got value " ++ show arg
    ) typeChecks
  return txDef

executeTransaction :: Contract -> Transaction -> ContractState -> Either String ContractState
executeTransaction contract tx currentState = do
  txDef <- validateTransaction contract tx
  let paramBindings = Map.fromList $ zip (map fst (txParams txDef)) (txArgs tx)
      stateTypes = [ (svName v, svType v) | v <- contractStateVars contract ]
      paramTypes = txParams txDef
      typeEnv = Map.fromList (stateTypes ++ paramTypes)
      initialEnv = VMEnv
        { envState = currentState
        , envParams = paramBindings
        , envSender = txSender tx
        , envTypes = typeEnv
        }
      runBody = mapM_ execStatement (txBody txDef)
  case runStateT runBody initialEnv of
    Left err -> Left $ "Transaction execution failed: " ++ err
    Right ((), finalEnv) -> Right (envState finalEnv)
