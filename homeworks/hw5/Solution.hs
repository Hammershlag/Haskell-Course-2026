import Control.Monad.State
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

-- State Monad

-- 1. Stack Machine

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Show, Eq)

execInstr :: Instr -> State [Int] ()
execInstr instr = do
  stack <- get
  case (instr, stack) of
    (PUSH n, xs)   -> put (n : xs)
    (POP, _:xs)    -> put xs
    (DUP, x:xs)    -> put (x : x : xs)
    (SWAP, x:y:xs) -> put (y : x : xs)
    (ADD, x:y:xs)  -> put ((x + y) : xs)
    (MUL, x:y:xs)  -> put ((x * y) : xs)
    (NEG, x:xs)    -> put (-x : xs)
    _              -> return ()

execProg :: [Instr] -> State [Int] ()
execProg instrs = mapM_ execInstr instrs

runProg :: [Instr] -> [Int]
runProg instrs = execState (execProg instrs) []

-- 2. Expression evaluator with variable bindings

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr   -- bind the value of the expression to the name, return that value
  | Seq  Expr Expr       -- evaluate the left, then the right; return the value of the right

eval :: Expr -> State (Map.Map String Int) Int
eval (Num n)       = return n
eval (Var name)    = do
  env <- get
  return (env Map.! name)
eval (Add e1 e2)   = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)
eval (Mul e1 e2)   = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 * v2)
eval (Neg e)       = do
  v <- eval e
  return (-v)
eval (Assign name e) = do
  v <- eval e
  modify (Map.insert name v)
  return v
eval (Seq e1 e2)   = do
  _ <- eval e1
  eval e2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty

-- 3. Memoised edit (Levenshtein) distance

editDistM :: String -> String -> Int -> Int -> State (Map.Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get
  case Map.lookup (i, j) cache of
    Just dist -> return dist
    Nothing   -> do
      dist <- compute
      modify (Map.insert (i, j) dist)
      return dist
  where
    compute
      | i == 0 = return j
      | j == 0 = return i
      | xs !! (i - 1) == ys !! (j - 1) = editDistM xs ys (i - 1) (j - 1)
      | otherwise = do
          del <- editDistM xs ys (i - 1) j
          ins <- editDistM xs ys i (j - 1)
          sub <- editDistM xs ys (i - 1) (j - 1)
          return $ 1 + minimum [del, ins, sub]

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty

-- StateT and "Treasure Hunters" Game Simulation

data GameState = GameState
  { position :: Int
  , energy   :: Int
  , score    :: Int
  } deriving (Show)

type AdventureGame a = StateT GameState IO a

-- 4. Player movement and decisions

movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
  modify (\s -> s { position = position s + roll, energy = energy s - 1 })
  return roll

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  choice <- liftIO $ getPlayerChoice options
  return choice

-- 5. Game loop

handleLocation :: AdventureGame Bool
handleLocation = do
  st <- get
  let pos = position st
  if pos >= 20
    then do
      liftIO $ putStrLn "\nGoal reached. You found the main treasure!"
      return True
    else do
      case pos of
        3 -> do
          liftIO $ putStrLn "Treasure found. (+10 score)"
          modify (\s -> s { score = score s + 10 })
        7 -> do
          liftIO $ putStrLn "Trap triggered. Gold lost. (-5 score)"
          modify (\s -> s { score = max 0 (score s - 5) })
        12 -> do
          liftIO $ putStrLn "Steep hill. Extra energy used. (-2 energy)"
          modify (\s -> s { energy = energy s - 2 })
        15 -> do
          liftIO $ putStrLn "Fork in the road."
          choice <- makeDecision ["left", "right"]
          if choice == "left"
            then liftIO $ putStrLn "Path is safe."
            else do
              liftIO $ putStrLn "Rough path. (-1 energy)"
              modify (\s -> s { energy = energy s - 1 })
        _ -> liftIO $ putStrLn "Path clear."
      return False

playTurn :: AdventureGame Bool
playTurn = do
  st <- get
  liftIO $ displayGameState st

  if energy st <= 0
    then do
      liftIO $ putStrLn "Energy depleted. Game over."
      return True
    else do
      roll <- liftIO getDiceRoll
      _ <- movePlayer roll
      liftIO $ putStrLn $ "Moved " ++ show roll ++ " steps."
      handleLocation

playGame :: AdventureGame ()
playGame = do
  gameOver <- playTurn
  if gameOver
    then do
      st <- get
      liftIO $ putStrLn $ "Final Score: " ++ show (score st)
    else playGame

-- 6. User interaction in IO

getDiceRoll :: IO Int
getDiceRoll = do
  putStr "Enter dice roll (1-6): "
  input <- getLine
  case reads input of
    [(n, "")] | n >= 1 && n <= 6 -> return n
    _ -> do
      putStrLn "Invalid input. Please enter a number between 1 and 6."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState (GameState pos eng scr) =
  putStrLn $ "\n[State] Position: " ++ show pos ++ " | Energy: " ++ show eng ++ " | Score: " ++ show scr

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  putStrLn $ "Choose a path: " ++ show options
  putStr "Your choice: "
  choice <- getLine
  if choice `elem` options
    then return choice
    else do
      putStrLn "Invalid choice."
      getPlayerChoice options
