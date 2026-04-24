import Data.Map (Map)
import Data.List (permutations)
import Control.Monad (guard)
import qualified Data.Map as Map
-- 1. Maze navigation

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- a)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    directions <- Map.lookup pos maze
    Map.lookup dir directions

-- b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos [] = Just pos
followPath maze pos (d:ds) = do
    newPos <- move maze pos d
    followPath maze newPos ds

-- c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos [] = Just [pos]
safePath maze pos (d:ds) = do
    newPos <- move maze pos d
    rest <- safePath maze newPos ds
    Just (pos : rest)

-- 2. Decoding a message

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key msg = traverse (\c -> Map.lookup c key) msg

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- 3. Seating arrangements

type Guest = String
type Conflict = (Guest, Guest)

isConflict :: [Conflict] -> (Guest, Guest) -> Bool
isConflict conflicts (g1, g2) =
    (g1, g2) `elem` conflicts || (g2, g1) `elem` conflicts

hasConflict :: [Conflict] -> [Guest] -> Bool
hasConflict conflicts ps = any (isConflict conflicts) pairs
  where
    adjacentPairs = zip ps (tail ps)
    wrapAround = (last ps, head ps)
    pairs = wrapAround : adjacentPairs

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    p <- permutations guests
    guard (not (hasConflict conflicts p))
    return p

-- 4. Result monad with warnings

data Result a = Failure String | Success a [String]
    deriving Show

-- a)

instance Functor Result where
    fmap _ (Failure msg)    = Failure msg
    fmap f (Success x ws)   = Success (f x) ws

instance Applicative Result where
    pure x = Success x []

    Failure msg    <*> _              = Failure msg
    _              <*> Failure msg    = Failure msg
    Success f ws1  <*> Success x ws2  = Success (f x) (ws1 ++ ws2)

instance Monad Result where
    Failure msg >>= _ = Failure msg
    Success x ws1 >>= f = case f x of
        Failure msg     -> Failure msg
        Success y ws2   -> Success y (ws1 ++ ws2)

-- b)
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure msg = Failure msg

-- c)
validateAge :: Int -> Result Int
validateAge age
    | age < 0   = failure "Age cannot be negative"
    | age > 150 = do
        warn "Age is above 150"
        return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges ages = mapM validateAge ages

-- 5. Evaluator with simplification log

-- 6. ZipList — an Applicative that is not a Monad