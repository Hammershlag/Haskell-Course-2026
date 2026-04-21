import Data.Map (Map)
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

-- 4. Result monad with warnings

-- 5. Evaluator with simplification log

-- 6. ZipList — an Applicative that is not a Monad