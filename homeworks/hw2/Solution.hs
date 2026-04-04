module Solution where

import Data.Foldable

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show, Eq)

-- 1. Functor Sequence

instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append s1 s2) = Append (fmap f s1) (fmap f s2)

-- 2. Foldable Sequence

instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append s1 s2) = foldMap f s1 <> foldMap f s2

seqToList :: Sequence a -> [a]
seqToList Empty = []
seqToList (Single x) = [x]
seqToList (Append s1 s2) = seqToList s1 ++ seqToList s2

seqLength :: Sequence a -> Int
seqLength s = length s

-- 3. Semigroup and Monoid for Sequence

instance Semigroup (Sequence a) where
    (<>) :: Sequence a -> Sequence a -> Sequence a
    Empty <> s = s
    s <> Empty = s
    s1 <> s2 = Append s1 s2

instance Monoid (Sequence a) where
    mempty :: Sequence a
    mempty = Empty

-- 4. Tail recursion and sequence search

tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target s = go [s]
  where
    go [] = False
    go (current : rest) = case current of
        Empty        -> go rest
        Single y     -> if target == y then True else go rest
        Append l r   -> go (l : r : rest)

-- 5. Tail recursion and sequence flatten

tailToList :: Sequence a -> [a]
tailToList s = go [s] []
  where
    go [] acc = reverse acc
    go (current : rest) acc = case current of
        Empty      -> go rest acc
        Single x   -> go rest (x : acc)
        Append l r -> go (l : r : rest) acc

-- 6. Tail recursion and reverse Polish notation

data Token = TNum Int | TAdd | TSub | TMul | TDiv
  deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [finalResult] = Just finalResult
    go [] _             = Nothing

    go (TNum n : ts) stack = go ts (n : stack)

    go (TAdd : ts) (y : x : rest) = go ts ((x + y) : rest)
    go (TSub : ts) (y : x : rest) = go ts ((x - y) : rest)
    go (TMul : ts) (y : x : rest) = go ts ((x * y) : rest)

    go (TDiv : ts) (y : x : rest)
        | y == 0    = Nothing
        | otherwise = go ts ((x `div` y) : rest)

    go (_ : _) _ = Nothing

-- 7. Expressing functions via foldr and foldl

-- a)

myReverse :: [a] -> [a]
myReverse xs = foldl (\acc x -> x : acc) [] xs

-- b)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p xs = foldr (\x acc -> if p x then x : acc else []) [] xs

-- c)

decimal :: [Int] -> Int
decimal ds = foldl (\acc d -> acc * 10 + d) 0 ds

-- 8. Run-length encoding via folds

-- a)

encode :: Eq a => [a] -> [(a, Int)]
encode xs = foldr step [] xs
  where
    step x [] = [(x, 1)]

    step x ((y, n) : rest)
        | x == y    = (y, n + 1) : rest
        | otherwise = (x, 1) : (y, n) : rest

-- b)

decode :: [(a, Int)] -> [a]
decode pairs = foldr step [] pairs
  where
    step (x, n) acc = replicate n x ++ acc