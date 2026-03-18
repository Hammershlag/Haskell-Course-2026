{-# LANGUAGE BangPatterns #-}

module Solution where

import Data.Function

-- 1. Goldbach Primes

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | even n = False
    | otherwise = all (/=0) [n `mod` i | i <- [2..s]]
    where
        s = n & fromIntegral & sqrt & floor

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
    | n < 4 = []
    | otherwise = [(p,q) | p <- [2..n], q <- [2..n], p + q == n, isPrime p, isPrime q, p <= q]

-- 2. Coprime Pairs

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = [(x,y) | x <- xs, y <- xs, gcd x y == 1, x < y, x > 0, y > 0]

-- 3. Sieve of Eratosthenes

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [p | p <- xs, p `mod` x /= 0, p >= 2]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime' :: Int -> Bool
isPrime' !n = primesTo n & any (== n)

-- 4. Matrix Multiplication

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul [] _ = []
matMul _ [] = []
matMul a b =
    [[sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1]]
        | j <- [0..n-1]]
        | i <- [0..m-1]]
    where
        m = length a
        p = length b
        n = length (head b)

-- 5. Permutations

permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs
    | k < 0 = []
    | k > length (xs) = []
    | otherwise = [ x:ps | (x, rest) <- picks xs, ps <- permutations (k-1) rest ]

picks :: [a] -> [(a, [a])]
picks [] = []
picks (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- picks xs ]

-- 6. Hamming Numbers

merge :: Ord a => [a] -> [a] -> [a]
marge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | y < x = y : merge (x:xs) ys
    | otherwise = x : merge xs ys

hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))

-- 7. Integer Power with Bang Patterns

power :: Int -> Int -> Int
power b e = go e 1
    where
        go :: Int -> Int -> Int
        go 0 !acc = acc
        go n !acc = go (n-1) (acc*b)
