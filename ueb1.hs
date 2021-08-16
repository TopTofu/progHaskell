module Ueb1 where

main :: IO ()
main = do
    return ()

-- Aufgabe 2 (a)
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

-- Aufgabe 2 (b)
sumFacs :: Int -> Int -> Int
sumFacs n m
    | n > m = 0
    | n == m = fac m
    | otherwise = sumFacs n (m -1) + fac m

-- Aufgabe 3
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

-- Zusatz 1
numOfFullTreesWithXNodes :: Int -> Int
numOfFullTreesWithXNodes x
    | even x = 0
    | otherwise = (x -1) `div` 2