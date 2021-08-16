data BinTree a = Branch a (BinTree a) (BinTree a) | Leaf a deriving (Show)

--Aufgabe 1a
someTree :: BinTree Int
someTree = Branch 4 (Branch 1 (Leaf 1) (Leaf 3)) (Branch 6 (Branch 5 (Leaf 3) (Leaf 6)) (Leaf 7))

--         4
--     /        \
--    1          6
--  /   \      /   \
-- 1     3    5     7
--          /   \
--         3     6

--Aufgabe 1b
depth :: BinTree a -> Int
depth (Leaf _) = 1
depth (Branch n t1 t2) = 1 + min (depth t1) (depth t2)

--Aufgabe 1c
paths1 :: BinTree a -> BinTree [a]
paths1 = pathsWithPrefix []
 where
  pathsWithPrefix xs (Leaf x) = Leaf (xs ++ [x])
  pathsWithPrefix xs (Branch x t1 t2) = Branch (xs ++ [x]) (pathsWithPrefix (xs ++ [x]) t1) (pathsWithPrefix (xs ++ [x]) t2)


--Aufgabe 1d
tmap :: (a -> b) -> BinTree a -> BinTree b
tmap f (Leaf x) = Leaf (f x)
tmap f (Branch x t1 t2) = Branch (f x) (tmap f t1) (tmap f t2)


--Probeklausur 1
values :: [Int] -> [Int]
values [] = []
values (x : xs) = if occurs x xs' then xs' else x:xs'
  where
    xs' = values xs
    occurs _ [] = False
    occurs i (x:xs) = i == x || occurs i xs


--Probeklausur 2
histogram :: [Int] -> [Int]
histogram xs = histogram' 1 xs 
  where  
    histogram' i xs = count i xs : histogram' (i+1) xs
    count _ [] = 0
    count i (x:xs)
      | i == x = 1 + count i xs
      | otherwise = count i xs

