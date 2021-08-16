-- Aufgabe 1a
rev :: [Int] -> [Int]
rev = reverse -- bruh

rev2 :: [Int] -> [Int]
rev2 [] = []
rev2 (x : xs) = rev xs ++ [x]

-- Aufgabe 1b
isOrd :: [Int] -> Bool
isOrd [x] = True
isOrd (x : y : xs)
    | x > y = False
    | x <= y = isOrd (y : xs)

-- Aufgabe 1c
merge :: [Int] -> [Int] -> [Int]
-- merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
    | x > y = y : merge (x : xs) ys
    | x <= y = x : merge xs (y : ys)

-- Aufgabe 1d
fibs :: [Int]
fibs = [0, 1] ++ [(fibs !! length fibs -1) + (fibs !! (length fibs -2))]

-- fibs2 :: [Int]
-- fibs2 = f' 0
--     where
--         f' :: Int -> [Int]
--         f' i = fib i : f' (i+1)

--Aufgabe 2a
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x : xs) (y : ys)
    | x == y = isPrefix xs ys
    | otherwise = False

--Aufgabe 2b
countPattern :: String -> String -> Int
countPattern _ [] = 0
countPattern [] (y : ys) = length (y : ys) + 1
countPattern (x : xs) (y : ys)
    | isPrefix (x : xs) (y : ys) = countPattern (x : xs) ys + 1
    | otherwise = countPattern (x : xs) ys

data BinTree = Branch Int BinTree BinTree | Nil deriving (Show)

--Aufgabe 3a
myTree :: BinTree
myTree = Branch 0 Nil (Branch 3 (Branch 1 Nil Nil) (Branch 5 Nil Nil))

--Aufgabe 3b
binId :: BinTree -> BinTree -> Bool
binId Nil Nil = True
binId _ Nil = False
binId Nil _ = False
binId (Branch x xs xss) (Branch y ys yss)
    | x == y = binId xs ys && binId xss yss
    | otherwise = False

--Aufgabe 3c
insert :: BinTree -> [Int] -> BinTree
insert x [] = x
insert Nil (x : xs) = insert (Branch x Nil Nil) xs
insert (Branch i b1 b2) (x : xs)
    | i > x = insert (Branch i (insert b1 [x]) b2) xs
    | otherwise = insert (Branch i b1 (insert b2 [x])) xs

--Zusatz 1a
pack :: [Char] -> [[Char]]
pack [] = []
pack [x] = [[x]]
pack (x : y : xs)
    | x == y = (x : [y]) : pack xs