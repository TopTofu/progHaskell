data RoseTree = Node Int [RoseTree] deriving (Show)

--Aufgabe 1a
countLeaves :: RoseTree -> Int
countLeaves (Node _ []) = 1
countLeaves (Node a [x]) = countLeaves x
countLeaves (Node a (x : xs)) = countLeaves x + countLeaves (Node a xs)

someTree :: RoseTree
someTree = Node 1 [Node 2 [Node 5 [], Node 4 []], Node 3 [], Node 6 []]

--        1
--      / | \
--     2  3  6
--    / \
--   5   4

--Aufgabe 1b
evenNodes :: RoseTree -> Bool
evenNodes (Node _ []) = True
evenNodes (Node _ [x]) = False
evenNodes (Node a (x : y : xs)) = evenNodes x && evenNodes y && evenNodes (Node a xs)

--Aufgabe 2
f :: [Int] -> Int
f [] = 0
f (x : xs) = foldr (*) 1 (map sq (filter even (x : xs)))
 where
  sq x = x * x

--Aufgabe 3
foldright :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldright _ z [] = z
foldright f z (x:xs) = f x (foldright f z xs)


foldleft :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldleft f a [] = a
foldleft f a [x] = f a x
foldleft f a (x:xs) = foldleft f (f a x) xs

--Zusatz 1a
join :: [String] -> String
join [] = ""
join [x] = x
join (x : xs) = x ++ [' '] ++ join xs

--Zusatz 1b
unjoin :: String -> [String]
unjoin "" = []
unjoin (x : xs)
  | x == ' ' = unjoin xs
  | otherwise = [x] : unjoin xs