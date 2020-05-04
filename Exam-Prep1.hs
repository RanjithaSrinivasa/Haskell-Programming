{- 
foldr' :: (a->b->b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x(foldr' f v xs)


foldl' :: (a->b->a)->a->[b]->a
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

-}

{- finding factors of given number -}
divisors :: Int -> [Int]
divisors 0 = []
divisors 1 = [1]
divisors n = [x | x <- [1..n], n `mod` x == 0]

{- find if number is prime-}
prime1 :: Int -> Bool
-- prime1 0 = False
-- prime1 1 = False
prime1 n = (length (divisors n) == 2)


{- function to return a list if it matches given number -}
matches :: Int -> [Int] -> [Int]
matches n xs = [x | x <- xs, (x == n)]

{- return true if elem is present in list -}
elem1 :: Int -> [Int] -> Bool
elem1 n [] = False
elem1 n (x:xs) 
             | (n==x) = True 
             | otherwise = elem1 n xs


ordpairs :: Int -> [(Int,Int)]
ordpairs 0 = []
ordpairs 1 = [(1,0)]
ordpairs n = take n [(x,y) | y <- [1..], x <- [1..y], x > 0,  x < y]

 
pmatch :: [Int] -> Int
pmatch (x:y:xs) = x+y
pmatch [x] = x
pmatch _ = 0

elemNum :: Int -> [Int] -> Int
elemNum n [] = 0
elemNum n xs = length [x | x <- xs, x == n ]

elemNum1 ::Int -> [Int] -> Int
elemNum1 n [] = 0
elemNum1 n (x:xs) 
                | (n==x) = 1 + elemNum1 n xs 
                | otherwise = elemNum1 n xs

issorted :: [Int] -> Bool
issorted [] = True
issorted (x:y:xs) 
                 | (x<y) && issorted (y:xs) = True
                 | otherwise = False 

zip3' :: Eq a => [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs

{- List comprehension has no base case -}
zip31 :: Eq a => [a] -> [b] -> [c] -> [(a,b,c)]
zip31 xs ys zs = [(x,y,z) | (x,(y,z)) <- zip xs (zip ys zs) ]

{-
zip31 xs ys zs = [ (x,y) | x <- xs, y <- ls let ls = [(y,z) | y <- ys, z <- zs]]
-}


all1 :: Eq a => (a -> Bool) -> [a] -> Bool
all1 f [] = True
all1 f (x:xs) = (f x) && all1 f xs

all12 :: Eq a => (a -> Bool) -> [a] -> Bool
all12 f [] = False
all12 f (x:xs) = foldr ((&&) . f) True (x:xs)

iter' :: Eq a => Int -> (a -> a) -> a -> a
iter' 0 f x = x
iter' n f x = f (iter' (n-1) f x)

sum' :: Int -> Int
sum' 0 = 0
sum' n = (n*n) + sum' (n-1)

sum1' :: Int -> Int
sum1' n = foldr (+) 0 (map (\x -> x*x) [1..n])

{-

composeList:: [(a -> a)] -> (a -> a)
composeList [] x = x
composeList (f:fs) x = f (composeList fs x)

composeList' :: [(a -> a)] -> (a -> a)
composeList' (f:fs) x = foldr (.) x (f:fs) 
-}

composeList :: [(a -> a)] -> (a -> a)
composeList = foldr (.) id

{-
total :: (Int -> Int) -> (Int -> Int)
total f 0 = 0
total f n = f n + total f (n-1) 
-}

total :: (Int -> Int) -> (Int -> Int)
total f = \n -> sum (map f [0..n])

total' :: (Int -> Int) -> (Int -> Int)
total' f = \n -> sum (map f [0..n])

data Name = Short String | Fullname String String

{-
title :: Name -> String
title (Fullname first last) = "Esteemed" ++ " " ++ first ++  " " ++ last
title (Short n) = n


title :: Name -> String
title (Fullname first last) = "Esteemed" ++ " " ++ first ++ " " ++ last
title (Short n) = n
-}

data BTree a = Empty | BNode a (BTree a) (BTree a) deriving (Show)

inTree :: Eq a => a -> BTree a -> Bool
inTree x Empty = False
inTree x (BNode a left right) 
                            | (x==a) = True
                            | otherwise = (inTree x left) || (inTree x right)

autumn :: BTree a -> BTree a
autumn (BNode v Empty Empty) = Empty
autumn (BNode v left right) = BNode v (autumn left) (autumn right)

getFloat :: IO Float
getFloat = do line <- getLine
              return (read line :: Float)

findMax :: IO ()
findMax = do putStrLn ("Enter Three Floats")
             x <- getFloat
             y <- getFloat
             z <- getFloat
             putStrLn ("max is " ++ show (maximum [x,y,z] ) )

{-
printPresum :: [Int] -> IO ()
printPresum [] = []
printPresum (x:xs) = x + printPresum 
-}

          



