import Data.Char


findpairs :: Eq a => a -> [(a,b)] -> [b]
findpairs k' [] = []
findpairs k' xs = [v | (k,v) <- xs, k == k']

{-

pairs1 :: Eq a => [a] -> [(a,a)]
pairs1 [] = []
pairs1 xs = [zip xs (tail xs)]
-}


sorted ::  [Int] -> Bool
sorted [] = True
sorted (x:y:xs) 
              | (x < y) && sorted (y:xs) = True
              | otherwise = False

positions :: Eq a => a -> [a] -> [Int]
positions a xs = [k | (k,v) <- zip [0..] xs, a == v]


count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']


lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']


isLower' :: Char -> Bool
isLower' c 
         | (c >= 'a' && c <= 'z') = True
         | otherwise = False


char1 :: Char -> String -> Int
char1 a xs = length [x | x <- xs, a == x]

lettoint :: Char -> Int
lettoint c = ord c - ord 'a'

inttolet :: Int -> Char
inttolet n = chr (n + ord 'a')


shift :: Int -> Char -> Char
shift n c 
        | isLower c = inttolet ((lettoint c + n) `mod` 26)
        | otherwise = c

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [ percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

grid1 :: Int -> Int -> [(Int,Int)]
grid1 m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int,Int)]
square n = [(x,y) | x <- [0..n], y<-[0..n], x /= y]

replicate1 :: Eq a => Int -> a -> [a]
replicate1 n a = [a | _ <- [1..n]]

pyth :: Int -> [(Int, Int, Int)]
pyth n = [(x,y,z) |  z <- [1..n], y <- [1..n], x <- [1..n], (z^2 == x^2 + y^2)]


listcomp :: [Int] -> [Int] -> [(Int,Int)]
listcomp xs ys = concat [[(1,x) | x <- [3,4]], [(2,y) | y <- [3,4]]]


scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | x <- xs, y <- ys]

