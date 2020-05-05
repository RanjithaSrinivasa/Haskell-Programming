factorial :: Int -> Int
factorial 0 = 1
factorial n 
           | (n > 0) = n * factorial (n-1) 

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

multipl :: Int -> Int -> Int
multipl m 0 = 1
multipl m n = m * multipl m (n-1)

euclid :: Int -> Int -> Int 
euclid m n 
          | (m == n) = m
          | (m > n) =  euclid (m-n) n
          | otherwise = euclid m (n-m)

and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x && (and1 xs)

concate :: Eq a => [[a]] -> [a]
concate [] = []
concate (x:xs) = x ++ concate xs

replicatef :: Int -> a -> [a]
replicatef 0 a = []
replicatef n a = a : replicatef (n-1) a



nelem :: Eq a => [a] -> Int -> a
nelem (x:xs) 0 = x
nelem (x:xs) n = nelem xs (n-1)

merge1 :: (Ord a) => [a] -> [a] -> [a]
merge1 xs [] = xs
merge1 [] ys = ys
merge1 (x:xs) (y:ys) 
                   | (x <=y) = x : merge1 xs (y:ys)
                   | otherwise = y : merge1 (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] =[]
msort [x] = [x]
msort xs = merge1 (msort firsthalve) (msort secondhalve)
           where
             firsthalve = take ((length xs) `div` 2) xs
             secondhalve = drop ((length xs) `div` 2) xs








