import Data.List

votes :: [String]
votes = ["Red", "blue", "blue"]

count :: Eq a => a -> [a] -> Int
count a = length . filter (==a)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

res :: Ord a => [a] -> [(Int,a)]
res vs = sort[ ((count v vs), v ) | v <- rmdups vs ]

winner :: Ord a => [a] -> a
winner = snd . last . res


rmempty :: Eq a => [[a]] -> [[a]] 
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]] 
elim x = map (filter (/= x))

{- [(+7) x | x <- [1..10], odd x] 
map (+7) filter ((x `mod` 2) /= 0) [1..10]
-}

{- all even [2,4,6,8] -}
all1 :: Eq a => (a -> Bool) -> [a] -> Bool
all1 p [] = True
all1 p (x:xs) = p x && all1 p xs


dec2int :: [Int] -> Int
dec2int xs = sum [ k*v | (k,v) <- zip (reverse xs) weights]
                  where 
                      weights = iterate (* 10) 1

dec2int1 :: [Int] -> Int
dec2int1 = foldl (\x y -> 10 * x + y) 0



