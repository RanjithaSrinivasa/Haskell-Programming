--replicate
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a =  a : replicate' (n-1)  a

--reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--zip
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys


--repeat
repeat' :: a -> [a]
repeat' a = a : repeat' a


--take
take' :: Int -> [a] -> [a]
take' n [] = []
take' n _ 
          | n <= 0 = []
take' n (x:xs) = x : take' (n-1) xs


elem' :: (Eq a) => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs)
      | n == x = True
      | otherwise = elem' n xs