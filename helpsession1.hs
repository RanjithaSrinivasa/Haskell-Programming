countdown :: Int -> [(Int, Int)]
countdown k 
    | (k <= 0) = []
    | otherwise = helper k
    where
        helper :: Int -> [(Int, Int)]
        helper k
            | (k==1) = [(0,0)]
            | otherwise = ((2*(k-1)),(k-1)) : helper (k-1)



remove3p :: [Int] -> [Int]
remove3p (x:y:z:xs) = y:z:remove3p xs
remove3p [x,y] = [y]
remove3p _ = []

