mergesort ::  (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort xs = merge (mergesort as) (mergesort bs)
                   where 
                   as = take ((length xs) `div` 2 ) xs
                   bs = drop ((length xs ) `div` 2 ) xs

merge :: (Ord a) => [a] -> [a]-> [a]
merge [a] [] = [a]
merge [] [b] = [b]
merge (a:as) (b:bs) = if a <= b then a : merge as (b:bs)
                      else b : merge (a:as)bs


doubleme :: Int -> Int
doubleme x = x + x

doubleus x y  = x * 2  + y * 2

doublesmall x = if x < 100
                then x * x
                else x


