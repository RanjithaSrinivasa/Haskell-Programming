quicksort :: [Int] -> [Int]
quicksort [x] = [x]
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
                   where
                   smaller = [a | a <- xs, a <= x]
                   larger = [b | b <- xs, b > x]