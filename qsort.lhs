> sumof :: [Int] -> Int
> sumof [] = 0
> sumof (x:xs) = x + sumof xs

> sumofsquares :: [Int] -> Int
> sumofsquares lst = sumof [x^2 | x <- lst] 

Quicksort
          
> qsort    :: [Int] -> [Int]
> qsort []     = [] 
> qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
>     where
>     smaller = [ a | a <- xs, a<= x]
>     larger  = [ b | b <- xs, b > x]

  
         
                