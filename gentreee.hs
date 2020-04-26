factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

fibo  :: Int -> Int
fibo 1 = 0
fibo 2 = 1
fibo n = fibo (n-1) + fibo (n-2)

{- 
data Gentree a = Node a [Gentree a]

labels :: Eq a => Gentree a -> [a]
labels (Node x ts) = x : concat( map labels ts)



howmany :: Eq a => (a-> Bool) -> Gentree a -> Int
howmany n (Node x ts) 
                   | (n == x) = 1 + howmany n (Node ts tss)
                   | otherwise = howmany n (Node ts tss)

-}

factors :: Int -> [Int]
factors n = [ k | k <- [1..n], (n `mod` k == 0)]

--function that applies two argument function to list alternatively 

alternate :: Eq a => (a->b) -> (a->b) -> [a] -> [b]
alternate f g [] = []
alternate f g (x:xs) = f x : alternate g f (xs)


{-
Prelude> iterate (+5) 1
[1,6,11,16,21,26,31,36,4
-}

iterate1 :: (a->a) -> a -> [a]
iterate1 f x = x : iterate1 f re
               where 
                  re = f x






