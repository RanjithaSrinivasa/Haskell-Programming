import Data.Char
import System.IO



exists :: Eq a => a -> [a] -> Bool
exists n xs = or [if (x == n) then True else False | x <- xs]

{-
take1 :: Eq a => Int -> [a] -> [a]
take1 n xs = [xs!!i|i <- [0..n-1], i < length xs]
-}

tak ::Eq a => Int -> [a] -> [a] 
tak n xs = [ x | (x,i) <- zip xs [1..n]]

fun1 :: [a] -> [b] -> (a->Bool) -> (b->c) -> [c]
fun1 xs ys p q = map (\(x,y) -> q y) (filter (\(x,y) -> p x) (zip xs ys))


getInteger :: IO Integer
getInteger = do line <- getLine
                return (read line :: Integer)


countdown :: Int -> IO ()
countdown num = if(num < 0) then

                  do
                    putStrLn ("End")
                else 
                    do
                      putStrLn(show(num))
                      countdown(num-1)

