import System.IO
import Data.Char

{-
getLine1 :: IO String
getLine1 = do x <- getChar
             if x == '\n' then
                return []
             else 
                do xs <- getLine1
                   return (x:xs)
-}

putStr1 :: String -> IO ()
putStr1 [] = return ()
putStr1 (x:xs) = do putChar x
                    putStr1 xs

putStrLn1 :: String -> IO()
putStrLn1 xs = do putStr1 xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStrLn "Enter line" 
            line <- getLine
            putStrLn (show (length line))


hangman :: IO ()
hangman = do putStrLn "Enter a word"
             line <- sgetLine
             putStrLn " Guess the word"
             play line



sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar 
           hSetEcho stdin True 
           return x


play :: String -> IO ()
play xs = do putStrLn "?"
             line <- getLine
             if (line == xs) then
                 putStrLn "Congrats"
             else
                 do putStrLn (match xs line)

                    play xs

match :: String -> String -> String
match xs ys = [if (x `elem` ys) then x else '-' | x <- xs]


getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     putChar '\n' 
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt

newline :: IO ()
newline = putChar '\n'
            
getFloat :: IO Float
getFloat = do line <- getLine
              return (read line :: Float)

adder :: IO()
adder = do putStrLn "How many numbers"
           n <- getLn
           nums <- getnums (read n :: Int)
           putStr ("total is" ++ show (sum nums))

getnums :: Int -> IO [Int]
getnums 0 = return []
getnums n = do
               cs <- getLn
               let num = read cs :: Int
               nums <- getnums (n-1)
               return (num:nums)

getLn :: IO String
getLn = do
          x <- getChar
          if x == '\n' then return []
          else
              do
                xs <- getLn
                return (x:xs)




                


