{- getInteger and getFloat are basic functions used to get int/float numbers -}
getInteger :: IO Integer
getInteger = do line <- getLine
                return (read line :: Integer)

getFloat :: IO Float
getFloat = do line <- getLine
              return (read line :: Float)

{- simple greet function -}
greet :: IO Int
greet = do putStrLn "what is your name"
           name <- getLine
           putStrLn ("My name is " ++ name)
           return (length name)


{- take float numbers as input until a value >= baseline number is entered 
also display input taken

smallerthan :: Float -> IO (Float, [Float])
smallerthan f = do x <- getFloat
                   if (x >= f)
                   then return (x, [])
                   else do (v, rest) <- smallerthan f
                            return (v, [x: rest])

-}

{- display a string vertically -}

display :: String -> IO Int
display (c:cs) = do putStrLn [c]
                    n <- display cs
                    return (n+1)

{- display a string vertically using sequence -}
display' ::String -> IO Int
display' cs = do sequence_[putStrLn [c] | c <- cs]
                 return (length cs)


{- display words vertically from a line -}

prompt :: IO ()
prompt = do putStrLn ("Please enter a line of text : ")
            line <- getLine
            sequence_ [putStrLn l | l <- words line]




{- have to correct it - error
promptver2 :: IO ()
promptver2 = do putStrLn ("Please enter a line of text : ")
                 line <- getLine
                 sequence_ [display n w | (n,w) <- zip [1..] (words line)]
                     where
                         display n w = putStrLn ( show n ++ "." ++ w )


-}


