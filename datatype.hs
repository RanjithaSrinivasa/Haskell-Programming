data Move = South | North | East | West deriving (Show)

{-
move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x, y-1)
move West (x,y) = (x+1, y)
move East (x,y) = (x-1,y)

move :: [Move] -> Pos -> Pos
move [] p = []
move (x:xs) p = x p (move xs p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Maybe = Nothing | Just a

safediv :: Int -> Int -> Maybe
safediv m 0 = Nothing
safediv m n = Just (m `div` n )

-}

data Shape = Circle Float | Rect Float Float
             deriving (Show, Eq)


square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area ( Circle r) = pi * r * r
area (Rect x y) = x * y

data Nat = Zero | Succ Nat deriving Show


natFromInteger :: Integer -> Nat
natFromInteger 0 = Zero
natFromInteger n = Succ . natFromInteger $ n - 1


add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult Zero n = Zero
mult m (Succ n) = add m (mult m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)

{-
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf a) = (x==a)
occurs x (Node l a r) = case compare x a of
 	                    LT -> occurs x l
                            GT -> occurs x r
                            EQ -> True


-}

                            

                        
{-
occurs x (Node left a right) 
                            | (x == a) = True
                            | otherwise = occurs x left || occurs x right
-}





