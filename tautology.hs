{- defining proposition data type -}
data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop


{- Propositions -}

p1:: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A')(Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A')(Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Substitutions

type Subst = Assoc Char Bool

type Assoc k v = [(k,v)]

find' :: Eq k => k -> Assoc k v -> v
find' k t = head [v | (k',v) <- t, k == k']

--tautology

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find' x s
eval s (Not p) = not (eval s p)
eval s (And p q) = (eval s p) && (eval s q)
eval s (Imply p q) = (eval s p <= eval s q)

{- finding the list of variables -}
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

{- generating the lists of boolean value combinations with given int value -}
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

{-
rmdups :: Eq a => [a] -> [a]
rmdups [] =[]
rmdups (x:xs) = x : (rmdups (remove x xs))
    where
        remove :: Eq a => [a] -> [a]
        remove x [] = []
        remove x (y:ys) 
            |  x == y = remove x ys
            | otherwise = y : (remove x ys)

-}

{- remove duplicates -}
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

{- substituting variables with thier boolean values -}
subs :: Prop -> [Subst]
subs p =  map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

{- evaluating propositions eg : tautology p1 -}
tautology :: Prop -> Bool
tautology p = and [eval s p | s <- subs p]





