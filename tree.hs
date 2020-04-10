
{- Defining Binary tree data type-}
data BTree a = Empty | BNode a ( BTree a ) ( BTree a ) deriving (Show)

{- Defining direction/path data type -}
data Dir = Lft | Rght
type Path = [ Dir ]


{- trace function which will take a Path p and a BTree as input, 
returns the subtree that you arrive at by tracing the path in tree.
-}

trace' :: Path -> BTree a -> BTree a
trace' [ ] tree = tree
trace' path Empty = Empty
trace' ( Lft: path ) ( BNode _ left _ )= trace' path left
trace' ( Rght: path )( BNode _ _ right )= trace' path right


{- haveAny p tree determines whether any of the labels in tree
   satisfy the predicate p
-}

haveany:: (a -> Bool) -> BTree a -> Bool
haveany p Empty = False
haveany p ( BNode a left right )  = p a ||  haveany p left || haveany p right


{- Inorder tree traversal 
inorder (BNode 2(BNode 3 (Empty) (BNode 5 Empty Empty)) (BNode 4 Empty Empty))
-}

inorder :: BTree a -> [a]
inorder Empty = []
inorder ( BNode a left right ) = ( inorder left ) ++ [a] ++ ( inorder right )


{- Height of a tree 
height' (BNode 2(BNode 3 (Empty) (BNode 5 Empty Empty)) (BNode 4 Empty Empty))
-}

height' :: BTree a -> Int
height' Empty = 0
height' ( BNode a left right ) = max lft rght
                                 where
                                     lft = 1 + height' left
                                     rght = 1 + height' right

{- Balanced tree 
balanced (BNode 2(BNode 3 Empty Empty) (BNode 4 Empty Empty))
-}

balanced :: BTree a -> Bool
balanced Empty = False
balanced (BNode a left right) 
                            | ((height' left - height' right) <= 0 ) = True
                            | otherwise = False

{- Rotate tree 
 rotateright (BNode 2(BNode 3 (Empty) (BNode 5 Empty Empty)) (BNode 4 Empty Empty))
-}
rotateright :: BTree a -> BTree a
rotateright Empty = Empty
rotateright (BNode a Empty right) = error "cannot rotate if left subtree is empty" 
rotateright (BNode a (BNode leftElem leftleftSubTree leftrightSubTree) right ) = ( BNode leftElem leftleftSubTree (BNode a leftrightSubTree right) )


{- count the number of leaves in a tree 
leaves (BNode 2(BNode 3 (Empty) (BNode 5 Empty Empty)) (BNode 4 Empty Empty))
-}

leaves :: BTree a -> Int
leaves Empty = 0
leaves ( BNode a Empty Empty) = 1
leaves (BNode a left right ) = leaves left + leaves right




