
data BTree a = Empty | BNode a ( BTree a ) ( BTree a ) deriving (Show)
data Dir = Lft | Rght
type Path = [ Dir ]



trace :: Path -> BTree a -> BTree a
trace [ ] tree = tree
trace path Empty = Empty
trace ( Lft: path ) ( BNode _ left _ )= trace path left
trace ( Rght: path )( BNode _ _ right )= trace path right