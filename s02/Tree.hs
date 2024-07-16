
data Tree a = Node (Tree a) (Tree a)
       |       Leaf a
   deriving (Show)

flatten n = case n of
    Node s t ->  (flatten s)++(flatten t)
    Leaf v ->  [v]

-- treeMap is a Functor
-- treeMap :: (t -> a) -> Tree t -> Tree a

treeMap f (Leaf a ) =  Leaf (f a)
treeMap f (Node  a b ) =  Node (treeMap f a) (treeMap f b)

-- Natrual Transformation

fun_g x = treeMap (+ 1) x
fun_f x = treeMap (* 2) x

nat_a (Leaf x)   = Leaf ((x - 1) * 2)
nat_a (Node x y) = Node (nat_a  x) (nat_a y)

-- G(f)t(A) = t(B)F(f)
--   fun_g x nat_a

--   nat_a . fun_g == fun_f

x = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

