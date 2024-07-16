data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

exampleTree :: Tree Int
exampleTree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)

-- 定義: 深さ優先探索によるTreeからListへの変換
depthFirst :: Tree a -> [a]
depthFirst Empty = []
depthFirst (Node x left right) = [x] ++ depthFirst left ++ depthFirst right

-- 定義: 幅優先探索によるTreeからListへの変換
breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [t]
  where
    bfs [] = []
    bfs (Empty:ts) = bfs ts
    bfs (Node x left right:ts) = x : bfs (ts ++ [left, right])

-- main関数: 実行と結果の表示
main :: IO ()
main = do
    -- bredth first
    let originalList = breadthFirst exampleTree
    let transformedTree = fmap (*2) exampleTree
    let transformedList = breadthFirst transformedTree
    let listAfterMap = fmap (*2) (breadthFirst exampleTree)
    
    putStrLn "Original Tree:"
    print exampleTree
    putStrLn "\nBreadth First Traversal of Original Tree:"
    print originalList
    putStrLn "\nTree after fmap (*2):"
    print transformedTree
    putStrLn "\nBreadth First Traversal after fmap (*2):"
    print transformedList
    putStrLn "\nBreadth First Traversal and then fmap (*2):"
    print listAfterMap

    putStrLn "\nVerification (Breadth First Traversal after fmap == fmap after Breadth First Traversal):"
    print (transformedList == listAfterMap)

    -- depth first
    let originalList = depthFirst exampleTree
    let transformedTree = fmap (*2) exampleTree
    let transformedList = depthFirst transformedTree
    let listAfterMap = fmap (*2) (depthFirst exampleTree)

    putStrLn "Original Tree:"
    print exampleTree
    putStrLn "\nDepth First Traversal of Original Tree:"
    print originalList
    putStrLn "\nTree after fmap (*2):"
    print transformedTree
    putStrLn "\nDepth First Traversal after fmap (*2):"
    print transformedList
    putStrLn "\nDepth First Traversal and then fmap (*2):"
    print listAfterMap

    putStrLn "\nVerification (Depth First Traversal after fmap == fmap after Depth First Traversal):"
    print (transformedList == listAfterMap)