import Prelude hiding (lookup)

-- データ型: キーと値のペア
data KeyValue = KeyValue { key :: Int, value :: String } deriving (Show, Eq)

-- データ型: MyMaybe
data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq)

-- Functorインスタンスの定義
instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)

-- Applicativeインスタンスの定義
instance Applicative MyMaybe where
    pure = MyJust
    MyNothing <*> _ = MyNothing
    (MyJust f) <*> mx = fmap f mx

-- Monadインスタンスの定義
instance Monad MyMaybe where
    return = pure
    MyNothing >>= _ = MyNothing
    (MyJust x) >>= f = f x

-- データ型: Tree
data Tree = Empty | Node KeyValue Tree Tree deriving (Show)

-- 挿入関数: insert
insert :: KeyValue -> Tree -> Tree
insert kv Empty = Node kv Empty Empty
insert kv@(KeyValue k _) (Node kv' left right)
    | k == key kv' = Node kv left right  -- すでにキーが存在する場合は値を更新
    | k < key kv' = Node kv' (insert kv left) right
    | otherwise = Node kv' left (insert kv right)

-- 検索関数: treeLookup
treeLookup :: Int -> Tree -> MyMaybe String
treeLookup _ Empty = MyNothing
treeLookup k (Node (KeyValue key' value') left right)
    | k == key' = MyJust value'
    | k < key' = treeLookup k left
    | otherwise = treeLookup k right

-- main関数の定義
main :: IO ()
main = do
    -- 空の木を作成し、キーと値のペアを挿入する
    let tree = insert (KeyValue 5 "apple") Empty
    let updatedTree1 = insert (KeyValue 3 "banana") tree
    let updatedTree2 = insert (KeyValue 7 "cherry") updatedTree1
    let updatedTree3 = insert (KeyValue 2 "date") updatedTree2

    putStrLn "Updated Tree:"
    print updatedTree3

    -- キーに対応する値を検索する
    putStrLn "Searching for key 3:"
    case treeLookup 3 updatedTree3 of
        MyNothing -> putStrLn "Key not found"
        MyJust value -> putStrLn $ "Value found: " ++ value

    putStrLn "Searching for key 6:"
    case treeLookup 6 updatedTree3 of
        MyNothing -> putStrLn "Key not found"
        MyJust value -> putStrLn $ "Value found: " ++ value

