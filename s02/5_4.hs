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

-- 単位関数: eta
eta :: a -> MyMaybe a
eta = MyJust

-- 結合関数: mu
mu :: MyMaybe (MyMaybe a) -> MyMaybe a
mu MyNothing = MyNothing
mu (MyJust MyNothing) = MyNothing
mu (MyJust (MyJust x)) = MyJust x

-- etaの自然変換の例
etaExample :: IO ()
etaExample = do
    let x = 5
    let f = (+1)
    print (fmap f (eta x) == eta (f x))  -- MyJust 6 == MyJust 6

-- muの自然変換の例
muExample :: IO ()
muExample = do
    let f :: Int -> Int
        f x = x + 1
    let nested :: MyMaybe (MyMaybe Int)
        nested = MyJust (MyJust 5)
    print (mu (fmap (fmap f) nested) == fmap f (mu nested))  -- MyJust 6 == MyJust 6
    print (mu (fmap eta (MyJust 5)) == MyJust 5)  -- MyJust 5 == MyJust 5

-- main関数の定義
main :: IO ()
main = do
    -- etaの自然変換の例
    etaExample

    -- muの自然変換の例
    muExample

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
