-- MyList 型の定義
data MyList a = MyNil | MyCons a (MyList a) deriving (Show, Eq)

-- MyList の Functor インスタンス
instance Functor MyList where
    fmap _ MyNil = MyNil
    fmap f (MyCons x xs) = MyCons (f x) (fmap f xs)

-- 例の関数
f :: Int -> Int
f x = x + 1

g :: Int -> Int
g x = x * 2

myList :: MyList Int
myList = MyCons 1 (MyCons 2 (MyCons 3 MyNil))

list :: [Int]
list = [1, 2, 3]

main :: IO ()
main = do
    -- Functor Law 1: fmap id == id
    print $ fmap id myList == id myList
    print $ fmap id list == id list

    -- Functor Law 2: fmap (f . g) == fmap f . fmap g
    print $ fmap (f . g) myList == (fmap f . fmap g) myList
    print $ fmap (f . g) list == (fmap f . fmap g) list
