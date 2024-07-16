-- 自分で MonoidalFunctor 型クラスを定義する
class MonoidalFunctor t where
    unit :: t ()
    phi :: (t a, t a) -> t (a, a)

-- MyMaybe 型を定義する
data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq)

instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)

instance Applicative MyMaybe where
    pure = MyJust
    MyNothing <*> _ = MyNothing
    _ <*> MyNothing = MyNothing
    MyJust f <*> MyJust x = MyJust (f x)

-- MyMaybe を MonoidalFunctor のインスタンスとして定義する
instance MonoidalFunctor MyMaybe where
    unit = MyJust ()
    phi (MyJust a, MyJust b) = MyJust (a, b)
    phi _ = MyNothing


