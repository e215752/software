import Control.Monad
import Control.Applicative


data MyMaybe a = MyNothing
   |  MyJust a
   deriving (Show, Eq)

instance Functor MyMaybe where
    fmap f MyNothing  = MyNothing
    fmap f (MyJust x)  = MyJust (f x)
    

instance Applicative MyMaybe where
    pure x = MyJust x
    (<*>) MyNothing x = MyNothing
    (<*>) (MyJust f) MyNothing = MyNothing
    (<*>) (MyJust f) (MyJust x) = MyJust (f x)
   
class Monad1 t where
        eta :: a -> t a
        mu ::  t ( t a ) -> t a

instance Monad1 MyMaybe where
   eta x = MyJust x
   mu MyNothing = MyNothing
   mu (MyJust MyNothing) = MyNothing
   mu (MyJust (MyJust x)) = MyJust x

instance Monad MyMaybe where
    return x = MyJust x
    (>>=) MyNothing f = MyNothing
    (>>=) (MyJust x) k = k x

mydiv x 0 = MyNothing
mydiv x y = MyJust (x / y)

x1 = mydiv 10 2
x2 = mydiv 10 0

x20 = do
  x <- mydiv 10 2
  return x

--x21 = do
--   x <- mydiv 10 2
--   y <- mydiv 10 2
--   return x + y

--x21 = do
--  x <- mydiv 10 2
--  return $ do 
--     y <- mydiv 10 2
--     return y

x21 = do
  x <- mydiv 10 2
  do 
    y <- mydiv 12 0
    return $ x + y


x22 =  pure (+)  <*> MyJust 1 <*> MyJust 2 
x23 =  (+)  <$> MyJust 1 <*> MyJust 2 

x24 = pure (+)  <*> mydiv 3 1 <*> mydiv 3 0
x25 = pure (+)  <*> mydiv 3 1 <*> mydiv 3 2
x26 = (\x y z -> sum [ x , y , z])  <$> mydiv 3 1 <*> mydiv 3 2 <*> mydiv 3 0

class MonoidalFunctor t where
        unit :: t ()
        phi ::  ( t a , t a ) -> t ( a , a )

instance MonoidalFunctor MyMaybe where
  unit = MyNothing
  phi (MyJust x , MyJust y)  = MyJust (x , y)
  phi _  = MyNothing

x3 = fmap (* 10) x1
x4 = fmap (* 10) x2

x5 = (MyJust (* 12)) <*> x1
x6 = (MyJust (* 12)) <*> x2

x7 :: Num a => MyMaybe a
x7 = return 10

-- x8 :: MyMaybe a -> MyMaybe a
x8' x = (fmap (* 10) x )  >>= (\x' -> return x')

-- x9 ::  MyMaybe a -> MyMaybe a
x8 x = do
   x' <- fmap (* 10) x
   return x'

x9' x = x >>= (\x' -> mydiv 10 x')

x9 x = do
   x' <- x
   mydiv 10 x'

-- x9' x = do
--    x' <- (mydiv 10) <*> x 
--    return x'


x10 =  x8 (MyJust 10)
x11 =  x8 MyNothing

x12 =  x9 (MyJust 10)
x13 =  x9 MyNothing





