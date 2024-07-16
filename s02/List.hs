import Control.Monad
import Control.Applicative


data MyList a = Nil
   |  Cons a (MyList a)              
   deriving (Show, Eq)

myLength Nil = 0
myLength (Cons _ tail) = 1 + ( myLength tail )


myAppend Nil t = t
myAppend (Cons h t ) t1 = Cons h ( myAppend t t1 )

toList Nil  = []
toList (Cons h t )  =  h :  toList t 


myHead  (Cons h _)  = h
myTail  (Cons _ t)  = t

l0 = Nil
l1 = Cons 1 l0
l2 = Cons 2 l1
l3 = Cons 3 l1

l4 = myAppend l2 l3

l5 = myHead l4

l6 = myHead l0

append x y =
    if x == Nil
           then y
           else
              Cons (myHead x) (append (myTail x) y )


append3 x y = case x of
    Nil -> y
    (Cons h t) -> Cons h ( append3 t y)

rev x = rev1 x []
rev1 [] x = x
rev1 (x:xs) y = rev1 xs (x:y)

myrev x = myrev1 x Nil
myrev1 Nil x = x
myrev1 (Cons x xs) y = myrev1 xs (Cons x y)

-- write tail call version of append

instance Functor MyList where
        fmap _ Nil = Nil
        fmap f (Cons h t)  = Cons (f h) ( fmap f t )

test1 = 
    let numList = Cons 1 ( Cons 2 Nil ) in
       fmap show numList


instance Applicative MyList where
    pure  _ = Nil
    (<*>) Nil x = Nil
    (<*>) (Cons f tf) list = myAppend (fmap f list) ( tf <*> list )

l7 =     (Cons ( * 2 ) ( Cons ( + 5 ) Nil )) <*> ( Cons 5 ( Cons 7 ( Cons 9 Nil )))
    
newtype MyZipList a = MyZipList { getMyList :: MyList a }

myZipWith f Nil _ =  Nil
myZipWith f (Cons fs ft) (Cons xs xt)  = Cons ( f fs xs) ( myZipWith f ft xt )

instance Functor MyZipList where
    fmap f (MyZipList x ) = MyZipList ( fmap f x )

instance Applicative MyZipList where
    pure x                            = MyZipList Nil
    (MyZipList fs) <*> (MyZipList xs) = MyZipList (myZipWith ($) fs xs) 


l8 =     MyZipList (Cons ( * 2 ) ( Cons ( + 5 ) Nil )) <*> MyZipList ( Cons 5 ( Cons 7 ( Cons 9 Nil )))

test2 = getMyList l8

b0 = Cons  True ( Cons False Nil )
b1 = fmap not b0

myListOr0 Nil x = Nil
myListOr0 (Cons h t) x = Cons (myListOr1 h x) (myListOr0 t x )

myListOr1 x Nil  = Nil
myListOr1 x (Cons y t) = Cons (x || y) (myListOr1 x t )

myListOr2 x y = fmap (\y' -> fmap (\x' -> x' || y' ) x) y

flatMyList Nil = Nil
flatMyList (Cons Nil t) = flatMyList t
flatMyList (Cons (Cons h t1) t) = Cons h ( flatMyList (Cons t1 t ) )

b2 = flatMyList ( myListOr2 b0 b1 )

myListOr x y = flatMyList $ myListOr2  x y 


instance Monad MyList where
    return x                            = Cons x Nil
    (>>=) x f =  flatMyList ( fmap f x )

b3 = b0 >>= ( \x -> fmap (\y -> x || y ) b1 )


f1 b0 b1 = b0 >>= ( \x -> fmap (\y -> x || y ) b1 )

f2 b0 b1 = do
        x <-  b0
        fmap (\y -> x || y ) b1




