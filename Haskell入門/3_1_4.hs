xyzw :: Integer -> Integer
xyzw w = xyz 1 2 3 + w
  where
    xyz :: Integer -> Integer -> Integer -> Integer
    xyz x y z = (x * y) + z

main :: IO ()
main = do
    print(xyzw(4))
