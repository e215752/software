xyzw :: Integer -> Integer
xyzw w = (x * y) + z + w
  where
    x = 1
    y = 2
    z = 3

main :: IO ()
main = do
    print(xyzw(4))
