x1 :: t1 -> t1
x1 x = x

x2 :: t2 -> t2
x2 x = x

x3 :: t3 -> t3
x3 = x2

x4 :: t4 -> t4
x4 = x3

main :: IO ()
main = do
    let result1 = x1 "Hello"
    let result2 = x2 42
    let result3 = x3 True
    let result4 = x4 3.14
    print result1
    print result2
    print result3
    print result4