xyzw w =
    let x = 1
        y = 2
        z = 3
    in
        (x * y) + z + w

main :: IO ()
main = print (xyzw 4)
