-- 定義
eitherImplies :: Either (a -> b) (a -> c) -> (a -> Either b c)
eitherImplies (Left f)  = \a -> Left (f a)
eitherImplies (Right g) = \a -> Right (g a)

-- メイン関数
main :: IO ()
main = do
    let leftImplies :: Int -> Either String Int
        leftImplies = eitherImplies (Left (\x -> show (x + 1)))
    let rightImplies :: Int -> Either String Int
        rightImplies = eitherImplies (Right (\x -> x * 2))
    
    print (leftImplies 5)  -- 出力: Left "6"
    print (rightImplies 5) -- 出力: Right 10
