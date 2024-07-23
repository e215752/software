-- 定義
eitherToA :: Either a a -> a
eitherToA (Left a)  = a
eitherToA (Right a) = a

-- メイン関数
main :: IO ()
main = do
    print (eitherToA (Left 42))    -- 出力: 42
    print (eitherToA (Right 99))   -- 出力: 99
