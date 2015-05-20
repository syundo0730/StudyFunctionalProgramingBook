-- 非負整数値の桁数
digits :: Int -> Int
digits = length . show

-- 数値の2乗
square :: Num a => a -> a
square = (^2)

ultimate :: Int -> String -- 関数ultimateを定義する
ultimate 42 = "人生、宇宙、すべての答え"
ultimate n = show n

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

deeping :: String -> String
deeping s@(' ':' ':_) = "  " ++ s
deeping s@(' ':_)     = " " ++ s
deeping s             = s

--nonExhaustive :: Int -> Int
--nonExhaustive 0 = 0
--nonExhaustive 1 = 1
--nonExhaustive 2 = 2

safeSqrt :: (Ord a, Floating a) => a -> Maybe a
safeSqrt x
 | x < 0 = Nothing
 | otherwise = Just (sqrt x)
