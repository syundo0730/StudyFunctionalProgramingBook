module Lazy where

-- 無限の自然数を生み出す
nats :: [Integer]
nats = 0 : map (+1) nats

-- フィボナッチ数を生み出す
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- 木構造
data Tree a = Leaf { element :: a }
            | Fork { element :: a
                    , left   :: Tree a
                    , right  :: Tree a
                    } deriving Show

-- 無限木
dtree :: Tree Integer
dtree = dtree' 0 where
    dtree' depth = Fork { element = depth
                        , left = dtree' (depth + 1)
                        , right = dtree' (depth + 1)
                        }

------
goodSumWithNext :: Num c => [c] -> [c]
goodSumWithNext xs = zipWith (+) xs (tail xs)

badSumWithNext :: [Integer] -> [Integer]
badSumWithNext [] = []
badSumWithNext (a:b:xs) = a + b : badSumWithNext (b:xs)

appliedHead :: (t -> [a]) -> t -> Maybe a
appliedHead f x = case f x of
                    []    -> Nothing
                    a : _ -> Just a

mean' :: [Double] -> Double
mean' xs = let (res, len) = foldl(\(m, n) x -> (m + x / len, n + 1)) (0, 0) xs in res

-- 遅延評価しない遅いたらい回し関数
tarai' :: Int -> Int -> Int -> Int
tarai' x y z
    | x <= y = y
    | otherwise = let z' = tarai' (z - 1) x y
        in z' `seq` tarai'
            (tarai' (x - 1) y z)
            (tarai' (y - 1) z x)
            z'
