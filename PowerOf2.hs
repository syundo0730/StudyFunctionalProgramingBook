module PowerOf2 where

-- | 2の冪乗型
newtype PowerOf2 = PowerOf2 Integer deriving (Eq, Show)

-- 2の冪乗に対し、それが2の何乗されたものかを得る
--
-- >>> exponentPowerOf2 (PowerOf2 1)
-- 0
-- >>> exponentPowerOf2 (PowerOf2 2)
-- 1
-- >>> exponentPowerOf2 (PowerOf2 4)
-- 2
-- >>> exponentPowerOf2 (PowerOf2 1024)
-- 10

exponentPowerOf2 :: PowerOf2 -> Integer
exponentPowerOf2 (PowerOf2 n) = exponentPowerOf2' 0 n where
    exponentPowerOf2' :: Integer -> Integer -> Integer
    exponentPowerOf2' r n
        | n == 1         = r
        | n < 1 || odd n = error (shows n " must be a power of 2.")
        | otherwise      = exponentPowerOf2' (r + 1) (n `div` 2)
