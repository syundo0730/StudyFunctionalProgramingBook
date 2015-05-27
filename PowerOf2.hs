module PowerOf2 where

-- | 2の冪乗に対し、それが2の何乗されたものかを得る。
-- ただし、それ以外のときにはエラーになるので2の冪乗以外与えるべきでない
--
-- >>> exponentPowerOf2 1
-- 0
-- >>> exponentPowerOf2 2
-- 1
-- >>> exponentPowerOf2 3
-- *** Exception: 3 must be a power of 2.
-- >>> exponentPowerOf2 4
-- 2
-- >>> exponentPowerOf2 1024
-- 10
-- >>> exponentPowerOf2 (-1)
-- *** Exception: -1 must be a power of 2.

exponentPowerOf2 :: Integer -> Integer
exponentPowerOf2 = exponentPowerOf2' 0 where
    exponentPowerOf2' :: Integer -> Integer -> Integer
    exponentPowerOf2' r n
        | n == 1         = r
        | n < 1 || odd n = error (shows n " must be a power of 2.")
        | otherwise      = exponentPowerOf2' (r + 1) (n `div` 2)
