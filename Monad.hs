module Monad where
import Control.Monad

-- クラスの例
class BoolLike a where
    fromBoolLike :: a -> Bool

instance BoolLike Int where
    fromBoolLike = (0 /=)
instance BoolLike (Maybe a) where
    fromBoolLike Nothing  = False
    fromBoolLike (Just _) = True

instance BoolLike Bool where
    fromBoolLike x = x

-- 型クラスインターフェイスのデフォルト実装
-- class Eq a where
--     (==) :: a -> a -> Bool
--     x == y = not (x /= y)
--     (/=) :: a -> a -> Bool
--     x /= y = not (x == y)

-- 型クラスを用いた型ごとの実装の注入
data A = A
data B = B

class ToInt a where
    toInt :: a -> Int

-- ToInt型クラスを使ってインスタンスAを定義
instance ToInt A where
    toInt _ = 1

-- ToInt型クラスを使ってインスタンスBを定義
instance ToInt B where
    toInt _ = 2

-- 同じ型でないと足せない関数
add :: ToInt x => x -> x -> Int
add x y = toInt x + toInt y

-- 同じ型でなくても足せる関数
add' :: (ToInt x, ToInt y) => x -> y -> Int
add' x y = toInt x + toInt y

-- Maybeモナドの例

-- 失敗するかもしれない関数
square :: Integer -> Maybe Integer
square n
    | 0 <= n    = Just (n * n)
    | otherwise = Nothing

-- 失敗するかもしれない関数2
squareRoot :: Integer -> Maybe Integer
squareRoot n
    | 0 <= n    = squareRoot' 1
    | otherwise = Nothing
    where
        squareRoot' x
            | n > x * x = squareRoot' (x + 1)
            | n < x * x = Nothing
            | otherwise = Just x

-- Maybeモナドを使わないと…
-- squareAndSquareRoot1 :: Integer -> Maybe Integer
-- squareAndSquareRoot1 n = case square n of 
--     Nothing -> Nothing
--     Just nn -> squareRoot nn
-- 
-- squareAndSquareRoot2 :: Integer -> Integer -> Maybe Integer
-- squareAndSquareRoot2 m n = case square m of
--     Nothing -> Nothing
--     Just mm -> case square n of
--         Nothing -> Nothing
--         Just nn -> squareRoot (mm * nn)

-- その組み合わせをMaybeモナドなら簡潔に書ける
squareAndSquareRoot1 :: Integer -> Maybe Integer
squareAndSquareRoot1 n = do
    nn <- square n
    squareRoot nn

squareAndSquareRoot2 :: Integer -> Integer -> Maybe Integer
squareAndSquareRoot2 m n = do
    mm <- square m
    nn <- square n
    squareRoot (mm * nn)

-- リストモナドの例
lessThan :: Integer -> [Integer]
lessThan n = [0 .. n-1]

plusMinus :: Integer -> Integer -> [Integer]
plusMinus a b = [a + b, a - b]

-- リストモナドを使わないと…
-- allPM0s :: Integer -> [Integer]
-- allPM0s n = concat (map (plusMinus 0) (lessThan n))
-- 
-- allPMs :: Integer -> Integer -> [Integer]
-- allPMs m n = concat (map (\x -> concat (map (plusMinus x) (lessThan n))) (lessThan m))

-- リストモナドなら簡潔に記述できる
allPM0s :: Integer -> [Integer]
allPM0s n = do
    x <- lessThan n
    plusMinus 0 x

allPMs :: Integer -> Integer -> [Integer]
allPMs m n = do
    x <- lessThan m
    y <- lessThan n
    plusMinus x y

-- 同じ環境を参照する計算とモナド
countOdd :: [Int] -> Int
countOdd = length . filter odd

countEven :: [Int] -> Int
countEven = length . filter even

-- モナドを使わない例
-- countAll :: [Int] -> Int
-- countAll xs = countOdd xs + countEven xs -- 文脈中に同じ変数が頻出する
-- 
-- countOdd :: ((->) [Int]) Int
-- countOdd = length . filter odd
-- 
-- countEven :: ((->) [Int]) Int
-- countEven = length . filter even
-- 
-- countAll :: ((->) [Int]) Int
-- countAll xs = countOdd xs + countEven xs

countAll :: [Int] -> Int
countAll = do
    odds <- countOdd
    evens <- countEven
    return (odds + evens)


-- do記法
-- 以下は同じ。つまり先は後の糖衣構文
-- do x <- m
--     y <- f x
--     let x = x + y
--     g z
-- 
-- m >>= (\x -> f x >>= (\y -> let z = x + y in gz))

-- do記法とモナド則
triple :: Int -> Int
triple = do
    n <- id     -- アクション1
    d <- (n+)   -- アクション2
    (d+)        -- アクション3

-- triple = id >>= (\n -> (n+) >>= (\d -> (d+)))

-- triple :: Int -> Int
-- triple = do
--     d <- tripleA
--     tripleB d
-- 
-- tripleA :: Int -> Int
-- tripleA = do
--     n <- id
--     (n+)
-- 
-- tripleB :: Int -> Int -> Int
-- tripleB d = do
--     (d+)

primes :: [Integer]
primes = f [2..] where
    f (p : ns) = p : f (filter ((/= 0) . (`mod` p)) ns)

primesl :: [Integer]
primesl = f [2..] where
    f (p : ns) = p : f [ n | n <- ns, n `mod` p /= 0 ]

filterPrimeM :: MonadPlus m => Integer -> m Integer
filterPrimeM n
    | n < 2                                  = mzero
    | and [ n `mod` x /= 0 | x <- [2..n-1] ] = return n
    | otherwise                              = mzero

searchPrime :: MonadPlus m => [Integer] -> m Integer
searchPrime = foldr (mplus . filterPrimeM) mzero
