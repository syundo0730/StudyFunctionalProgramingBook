-- 非負整数値の桁数
digits :: Int -> Int
digits = length . show

-- 数値の2乗
square :: Num a => a -> a
square = (^2)

-- square' :: Num a => a -> a
-- aquare' x = x ^ 2

ultimate :: Int -> String -- 関数ultimateを定義する
ultimate 42 = "人生、宇宙、すべての答え"
ultimate n = show n

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a : as) = Just a

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

caseOfFirstLetter :: String -> String
caseOfFirstLetter "" = "empty"
caseOfFirstLetter (x:xs)
 | 'a' <= x && x <= 'z' = "lower"
 | 'A' <= x && x <= 'Z' = "upper"
 | otherwise            = "other"

caseOfFirstLetter2 :: String -> String
caseOfFirstLetter2 str =
    case str of
        ""     -> "empty"
        (x:xs) -> if 'a' <= x && x <= 'z'
            then "lower"
            else if 'A' <= x && x <= 'Z'
                then "upper"
                else "other"

caseOfFirstLetter3 :: String -> String
caseOfFirstLetter3 str =
    case str of
        ""     -> "empty"
        (x:xs) | 'a' <= x && x <= 'z' -> "lower"
               | 'A' <= x && x <= 'Z' -> "upper"
               | otherwise            -> "other"

caseOfFirstLetter4 :: String -> String
caseOfFirstLetter4 "" = "empty"
caseOfFirstLetter4 (x:xs)
    | inRange 'a' 'z' = "lower"
    | inRange 'A' 'Z' = "upper"
    | otherwise       = "other"
    where
        inRange lower upper = lower <= x && x <= upper

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ []         = []
take' n (x:xs)     = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n xs | n <= 0 = xs
drop' _ []         = []
drop' n (_:xs)     = drop' (n-1) xs

ins :: Ord a => a -> [a] -> [a]
ins e [] = [e]
ins e s@(x:xs)
    | e < x     = e : s
    | otherwise = x : ins e xs

insSort :: Ord a => [a] -> [a]
insSort [] = []
insSort (x:xs) = ins x (insSort xs)

inc :: Int -> Int
inc = (+) 1

each :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
each f g (x, y) = (f x, g y)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []     = []
filter' p (x:xs) = if p x then x:(filter' p xs) else filter' p xs
-- p x が真ならばxとその後フィルターするものと結合し、そうでなければフィルターするものだけを残す

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _    []     _   = []
zipWith' _    _     []   = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs 

zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith' (,)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ e [] = e
foldr' f e (x:xs) = f x (foldr' f e xs)

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ e []     = [e]
scanr' f e (x:xs) = f x (head rs) : rs where
    rs = scanr' f e xs

segments :: [a] -> [[a]]
segments = foldr (++) [] . scanr (\a b -> [a] : map (a:) b) []
