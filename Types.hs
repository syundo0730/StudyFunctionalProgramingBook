-- 数値リテラル
-- 0
-- 1
-- -1
-- 42

-- 0o644 -- 8進数リテラル
-- 0xFF -- 16進数リテラル
-- 3.14 -- 小数点数リテラル
-- 0.01
-- 1.0e-2

-- 文字リテラル
-- 'A'
-- '\''

-- 文字列リテラル
-- "Hello, World"

-- ラムダ式
-- (\x -> 2 * x) 2

-- 値コンストラクタ
-- True
-- 1 < 2

-- 変数の束縛
-- let one = 1

-- let another = one

-- 関数の型
-- t: not
-- not :: Bool -> Bool
-- t: (&&)
-- (&&) :: Bool -> Bool -> Bool
--
-- カリー化
-- let andT = (&&) True
-- :t andT
-- andT :: Bool -> Bool

-- 多相型と型変換
--
-- リスト
-- [1,2,3,4]
-- 1 : []
-- length [1,2,3,4,5]
--
-- :t length
-- length :: [a] -> Int
-- a は型変数
--
-- タプル
-- (1,2)
-- ('A', 2)
-- dd

-- *Main> :t (1 :: Int, 2 :: Double)
-- (1 :: Int, 2 :: Double) :: (Int, Double)

-- Either

-- Maybe

-- type宣言
-- type K = Double
-- type C = Double
-- let convert :: C -> K; convert = \c -> c + 237.15 :: Double

-- newtype宣言
-- newtype K = K Double
-- newtype C = C Double
-- let convert = \(C x) -> K (x + 237.5)

-- 代数データ型
-- data HTTPStatus = OK
--                  | Found
--                  | NotFound
--                  | ServiceUnavailable

-- レコード
-- data RGBA = RGBA Float Float Float Float
--
