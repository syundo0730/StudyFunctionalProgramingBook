{-# LANGUAGE FlexibleInstances #-}
module EscapedString
    ( EscapedString
    , fromString, toString
    ) where

import Data.List ( isPrefixOf )

-- | エスケープされている文字列
newtype EscapedString x = EscapedString { unEscapedString :: String } deriving Eq

-- | エスケープされている文字列のShowインスタンス化
instance Show (EscapedString x) where
    show = show . unEscapedString

-- | 普通の文字列をエスケープされている文字列にする
fromString :: String -> EscapedString String
fromString = EscapedString

-- | エスケープされている文字列を普通の文字列にする
toString :: EscapedString String -> String
toString = unEscapedString

-- | エスケープされている文字列のようなもの
class EscapedStringLike s

-- | 生の文字列はエスケープされていないが
-- エスケープされている文字列 のようなもの
-- エスケープのための変換として恒等変換がかかっていると考える
instance EscapedStringLike String

-- | エスケープ方法
class EscapeMethod m where
    -- | 変換
    escape :: EscapedStringLike s => EscapedString s -> EscapedString (m s)
    -- | 逆変換
    unescape :: EscapedStringLike s => EscapedString (m s) -> EscapedString s

-- | HTMLエスケープ
data HTMLEscape s

-- | エスケープされている文字列のようなもの を、
-- さらにエスケープしたのもまた エスケープされている文字列のようなもの
instance EscapedStringLike s => EscapedStringLike (HTMLEscape s)

-- | エスケープ方法 (HTMLエスケープ)
instance EscapeMethod HTMLEscape where
    escape = EscapedString . escape' . unEscapedString where
        escape' :: String -> String
        escape' str = str >>= escapeAmp >>= escapeOther where
            escapeAmp   '&' = "&amp;"
            escapeAmp     c = [c]
            escapeOther '<' = "&lt;"
            escapeOther '>' = "&gt;"
            escapeOther '"' = "&quot;"
            escapeOther c = [c]
    unescape = EscapedString . unescape' . unEscapedString where
        unescape' :: String -> String
        unescape' = foldr (\c s -> unescapePrefix (c:s)) "" where
            unescapePrefix str
                | "&quot;" `isPrefixOf` str = '"':drop 6 str
                | "&gt;"   `isPrefixOf` str = '>':drop 4 str
                | "&lt;"   `isPrefixOf` str = '<':drop 4 str
                | "&amp;"  `isPrefixOf` str = '&':drop 5 str
                | otherwise                 = str

-- | 文字列エスケープ
data StringEscape s

-- | エスケープされている文字列のようなもの を、
-- さらに文字列エスケープしたものもまた エスケープされている文字列のようなもの
instance EscapedStringLike s => EscapedStringLike (StringEscape s)

-- | エスケープ方法(文字列エスケープ)
instance EscapeMethod StringEscape where
    escape = EscapedString . show . unEscapedString
    unescape = EscapedString . read . unEscapedString
