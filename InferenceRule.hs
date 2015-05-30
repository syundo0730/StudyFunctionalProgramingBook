{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyCase #-}

-- 推論規則
module InferenceRule
    ( People(..)
    , Food(..)
    , Eat(..)
    , NotEat(..)
    ) where

data (:==:) :: k -> k -> * where -- 型の等価性
    Refl :: a :==: a -- 反射率

data Bottom -- 矛盾

type Not p = p -> Bottom

type a :/=: b = Not (a :==: b) -- 型が等価でない

class (a :: k) `Neq` (b :: k) where
    neq :: a :/=: b
    neq x = case x of {}

-- 人物
data People = Tonkichi -- トンキチ
            | Chinpei  -- チンペイ
            | Kanta    -- カンタ
              deriving (Eq, Show)

class IsPeople (people :: People)
instance IsPeople Tonkichi
instance IsPeople Chinpei
instance IsPeople Kanta

instance Tonkichi `Neq` Chinpei
instance Tonkichi `Neq` Kanta
instance Chinpei `Neq` Kanta
instance Chinpei `Neq` Tonkichi
instance Kanta `Neq` Tonkichi
instance Kanta `Neq` Chinpei

-- 食べもの
data Food = Curry -- カレー
          | Soba  -- そば
          | Ramen -- ラーメン
            deriving (Eq, Show)

class IsFood (food :: Food)
instance IsFood Curry
instance IsFood Soba
instance IsFood Ramen

instance Curry `Neq` Soba
instance Curry `Neq` Ramen
instance Soba `Neq` Ramen
instance Soba `Neq` Curry
instance Ramen `Neq` Curry
instance Ramen `Neq` Soba

-- 「誰かが何を食べた」という型
data Eat :: People -> Food -> * where
    -- 推論規則：ある人物について、食べ物１と食べ物２を食べていなければ、食べ物３を食べたはずである
    EatRemainFood   :: ( IsPeople p -- p は人物
                       , IsFood f1 -- f1 は食べ物
                       , IsFood f2 -- f2 は食べ物
                       , IsFood f3 -- f3 は食べ物
                       , f1 `Neq` f2 -- f1 と f2 は別の食べ物
                       , f2 `Neq` f3 -- f2 と f3 は別の食べ物
                       , f3 `Neq` f1 -- f3 と f1 は別の食べ物
                       ) =>
                       p `NotEat` f1 -- p は f1 を食べていない
                    -> p `NotEat` f2 -- p は f2 を食べていない
                    -> p `Eat` f3 -- ならば p は f3 を食べたはずだ
    -- 推論規則：ある食べ物について、人物１と人物２が食べていなければ、人物３が食べたはずである
    RemainPeopleEat :: ( IsPeople p1 -- p は人物
                       , IsPeople p2 -- p2 は人物
                       , IsPeople p3 -- p3 は人物
                       , IsFood f -- f は食べ物
                       , p1 `Neq` p2 -- p1 と p2 は別の人物
                       , p2 `Neq` p3 -- p2 と p3 は別の人物
                       , p3 `Neq` p1 -- p3 と p1 は別の人物
                       ) =>
                       p1 `NotEat` f -- p1 は f を食べていない
                    -> p2 `NotEat` f -- p2 は f を食べていない
                    -> p3 `Eat` f -- ならば p3 は f を食べたはずだ

-- 「誰かが何かを食べていない」という型
data NotEat :: People -> Food -> * where
    -- 推論規則：ある人物について、食べ物１を食べていれば、他のものは食べていない
    NotEatAnotherFood   :: ( IsPeople p -- p は人物
                           , IsFood f1 -- f1 は食べ物
                           , IsFood f2 -- f2 は食べ物
                           , f1 `Neq` f2 -- f1 と f2 は別の食べ物
                           ) =>
                           p `Eat` f1 -- p は f1 を食べた
                        -> p `NotEat` f2 -- ならば p は f2 を食べていない
    -- 推論規則：ある食べ物について、ある人物が食べていれば、他の人物は食べていない
    AnotherPeopleNotEat :: ( IsPeople p1 -- p1 は人物
                           , IsPeople p2 -- p2 は人物
                           , IsFood f -- f は食べ物
                           , p1 `Neq` p2 -- p1 と p2 は別の人物
                           ) =>
                           p1 `Eat` f -- p1 は f を食べた
                        -> p2 `NotEat` f -- ならば、p2 は f を食べていないはず
