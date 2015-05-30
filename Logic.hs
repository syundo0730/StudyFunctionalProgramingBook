{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Logic where

import InferenceRule

-- | 正しい答え
--
-- 前提を意味する方から正しい答えを意味する型の結果を得る関数は実装できる

correctAnswer :: ( Chinpei `NotEat` Soba
                 , Kanta `NotEat` Curry
                 , Kanta `NotEat` Soba
                 )
              -> (Tonkichi `Eat` Soba
                 , Chinpei `Eat` Curry
                 , Kanta `Eat` Ramen
                 )

correctAnswer (chinpeiUnsatisfySoba, kantaHateCurry, kantaHateSoba) = (tonkichiEatSoba, chinpeiEatCurry, kantaEatRamen) where
    -- カンタはカレーとそばが嫌いなのでラーメンを食べた
    kantaEatRamen = EatRemainFood kantaHateCurry kantaHateSoba
    -- ラーメンはかんたが食べたのでチンペイはラーメンを食べていない
    chinpeiNotEatRamen = AnotherPeopleNotEat kantaEatRamen
    -- チンペイはそばでは満足できず、ラーメンも食べてないので、カレーを食べた
    chinpeiEatCurry = EatRemainFood chinpeiUnsatisfySoba chinpeiNotEatRamen
    -- カレーはチンペイが食べたので、トンキチはカレーを食べていない
    tonkichiNotEatCury = AnotherPeopleNotEat chinpeiEatCurry
    -- ラーメンはカンタが食べたので、トンキチはラーメンを食べていない
    tonkichiNotEatRamen = AnotherPeopleNotEat kantaEatRamen
    -- トンキチはカレーもラーメンも食べていないので、そばを食べた
    tonkichiEatSoba = EatRemainFood tonkichiNotEatCury tonkichiNotEatRamen
