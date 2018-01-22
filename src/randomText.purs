module RandomText where

import Prelude

import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Random (RANDOM(..), randomInt)
import Data.Array (length, (!!), head, tail, take, takeEnd, drop, dropEnd)
import Data.Maybe (Maybe(Nothing, Just), fromJust)
import Data.String (toCharArray, fromCharArray)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)


--rando Ran x [] = Ran x []
--rando Ran x xs = Ran (x <> [x']) (rando xs')
--    where
--        Ran x' xs' = randomIt xs
--

remove :: forall t5. Int -> Array t5 -> Array t5
remove n [] = []
remove n xs = unsafePartial $ case n of
                   0 -> fromJust $ tail xs
                   _ -> (take n xs) <> (drop (n+1) xs)


data Ran = Ran { l :: Char, r :: Array Char}

randomIt :: forall t46. String -> Eff ( random :: RANDOM | t46 ) Ran
randomIt ar = unsafePartial $ do
    let ar' = toCharArray ar
    n    <- randomInt 0 (length ar' - 1)

    let maybeChar = (ar' !! n )
    let rest   = remove n ar'

    case maybeChar of
         Nothing    ->
           crash "it should not happen"
         Just char  ->
           pure $ Ran {l : char, r : rest}

randomText :: String -> String
randomText txt = fromCharArray ( start <> middle <> end)
        where
            txt'   = toCharArray        txt
            start  = take 2             txt'
            middle = dropEnd 2 $ drop 2 txt'
            end    = takeEnd 2          txt'

testText :: String -> String
testText txt = case (length (toCharArray txt)) >= 5 of
                    true -> randomText txt
                    _    -> txt
