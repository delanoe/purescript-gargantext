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


data RanR = RanR { l :: Array Char, r :: Array Char}

instance showRanR :: Show RanR where
    show (RanR {l:l', r:r'}) = show $ (show l') /\ (show r')

rando (RanR {l:x,r:[]}) = pure $ RanR {l:x,r:[]}
rando (RanR {l:x,r:xs}) = do
    Ran {l:x',r:xs'} <- randomIt xs
    rando (RanR {l:(x <> [x']), r: xs'})


remove :: forall t5. Int -> Array t5 -> Array t5
remove n [] = []
remove n xs = unsafePartial $ case n of
                   0 -> fromJust $ tail xs
                   _ -> (take n xs) <> (drop (n+1) xs)


data Ran = Ran { l :: Char, r :: Array Char}

instance showRan :: Show Ran where
    show (Ran {l:l', r:r'}) = show $ (show l') /\ (show r')

randomIt :: forall t46. Array Char -> Eff ( random :: RANDOM | t46 ) Ran
randomIt ar = unsafePartial $ do
    -- let ar' = toCharArray ar
    n    <- randomInt 0 (length ar - 1)

    let maybeChar = (ar !! n )
    let rest   = remove n ar

    case maybeChar of
         Nothing    -> 
            crash "it should not happen"
         Just char  -> 
            pure $ Ran {l : char, r : rest}

randomize :: forall t98. String -> Eff ( random :: RANDOM | t98) String
randomize string = do 
    RanR rr <- rando (RanR {l:[], r:(toCharArray string)})
    pure $ fromCharArray (rr.l)


randomize' :: forall t98. (Array Char) -> Eff ( random :: RANDOM | t98) (Array Char)
randomize' string = do 
    RanR rr <- rando (RanR {l:[], r:string})
    pure rr.l


randomText :: forall t114. String -> Eff( random :: RANDOM| t114) String
randomText txt = randomize' middle >>= \middle' -> pure $ fromCharArray ( start <> middle' <> end)
        where
            txt'    = toCharArray        txt
            start   = take 2             txt'
            middle  = dropEnd 2 $ drop 2 txt'
            end     = takeEnd 2          txt'

testText :: forall t114. String -> Eff( random :: RANDOM| t114) String
testText txt = case (length (toCharArray txt)) >= 5 of
                    true -> randomText txt
                    _    -> pure txt
