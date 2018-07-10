{-|
Module      : RandomText
Description : Contextual randomized text
Copyright   : (c) CNRS / Alexandre Delanoe, 2017-present
License     : AGPL + CECILL v3
Maintainer  : alexandre.delanoe@iscpif.fr
Stability   : experimental
Portability : POSIX

How semantic emerge from contextualized randomness can be experimented
with these simple functions;

randomSentences: randomizes sentences in a paragraph.
randomWords    : randomizes words     in a sentence.
randomChars    : randomizes chars     in a word.

TODO: add some tests as examples.
-}

module Gargantext.RandomText where

import Prelude

import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Random (RANDOM(..), randomInt)

import Data.Maybe (Maybe(Nothing, Just), fromJust)
import Data.Array ( length, (!!), filter, foldl
                  , head, tail
                  , take, takeEnd
                  , drop, dropEnd
                  )
import Data.String ( toCharArray, fromCharArray
                   , split, Pattern(..)
                   )
import Data.Tuple.Nested ((/\))

import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------------
randomSentences :: forall a. String -> Eff ( random :: RANDOM | a ) String
randomSentences ss = case (length (sentences ss)) >= 5 of
                    true -> foldl (\a b -> a <> "." <> b) "" <$> randomPart (sentences ss)
                    _    -> pure ss


randomWords :: forall a. String -> Eff ( random :: RANDOM | a ) String
randomWords ws = case (length (words ws)) >= 5 of
                    true -> foldl (\a b -> a <> " " <> b) "" <$> randomPart (words ws)
                    _    -> pure ws

randomChars :: forall a. String -> Eff ( random :: RANDOM | a ) String
randomChars word = case (length (toCharArray word)) >= 5 of
                    true -> fromCharArray <$> randomPart (toCharArray word)
                    _    -> pure word

-------------------------------------------------------------------
words :: String -> Array String
words sentence = filter ((/=) "") $ split (Pattern " ") sentence

sentences :: String -> Array String
sentences paragraph = filter ((/=) "") $ split (Pattern ".") paragraph
-------------------------------------------------------------------


data RandomWheel a = RandomWheel { before :: Array a
                                 , during :: a
                                 , after  :: Array a
                                 }

randomPart :: forall a b. Array b -> Eff ( random :: RANDOM | a ) (Array b)
randomPart array = randomArrayPoly middle >>= \(middle') -> pure ( start <> middle' <> end)
        where
            start   = take    2          array
            middle  = dropEnd 2 $ drop 2 array
            end     = takeEnd 2          array


randomArrayPoly :: forall a b. Array a -> Eff ( random :: RANDOM | b ) (Array a)
randomArrayPoly wheel = case head wheel of
                         Nothing -> pure []
                         Just wheel' -> randomWheel (RandomWheel { before:wheel, during:wheel', after:[]}) 
                                     >>= \(RandomWheel rand) -> (pure rand.after)

randomWheel :: forall a b. RandomWheel b -> Eff ( random :: RANDOM | a ) (RandomWheel b)
randomWheel (RandomWheel {before:[], during:d, after:a}) = 
    pure   (RandomWheel {before:[], during:d, after:a})

randomWheel (RandomWheel {before:b, during:d, after:a}) = do
    RandomWheel {before:b', during:d', after:a'} <- randomArray b
    randomWheel $ RandomWheel {before:b', during:d', after:(a <> [d'])}


randomArray :: forall a b. Array b -> Eff ( random :: RANDOM | a ) (RandomWheel b) 
randomArray array = unsafePartial $ do
    n    <- randomInt 0 (length array - 1)

    let maybeDuring = (array !! n)

    case maybeDuring of
         Nothing    ->
            crash "[ERROR] It should never happen."
         Just during  ->
            pure $ RandomWheel { before : remove n array
                               , during : during
                               , after  : []
                               }


remove :: forall a. Int -> Array a -> Array a
remove n [] = []
remove n xs = unsafePartial $ case n of
                   0 -> fromJust $ tail xs
                   _ -> (take n xs) <> (drop (n+1) xs)

-------------------------------------------------------------------
