-- | Break a string into words and spaces
-- | It uses a simple algorithm of searching for word characters incrementally
-- | Punctuation is considered whitespace, so it's best used in a sentence or
-- | for highlighting purposes
module Gargantext.Text.BreakWords (BrokenWord(..), breakWords) where

import Prelude (Unit, discard, negate, otherwise, pure, ($), (-), (<<<), (==), (>>=))
import Data.Traversable (traverse_)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (length, slice) -- TODO: double check i'm the right choice
import Data.String.Regex (Regex)
import Gargantext.Utils.Regex (cloneRegex, execRegex, getRegexLastIndex)
import Gargantext.Utils.Array (push)

data BrokenWord
  = Word String
  | Space String

breakWords :: String -> Effect (Array BrokenWord)
breakWords s = loop $ break s
  where
  loop b = breakNext b >>= (h b)

  h :: Breaking -> Boolean -> Effect (Array BrokenWord)
  h b cont
    | cont = loop b
    | otherwise = pure b.results

-- Implementation
-- Returns whether to continue
breakNext :: Breaking -> Effect Boolean
breakNext b = checkStatic (lastIndex b)
  where
  checkStatic origin
    | origin == length b.source = pure false
    | otherwise = search b >>= next' origin

  next' origin Nothing = finish b origin

  next' origin (Just w) = next b origin w

next :: Breaking -> Int -> String -> Effect Boolean
next b origin word = do
  traverse_ (pushSpace b) $ preceding b origin word
  pushWord b word
  pure true

preceding :: Breaking -> Int -> String -> Maybe String
preceding b origin word = p $ (lastIndex b) - (length word)
  where
  p o
    | o == origin = Nothing
    | otherwise = slice origin o b.source

finish :: Breaking -> Int -> Effect Boolean
finish b origin = do
  let
    last = slice origin (-1) b.source
  traverse_ (pushSpace b) last
  pure false

type Breaking
  = { source :: String, wordRegex :: Regex, results :: Array BrokenWord }

-- almost `pure`
break :: String -> Breaking
break s = { source, wordRegex, results }
  where
  source = s

  wordRegex = cloneRegex _wordRegex

  results = []

search :: Breaking -> Effect (Maybe String)
search b = execRegex b.wordRegex b.source

lastIndex :: Breaking -> Int
lastIndex b = getRegexLastIndex b.wordRegex

pushResult :: Breaking -> BrokenWord -> Effect Unit
pushResult b = push b.results

pushSpace :: Breaking -> String -> Effect Unit
pushSpace b = pushResult b <<< Space

pushWord :: Breaking -> String -> Effect Unit
pushWord b = pushResult b <<< Word

foreign import _wordRegex :: Regex
