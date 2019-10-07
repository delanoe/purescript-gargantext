-- | Utilities for working with regexes in a naughty mutable manner
module Gargantext.Utils.Regex where

import Effect (Effect)
import Prelude ((<$>))
import Data.Maybe (Maybe)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Nullable (Nullable, toMaybe)
import Data.String.Regex (Regex)

foreign import _cloneRegex :: Fn1 Regex Regex

foreign import _getRegexLastIndex :: Fn1 Regex Int

foreign import _execRegex :: EffectFn2 Regex String (Nullable String)

cloneRegex :: Regex -> Regex
cloneRegex = runFn1 _cloneRegex

getRegexLastIndex :: Regex -> Int
getRegexLastIndex = runFn1 _getRegexLastIndex

execRegex :: Regex -> String -> Effect (Maybe String)
execRegex r s = toMaybe <$> runEffectFn2 _execRegex r s
