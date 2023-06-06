module Gargantext.Utils.String where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Int as Int
import Data.Number.Format as DNF

foreign import _btoa :: Fn1 String String
foreign import _specialCharNormalize :: Fn1 String String

btoa :: String -> String
btoa = runFn1 _btoa

intToString :: Int -> String
intToString = DNF.toString <<< Int.toNumber

-- | https://ricardometring.com/javascript-replace-special-characters
specialCharNormalize :: String -> String
specialCharNormalize = runFn1 _specialCharNormalize
