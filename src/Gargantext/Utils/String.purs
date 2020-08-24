module Gargantext.Utils.String where

import Data.Function.Uncurried (Fn1, runFn1)

foreign import _btoa :: Fn1 String String

btoa :: String -> String
btoa = runFn1 _btoa
