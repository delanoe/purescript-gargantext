module LensTest where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens
import Effect
import Effect.Console

newtype S =
  S {
    first :: String
  , last :: String
  }
derive instance genericS :: Generic S _
instance showS :: Show S where
  show = genericShow

defaultS :: S
defaultS =
  S {
    first: "First"
  , last: "Last"
  }

newtype T =
  T {
    s :: S
  }

derive instance genericT :: Generic T _
instance showT :: Show T where
  show = genericShow

defaultT :: T
defaultT =
  T {
    s: defaultS
  }

_s :: Lens' T S
_s = lens getter setter
  where
    getter (T {s}) = s
    setter (T t) s = T $ t { s = s }

_last :: Lens' S String
_last = lens getter setter
  where
    getter (S {last}) = last
    setter (S s) l = S $ s { last = l }


func :: T -> Lens' T String -> Effect Unit
func t l = do
  logShow $ view l t
  logShow $ set l "last" t

-- funcT :: T -> Effect Unit
-- funcT t = func t (_ s <<< _last)
