module Gargantext.Utils.DecodeMaybe where

import Prelude

import Data.Argonaut (class DecodeJson, JObject, getFieldOptional)
import Data.Either (Either)
import Data.Maybe (Maybe(..))

foreign import isNull :: forall a. a -> Boolean

getFieldOptional' :: forall a. DecodeJson a => JObject -> String -> Either String (Maybe a)
getFieldOptional' o s = (case _ of
    Just v -> if isNull v then Nothing else v
    Nothing -> Nothing
  ) <$> (getFieldOptional o s)

infix 7 getFieldOptional' as .?|
