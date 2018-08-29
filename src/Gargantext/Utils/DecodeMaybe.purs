module Gargantext.Utils.DecodeMaybe where

import Prelude

import Data.Argonaut (class DecodeJson, Json, getFieldOptional)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)

foreign import isNull :: forall a. a -> Boolean

getFieldOptional' :: forall t9. DecodeJson t9 => Object Json -> String -> Either String (Maybe t9)
getFieldOptional' o s = (case _ of
    Just v -> if isNull v then Nothing else v
    Nothing -> Nothing
  ) <$> (getFieldOptional o s)

infix 7 getFieldOptional' as .?|
