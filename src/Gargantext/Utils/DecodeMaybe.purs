module Gargantext.Utils.DecodeMaybe where

import Prelude

import Data.Argonaut (class DecodeJson, Json, getFieldOptional)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object (Object)

foreign import isNull :: forall a. a -> Boolean

getFieldOptional' :: forall a. DecodeJson a => 
  Object Json -> String -> Either JsonDecodeError (Maybe a)
getFieldOptional' o s = (case _ of
    Just v -> if isNull v then Nothing else v
    Nothing -> Nothing
  ) <$> (getFieldOptional o s)

infix 7 getFieldOptional' as .?|

getFieldOptionalAsMempty :: forall a. DecodeJson a => 
  Monoid a => Object Json -> String -> Either JsonDecodeError a
getFieldOptionalAsMempty o s = 
  fromMaybe mempty <$> (getFieldOptional' o s)

infix 7 getFieldOptionalAsMempty as .|
