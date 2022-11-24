module Gargantext.Utils.Argonaut where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json)
import Data.Argonaut as Argonaut
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

-- | Provide a generic sum JSON decoding for sum types deriving Generic
genericSumDecodeJson
  :: forall a rep
   . GR.Generic a rep
  => GenericSumDecodeJsonRep rep
  => Json
  -> Either JsonDecodeError a
genericSumDecodeJson f =
  GR.to <$> genericSumDecodeJsonRep f

-- | Provide a generic sum JSON encoding for sum types deriving Generic
genericSumEncodeJson
  :: forall a rep
   . GR.Generic a rep
  => GenericSumEncodeJsonRep rep
  => a
  -> Json
genericSumEncodeJson f =
  genericSumEncodeJsonRep $ GR.from f

class GenericSumDecodeJsonRep rep where
  genericSumDecodeJsonRep :: Json -> Either JsonDecodeError rep

class GenericSumEncodeJsonRep rep where
  genericSumEncodeJsonRep :: rep -> Json

instance
  ( GenericSumDecodeJsonRep a
  , GenericSumDecodeJsonRep b
  ) => GenericSumDecodeJsonRep (GR.Sum a b) where
  genericSumDecodeJsonRep f
      = GR.Inl <$> genericSumDecodeJsonRep f
    <|> GR.Inr <$> genericSumDecodeJsonRep f

instance
  ( GenericSumDecodeJsonRep a
  , IsSymbol name
  ) => GenericSumDecodeJsonRep (GR.Constructor name a) where
  genericSumDecodeJsonRep f = do
    -- here we attempt to read the following json:
    -- { "ConstructorName": argument }
    let name = reflectSymbol (Proxy :: _ name)
    obj <- Argonaut.decodeJson f
    inner <- Argonaut.getField obj name
    argument <- genericSumDecodeJsonRep inner
    pure $ GR.Constructor argument

instance
  GenericSumDecodeJsonRep (GR.NoArguments) where
  genericSumDecodeJsonRep _ = do
    pure GR.NoArguments

instance
  ( Argonaut.DecodeJson a
  ) => GenericSumDecodeJsonRep (GR.Argument a) where
  genericSumDecodeJsonRep f = GR.Argument <$> Argonaut.decodeJson f

instance
  ( GenericSumEncodeJsonRep a
  , GenericSumEncodeJsonRep b
  ) => GenericSumEncodeJsonRep (GR.Sum a b) where
  genericSumEncodeJsonRep (GR.Inl f) = genericSumEncodeJsonRep f
  genericSumEncodeJsonRep (GR.Inr f) = genericSumEncodeJsonRep f

instance
  ( GenericSumEncodeJsonRep a
  , IsSymbol name
  ) => GenericSumEncodeJsonRep (GR.Constructor name a) where
  genericSumEncodeJsonRep (GR.Constructor inner) = do
    -- here we attempt to write the following json:
    -- { "ConstructorName": argument }
    let name = reflectSymbol (Proxy :: _ name)
    let argument = genericSumEncodeJsonRep inner
    Argonaut.jsonSingletonObject name argument

instance
  GenericSumEncodeJsonRep GR.NoArguments where
  genericSumEncodeJsonRep GR.NoArguments = Argonaut.jsonNull

instance
  ( Argonaut.EncodeJson a
  ) => GenericSumEncodeJsonRep (GR.Argument a) where
  genericSumEncodeJsonRep (GR.Argument f) = Argonaut.encodeJson f

genericEnumDecodeJson :: forall a rep
   . GR.Generic a rep
  => GenericEnumDecodeJson rep
  => Json
  -> Either JsonDecodeError a
genericEnumDecodeJson f =
  GR.to <$> genericEnumDecodeJsonRep f

-- | Generic Enum Sum Representations, with constructor names as strings
class GenericEnumDecodeJson rep where
  genericEnumDecodeJsonRep :: Json -> Either JsonDecodeError rep

instance
  ( GenericEnumDecodeJson a
  , GenericEnumDecodeJson b
  ) => GenericEnumDecodeJson (GR.Sum a b) where
  genericEnumDecodeJsonRep f
      = GR.Inl <$> genericEnumDecodeJsonRep f
    <|> GR.Inr <$> genericEnumDecodeJsonRep f

instance
  ( IsSymbol name
  ) => GenericEnumDecodeJson (GR.Constructor name GR.NoArguments) where
  genericEnumDecodeJsonRep f = do
    s <- Argonaut.decodeJson f
    if s == name
       then pure $ GR.Constructor GR.NoArguments
       else Left $ Named s $ TypeMismatch $ "Enum did not match expected string " <> name
    where
      name = reflectSymbol (Proxy :: Proxy name)

genericEnumEncodeJson :: forall a rep
   . GR.Generic a rep
  => GenericEnumEncodeJson rep
  => a
  -> Json
genericEnumEncodeJson f =
  genericEnumEncodeJsonRep $ GR.from f

-- | Generic Enum Sum Representations, with constructor names as strings
class GenericEnumEncodeJson rep where
  genericEnumEncodeJsonRep :: rep -> Json

instance
  ( GenericEnumEncodeJson a
  , GenericEnumEncodeJson b
  ) => GenericEnumEncodeJson (GR.Sum a b) where
  genericEnumEncodeJsonRep (GR.Inl x) = genericEnumEncodeJsonRep x
  genericEnumEncodeJsonRep (GR.Inr x) = genericEnumEncodeJsonRep x

instance
  ( IsSymbol name
  ) => GenericEnumEncodeJson (GR.Constructor name GR.NoArguments) where
  genericEnumEncodeJsonRep _ = Argonaut.encodeJson $ reflectSymbol (Proxy :: Proxy name)
