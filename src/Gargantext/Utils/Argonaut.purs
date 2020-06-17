module Gargantext.Utils.Argonaut where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json)
import Data.Argonaut as Argonaut
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

-- | Provide a generic sum JSON decoding for sum types deriving Generic
genericSumDecodeJson
  :: forall a rep
   . GR.Generic a rep
  => GenericSumDecodeJsonRep rep
  => Json
  -> Either String a
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
  genericSumDecodeJsonRep :: Json -> Either String rep

class GenericSumEncodeJsonRep rep where
  genericSumEncodeJsonRep :: rep -> Json

instance genericSumDecodeJsonRepSum ::
  ( GenericSumDecodeJsonRep a
  , GenericSumDecodeJsonRep b
  ) => GenericSumDecodeJsonRep (GR.Sum a b) where
  genericSumDecodeJsonRep f
      = GR.Inl <$> genericSumDecodeJsonRep f
    <|> GR.Inr <$> genericSumDecodeJsonRep f

instance genericSumDecodeJsonRepConstructor ::
  ( GenericSumDecodeJsonRep a
  , IsSymbol name
  ) => GenericSumDecodeJsonRep (GR.Constructor name a) where
  genericSumDecodeJsonRep f = do
    -- here we attempt to read the following json:
    -- { "ConstructorName": argument }
    let name = reflectSymbol (SProxy :: _ name)
    obj <- Argonaut.decodeJson f
    inner <- Argonaut.getField obj name
    argument <- genericSumDecodeJsonRep inner
    pure $ GR.Constructor argument

instance genericSumDecodeJsonRepArgument ::
  ( Argonaut.DecodeJson a
  ) => GenericSumDecodeJsonRep (GR.Argument a) where
  genericSumDecodeJsonRep f = GR.Argument <$> Argonaut.decodeJson f

instance genericSumEncodeJsonRepSum ::
  ( GenericSumEncodeJsonRep a
  , GenericSumEncodeJsonRep b
  ) => GenericSumEncodeJsonRep (GR.Sum a b) where
  genericSumEncodeJsonRep (GR.Inl f) = genericSumEncodeJsonRep f
  genericSumEncodeJsonRep (GR.Inr f) = genericSumEncodeJsonRep f

instance genericSumEncodeJsonRepConstructor ::
  ( GenericSumEncodeJsonRep a
  , IsSymbol name
  ) => GenericSumEncodeJsonRep (GR.Constructor name a) where
  genericSumEncodeJsonRep (GR.Constructor inner) = do
    -- here we attempt to write the following json:
    -- { "ConstructorName": argument }
    let name = reflectSymbol (SProxy :: _ name)
    let argument = genericSumEncodeJsonRep inner
    Argonaut.jsonSingletonObject name argument

instance genericSumEncodeJsonRepArgument ::
  ( Argonaut.EncodeJson a
  ) => GenericSumEncodeJsonRep (GR.Argument a) where
  genericSumEncodeJsonRep (GR.Argument f) = Argonaut.encodeJson f

genericEnumDecodeJson :: forall a rep
   . GR.Generic a rep
  => GenericEnumDecodeJson rep
  => Json
  -> Either String a
genericEnumDecodeJson f =
  GR.to <$> genericEnumDecodeJsonRep f

-- | Generic Enum Sum Representations, with constructor names as strings
class GenericEnumDecodeJson rep where
  genericEnumDecodeJsonRep :: Json -> Either String rep

instance sumEnumDecodeJsonRep ::
  ( GenericEnumDecodeJson a
  , GenericEnumDecodeJson b
  ) => GenericEnumDecodeJson (GR.Sum a b) where
  genericEnumDecodeJsonRep f
      = GR.Inl <$> genericEnumDecodeJsonRep f
    <|> GR.Inr <$> genericEnumDecodeJsonRep f

instance constructorEnumSumRep ::
  ( IsSymbol name
  ) => GenericEnumDecodeJson (GR.Constructor name GR.NoArguments) where
  genericEnumDecodeJsonRep f = do
    s <- Argonaut.decodeJson f
    if s == name
       then pure $ GR.Constructor GR.NoArguments
       else Left $ "Enum string " <> s <> " did not match expected string " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)

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

instance sumGenericEnumEncodeJson ::
  ( GenericEnumEncodeJson a
  , GenericEnumEncodeJson b
  ) => GenericEnumEncodeJson (GR.Sum a b) where
  genericEnumEncodeJsonRep (GR.Inl x) = genericEnumEncodeJsonRep x
  genericEnumEncodeJsonRep (GR.Inr x) = genericEnumEncodeJsonRep x

instance constructorGenericEnumEncodeJson ::
  ( IsSymbol name
  ) => GenericEnumEncodeJson (GR.Constructor name GR.NoArguments) where
  genericEnumEncodeJsonRep _ = Argonaut.encodeJson $ reflectSymbol (SProxy :: SProxy name)
