module Gargantext.Utils.SimpleJSON where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (withExcept)
import Data.Generic.Rep as GR
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign (Foreign, ForeignError(..), fail)
import Foreign as Foreign
import Foreign.Object as FO
import Simple.JSON as JSON
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

taggedSumRep :: forall a rep
   . GR.Generic a rep
  => GenericTaggedSumRep rep
  => Foreign
  -> Foreign.F a
taggedSumRep f = GR.to <$> genericTaggedSumRep f

-- | Generic Tagged Sum Representations, with "type" as key and rest
-- of key/values representing the object.  Note that this is slightly
-- difrerent than what Simple.JSON generics provides as it wrapes the
-- tag in "type" and object under "value" key.
class GenericTaggedSumRep rep where
  genericTaggedSumRep :: Foreign -> Foreign.F rep

instance ( GenericTaggedSumRep a
         , GenericTaggedSumRep b
         ) => GenericTaggedSumRep (GR.Sum a b) where
  genericTaggedSumRep f
      = GR.Inl <$> genericTaggedSumRep f
    <|> GR.Inr <$> genericTaggedSumRep f

instance ( GenericTaggedSumRep a
         , IsSymbol name
         ) => GenericTaggedSumRep (GR.Constructor name a) where
  genericTaggedSumRep f = do
    -- r :: { "type" :: String } <- JSON.read' f
    -- if r."type" == name 
    --   then withExcept (map $ ErrorAtProperty name) $ GR.Constructor <$> genericTaggedSumRep r
    --   else fail $ ForeignError $ "Wrong type tag " <> r."type" <> " where " <> name <> " was expected."
    r :: FO.Object Foreign <- JSON.read' f
    case FO.pop "type" r of
      Nothing -> fail $ ForeignError $ "Key 'type' not found."
      Just (Tuple name' obj) -> do
        n' <- Foreign.readString name'
        if n' == name
         then withExcept (map $ ErrorAtProperty name) $ GR.Constructor <$> (genericTaggedSumRep $ Foreign.unsafeToForeign obj)
         else fail $ ForeignError $ "Wrong type tag " <> n' <> " where " <> name <> " was expected."
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP

instance ( JSON.ReadForeign a
         ) => GenericTaggedSumRep (GR.Argument a) where
  genericTaggedSumRep f = GR.Argument <$> JSON.readImpl f





