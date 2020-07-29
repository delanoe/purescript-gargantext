module Gargantext.Components.Forest.Tree.Node.Action.Contact.Types where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Prelude (($))
import Reactix as R
import Gargantext.Types as GT
import Gargantext.Types (ID)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Prelude
import Gargantext.Sessions (Session, put_)
import Gargantext.Types as GT
import Reactix as R
import Reactix.DOM.HTML as H

import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)
import Data.Maybe (Maybe(..))
import Gargantext.Prelude (class Eq, class Read, class Show)

------------------------------------------------------------------------
data AddContactParams =
  AddContactParams { firstname :: String 
                   , lastname  :: String
                   }

derive instance eqAddContactParams :: Eq AddContactParams

derive instance genericAddContactParams :: Generic AddContactParams _

instance showAddContactParams :: Show AddContactParams where
  show = genericShow

instance decodeJsonAddContactParams :: Argonaut.DecodeJson AddContactParams where
  decodeJson = genericSumDecodeJson

instance encodeJsonAddContactParams :: Argonaut.EncodeJson AddContactParams where
  encodeJson = genericSumEncodeJson

------------------------------------------------------------------------
