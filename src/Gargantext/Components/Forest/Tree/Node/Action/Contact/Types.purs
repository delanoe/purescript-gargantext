module Gargantext.Components.Forest.Tree.Node.Action.Contact.Types where

import Gargantext.Prelude (class Eq, class Show)
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data AddContactParams =
  AddContactParams { firstname :: String, lastname :: String }

derive instance eqAddContactParams :: Eq AddContactParams

derive instance genericAddContactParams :: Generic AddContactParams _

instance showAddContactParams :: Show AddContactParams where
  show = genericShow

instance decodeJsonAddContactParams :: DecodeJson AddContactParams where
  decodeJson = genericSumDecodeJson

instance encodeJsonAddContactParams :: EncodeJson AddContactParams where
  encodeJson = genericSumEncodeJson
