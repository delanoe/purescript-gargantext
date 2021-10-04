module Gargantext.Components.Forest.Tree.Node.Action.Contact.Types where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Gargantext.Utils.SimpleJSON as GUSJ
import Simple.JSON as JSON

import Gargantext.Prelude (class Eq, class Show)

data AddContactParams =
    AddContactParams { firstname :: String, lastname :: String }
  | AddContactParamsAdvanced { firstname :: String, lastname :: String }
derive instance Eq AddContactParams
derive instance Generic AddContactParams _
instance Show AddContactParams where show = genericShow
instance JSON.ReadForeign AddContactParams where readImpl = GUSJ.taggedSumRep
instance JSON.WriteForeign AddContactParams where
  writeImpl (AddContactParams { firstname, lastname }) =
    JSON.writeImpl { type: "AddContactParams"
                   , values: { firstname, lastname } }
  writeImpl (AddContactParamsAdvanced { firstname, lastname }) =
    JSON.writeImpl { type: "AddContactParamsAdvanced"
                   , values: { firstname, lastname } }


