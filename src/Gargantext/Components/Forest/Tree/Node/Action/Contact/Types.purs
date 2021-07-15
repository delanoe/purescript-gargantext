module Gargantext.Components.Forest.Tree.Node.Action.Contact.Types where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Simple.JSON as JSON

import Gargantext.Prelude (class Eq, class Show)

newtype AddContactParams =
  AddContactParams { firstname :: String, lastname :: String }
derive instance Eq AddContactParams
derive instance Generic AddContactParams _
derive instance Newtype AddContactParams _
instance Show AddContactParams where show = genericShow
derive newtype instance JSON.ReadForeign AddContactParams
derive newtype instance JSON.WriteForeign AddContactParams

