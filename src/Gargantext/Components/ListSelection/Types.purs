module Gargantext.Components.ListSelection.Types where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Gargantext.Types (ID, ListId, NodeType)
import Simple.JSON as JSON

data Selection = MyListsFirst | OtherListsFirst | SelectedLists (Array ListId)
derive instance Generic Selection _
instance Show Selection where
  show MyListsFirst = "My lists first"
  show OtherListsFirst = "Other lists first"
  show (SelectedLists _) = "Selected lists"
instance Eq Selection where eq = genericEq
instance Read Selection where
  read "My lists first" = Just MyListsFirst
  read "Other lists first" = Just OtherListsFirst
  read "Selected lists" = Just $ SelectedLists []
  read _ = Nothing

selectedListIds :: Selection -> Array ListId
selectedListIds (SelectedLists ids) = ids
selectedListIds _                   = []


----------------------


-- A simplified data structure (we don't want the full-blown (NodePoly
-- a), we care only about Corpus and NodeList node types, with id,
-- name and that's all).
newtype NodeSimple =
  NodeSimple { id       :: ID
             , name     :: String
             , nodeType :: NodeType }
derive instance Generic NodeSimple _
derive instance Newtype NodeSimple _
derive instance Eq NodeSimple
instance JSON.ReadForeign NodeSimple where
  readImpl f = do
    { node } :: { node :: { id   :: ID
                          , name :: String
                          , type :: NodeType } } <- JSON.read' f
    pure $ NodeSimple { id: node.id
                      , name: node.name
                      , nodeType: node.type }
