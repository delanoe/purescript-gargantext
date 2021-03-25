module Gargantext.Components.Forest.Tree.Node.Tools.FTree where

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

import Gargantext.Prelude

import Gargantext.Types  as GT

-----------------------------------------------------------------------
type ID   = Int
type Name = String
-----------------------------------------------------------------------
type FTree = NTree LNode
data NTree a = NTree a (Array (NTree a))
derive instance genericNTree :: Generic (NTree a) _
instance eqNTree :: Eq a => Eq (NTree a) where
  eq (NTree a1 as1) (NTree a2 as2) = (eq a1 a2) && (eq as1 as2)
type Tree = { tree       :: FTree
            , tasks     :: Array GT.AsyncTaskWithType
            }

fTreeID :: FTree -> ID
fTreeID (NTree (LNode { id }) _) = id

instance ntreeFunctor :: Functor NTree where
  map f (NTree x ary) = NTree (f x) (map (map f) ary)

newtype LNode =
  LNode
  { id        :: ID
  , name      :: Name
  , nodeType  :: GT.NodeType
  , parent_id :: Maybe ID
  }
derive instance newtypeLNode :: Newtype LNode _
derive instance genericLNode :: Generic LNode _
instance eqLNode :: Eq LNode where
  eq = genericEq
instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj  <- decodeJson json
    id_  <- obj .: "id"
    name <- obj .: "name"
    nodeType <- obj .: "type"
    pure $ LNode { id : id_
                 , name
                 , nodeType
                 , parent_id : Nothing
                 }

instance decodeJsonFTree :: DecodeJson (NTree LNode) where
  decodeJson json = do
    obj    <- decodeJson json
    node   <- obj .: "node"
    nodes  <- obj .: "children"
    node'  <- decodeJson node
    nodes' <- decodeJson nodes
    let (LNode {id}) = node'
    pure $ NTree node' (map (addParent id) nodes')

addParent :: ID -> NTree LNode -> NTree LNode
addParent id (NTree (LNode p@{id:id'}) ary)=
  NTree (LNode (p {parent_id=Just id}))
        (map (addParent id') ary)
