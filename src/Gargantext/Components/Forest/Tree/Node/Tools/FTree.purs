module Gargantext.Components.Forest.Tree.Node.Tools.FTree where

import Data.Maybe (Maybe(..))
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Newtype (class Newtype)
import Gargantext.Types  as GT
import Prelude hiding (div)

-----------------------------------------------------------------------
type ID   = Int
type Name = String
-----------------------------------------------------------------------
type FTree = NTree LNode
data NTree a = NTree a (Array (NTree a))
type Tree = { tree       :: FTree
            , asyncTasks :: Array GT.AsyncTaskWithType
            }

instance ntreeFunctor :: Functor NTree where
  map f (NTree x ary) = NTree (f x) (map (map f) ary)

newtype LNode = LNode { id :: ID
                      , name :: Name
                      , nodeType :: GT.NodeType
                      , parent_id :: Maybe ID
                      }

derive instance newtypeLNode :: Newtype LNode _

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
